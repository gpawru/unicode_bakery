use std::collections::HashMap;

use unicode_data::codepoint::Codepoint;
use unicode_data::hangul::is_composable_hangul_jamo;
use unicode_data::{COMBINES_BACKWARDS, COMPOSITION_EXCLUSIONS};
use unicode_data::{NFC, NFKC, QC_NFC, QC_NFKC};

use super::compositions::compositions;
use super::compositions::CompositionInfo;
use crate::blocking_checks;
use crate::common::starters_map;
use crate::encode::{EncodeCodepoint, EncodedCodepoint};
use crate::expansion;
use crate::stats::EncodeCodepointStats;

/// стартер без декомпозиции
pub const MARKER_STARTER: u64 = 0b_000;
/// пара стартер + нестартер
pub const MARKER_PAIR: u64 = 0b_001;
/// стартер-синглтон
pub const MARKER_SINGLETON: u64 = 0b_010;
/// нестартер без декомпозиции
pub const MARKER_NONSTARTER: u64 = 0b_011;

// 4 маркера объединены в один кейс: 0 или несколько стартеров + 0 или несколько нестартеров
//  - первый стартер не комбинируется с предыдущими кодпоинтами
//  - информация о комбинировании записана для последнего стартера последовательности

/// стартер и нестартеры
pub const MARKER_EXPANSION_STARTER_NONSTARTERS: u64 = 0b_100;
/// последовательность стартеров
pub const MARKER_EXPANSION_STARTERS: u64 = 0b_0100;
/// два стартера + нестартер
pub const MARKER_EXPANSION_TWO_STARTERS_NONSTARTER: u64 = 0b_100;
/// исключения - стартеры, которые декомпозируются в нестартеры
pub const MARKER_NONSTARTERS_EXCLUSION: u64 = 0b_100;

/// исключения - стартеры, которые комбинируются с предыдущими кодпоинтами
pub const MARKER_COMBINES_BACKWARDS: u64 = 0b_101;

pub struct EncodeComposition64
{
    pub is_canonical: bool,
    /// таблица композиций кодпоинтов в сжатом виде
    pub composition_table: Vec<u64>,
    /// индексы - комбинирование кодпоинтов с идущими следом кодпоинтами
    pub composition_index: HashMap<u32, CompositionInfo>,
    /// индексы - комбинирование с предыдущим кодпоинтом
    pub composition_index_backwards: HashMap<u32, CompositionInfo>,
}

impl EncodeComposition64
{
    #[allow(dead_code)]
    pub fn new(is_canonical: bool) -> Self
    {
        let (composition_table, composition_index, composition_index_backwards) = compositions();

        Self {
            is_canonical,
            composition_table,
            composition_index,
            composition_index_backwards,
        }
    }

    /// информация о записи в таблице комбинирования для кодпоинта, записанная как u16
    /// 0, если кодпоинт не комбинируется с идущими за ним
    pub fn combination_info<T: Into<u64>>(&self, code: T) -> u16
    {
        match self.composition_index.get(&(code.into() as u32)) {
            Some(info) => info.baked(),
            None => 0,
        }
    }

    /// информация о комбинировании с предыдущим кодпоинтом
    pub fn combination_backwards_info<T: Into<u64>>(&self, code: T) -> u16
    {
        match self.composition_index_backwards.get(&(code.into() as u32)) {
            Some(info) => info.baked(),
            None => 0,
        }
    }
}

impl EncodeCodepoint<u64, u32> for EncodeComposition64
{
    fn encode(
        &self,
        codepoint: &Codepoint,
        exp_position: usize,
        stats: &mut EncodeCodepointStats,
    ) -> Option<EncodedCodepoint<u64, u32>>
    {
        let (precomposition, mut quick_check) = match self.is_canonical {
            true => (&NFC[&codepoint.code], QC_NFC[codepoint.code as usize]),
            false => (&NFKC[&codepoint.code], QC_NFKC[codepoint.code as usize]),
        };

        if is_composable_hangul_jamo(codepoint.code) {
            quick_check = 'M';
        }

        let variants = &[
            starter,
            singleton,
            nonstarter,
            starter_nonstarter_pair,
            starter_nonstarters_sequence,
            starters_sequence,
            two_starters_nonstarter,
            starters_to_nonstarters,
            combines_backwards_case,
        ];

        let value = variants.iter().find_map(|f| {
            f(
                &self,
                codepoint,
                precomposition,
                quick_check,
                exp_position,
                stats,
            )
        });

        if value.is_none() {
            panic!(
                "не определили тип хранения кодпоинта: U+{:04X} - {} ({})",
                codepoint.code,
                codepoint.name,
                codepoint.ccc.u8(),
            );
        };

        let value = value.unwrap();

        match value.value == 0 {
            true => None,
            false => Some(value),
        }
    }

    fn default(&self) -> &EncodedCodepoint<u64, u32>
    {
        &EncodedCodepoint {
            value: MARKER_STARTER,
            extra: None,
        }
    }
}

// все битовые представения u64 представлены как little endian
//
// q - маркер быстрой проверки
//   0 - 'Y'/'M' - не участвует или может участвовать в композиции,
//   1 - 'N' - участвует в композиции или кодпоинт хангыль, который может быть скомбинирован с предыдущим
//
// mm - маркер типа записи
//
// ii.. - (16 бит) - информация о комбинациях стартера со следующим кодпоинтом (в большинстве случаев),
//        или с предыдущим кодпоинтом (когда стартер может быть скомбинирован с предыдущим)
//
// ccс. - (8 бит) - ССС нестартера в записи
//
// pp.. - (16 бит) - индекс последовательности кодпоинтов в таблице expansions
// nn.. - (8 бит) - количество кодпоинтов в последовательности в таблице expansions
// tttt - (4 бита) - количество нестартеров в последовательности в таблице expansions

macro_rules! assert_qc {
    ($qc: expr, $($c:expr),+) => {
        assert!([$($c),+].contains(&$qc));
    };
}

macro_rules! assert_not_combines_backwards {
    ($code: expr) => {
        assert!(!combines_backwards($code as u32));
    };
}

macro_rules! encoded {
    ($marker:expr, $qc:expr, $value:expr, $expansion:expr; $stats:expr, $codepoint:expr, $description:expr) => {{
        let description = match $description.is_empty() {
            true => format!("{}", $codepoint.name),
            false => format!("{} : {}", $codepoint.name, $description),
        };

        $stats.inc($codepoint.code, description);

        let qc = match $qc {
            'Y' => 0,
            'N' => 1,
            'M' => 1,
            _ => unreachable!(),
        };

        Some(EncodedCodepoint {
            value: $marker << 1 | qc | ($value as u64),
            extra: $expansion,
        })
    }};
    ($marker:expr, $qc:expr, $value:expr, $expansion:expr; $stats:expr, $codepoint:expr) => {{
        encoded!($marker, $qc, $value, $expansion; $stats, $codepoint, &"")
    }};
}

macro_rules! expansion_entry {
    ($marker: expr, $fast: expr, $combining: expr, $precomposition: expr, $e_index: expr; $stats:expr, $codepoint:expr) => {{
        let e_index = $e_index as u64;
        let e_len = $precomposition.len() as u64;

        assert!((e_index < 0xFFFF) && (e_len < 0xFF));

        if !$precomposition.is_empty() {
            let c0 = &$precomposition[0];

            assert!(!is_composable_hangul_jamo(c0.code));

            if c0.is_starter() {
                assert_not_combines_backwards!(c0.code);
            }
        }

        // количество нестартеров (располагаются только в конце)
        let n_len = $precomposition.iter().rev().take_while(|&c| c.is_nonstarter()).count() as u64;
        assert!(n_len <= 0b_1111);

        let (expansion, description) = expansion!($precomposition);

        encoded!(
            $marker,
            $fast,
            ($combining << 32) | (e_index << 16) | (e_len << 8) | (n_len << 4),
            Some(expansion); $stats, $codepoint, description
        )
    }};
}

/// стартер, нет декомпозиции, не комбинируется с предыдущим
///
/// qmmm ____  ____ ____    iiii iiii  iiii iiii    ____ ____  ____ ____    ____ ____  ____ ____
///
fn starter(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("стартер (комбинируемый/хангыль)");

    blocking_checks!(
        codepoint.is_nonstarter(),
        !precomposition.is_empty(),
        combines_backwards(codepoint.code)
    );

    // Y - если за стартером следует стартер, то кодпоинт не изменится
    // M - чамо хангыль V / T - нужно попробовать скомбинировать с предыдущим кодпоинтом
    assert_qc!(qc, 'Y', 'M');

    let combining = encoder.combination_info(codepoint.code) as u64;

    match (combining == 0) && (qc == 'Y') {
        true => Some(EncodedCodepoint {
            value: MARKER_STARTER,
            extra: None,
        }),
        false => encoded!(MARKER_STARTER, qc, combining << 16, None; se, codepoint),
    }
}

/// синглтон
///
/// qmmm ____  ____ ____    iiii iiii  iiii iiii    xxxx xxxx  xxxx xxxx    xx__ ____  ____ ____
///
fn singleton(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("синглтон");

    blocking_checks!(starters_map(precomposition) != "s");

    // N - синглтон - это всегда замена одного кодпоинта другим
    assert_qc!(qc, 'N');

    let c0 = precomposition[0].code as u64;
    let combining = encoder.combination_info(c0) as u64;

    assert_ne!(codepoint.code as u64, c0);
    assert_not_combines_backwards!(c0);

    encoded!(MARKER_SINGLETON, qc, (combining << 16) | (c0 << 32), None; se, codepoint)
}

/// нестартер без декомпозиции
///
/// qmmm ____  cccc cccc    ____ ____  ____ ____    ____ ____  ____ ____    ____ ____  ____ ____
///
fn nonstarter(
    _encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("нестартер");

    blocking_checks!(!precomposition.is_empty(), codepoint.is_starter());

    // M, Y - нестартеры делятся на те, которые могут быть скомбинированы, и на те, которые не могут.
    // тем не менее, будем рассматривать любой нестартер как N, т.к. мы в данном случае не используем
    // непосредственно алгоритм валидации NF(K)C, а его часть
    assert_qc!(qc, 'Y', 'M');

    let ccc = codepoint.ccc.compressed() as u64;

    encoded!(MARKER_NONSTARTER, 'N', (ccc << 8), None; se, codepoint)
}

/// пара стартер + нестартер
///
/// qmmm cccc  cccc xxxx    xxxx xxxx  xxxx xxyy    yyyy yyyy  yyyy yyyy    iiii iiii  iiii iiii
///
fn starter_nonstarter_pair(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("пара стартер + нестартер");

    // пара: стартер + нестартер, не является исключением композиции

    blocking_checks!(
        starters_map(precomposition) != "sn",
        is_exclusion(codepoint.code)
    );

    // Y - пара будет скомбинирована обратно, если за ней следет стартер
    // N - при комбинировании будет получен другой символ
    assert_qc!(qc, 'Y', 'N');

    let c0 = precomposition[0].code as u64;
    let c1 = precomposition[1].code as u64;
    let c1_ccc = precomposition[1].ccc.compressed() as u64;

    let combining = encoder.combination_info(c0) as u64;

    assert_not_combines_backwards!(c0);

    encoded!(MARKER_PAIR, qc, (c1_ccc << 4) | (c0 << 12) | (c1 << 30) | (combining << 48), None; se, codepoint)
}

/// стартер и последовательность нестартеров
///
/// qmmm tttt  nnnn nnnn    pppp pppp  pppp pppp    iiii iiii  iiii iiii    ____ ____  ____ ____
///
fn starter_nonstarters_sequence(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("стартер + нестартеры");

    blocking_checks!(
        precomposition.len() < 2,
        !precomposition[0].is_starter(),
        !precomposition[1 ..].iter().all(|c| c.is_nonstarter())
    );

    // Y, N - аналогично паре
    assert_qc!(qc, 'Y', 'N');

    let c0 = precomposition[0].code;
    let combining = encoder.combination_info(c0) as u64;

    expansion_entry!(MARKER_EXPANSION_STARTER_NONSTARTERS, qc, combining, &precomposition, exp_position; se, codepoint)
}

/// последовательность стартеров
///
/// qmmm 0000  nnnn nnnn    pppp pppp  pppp pppp    iiii iiii  iiii iiii    ____ ____  ____ ____
///
fn starters_sequence(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("последовательность стартеров");

    blocking_checks!(
        precomposition.len() < 2,
        !precomposition.iter().all(|c| c.is_starter())
    );

    // N - декомпозиция происходит всегда
    assert_qc!(qc, 'N');

    let clast = precomposition.last().unwrap().code;
    let combining = encoder.combination_info(clast) as u64;

    expansion_entry!(MARKER_EXPANSION_STARTERS, qc, combining, &precomposition, exp_position; se, codepoint)
}

/// стартер + стартер + нестартер
///
/// qmmm tttt  nnnn nnnn    pppp pppp  pppp pppp    iiii iiii  iiii iiii    ____ ____  ____ ____
///
fn two_starters_nonstarter(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("два стартера + нестартер");

    blocking_checks!(starters_map(precomposition) != "ssn");

    // N - только в NFKC - декомпозиция происходит всегда
    assert_qc!(qc, 'N');

    let c1 = precomposition[1].code;
    let combining = encoder.combination_info(c1) as u64;

    expansion_entry!(MARKER_EXPANSION_TWO_STARTERS_NONSTARTER, qc, combining, &precomposition, exp_position; se, codepoint)
}

/// исключение - стартеры с декомпозицией в нестартеры
///
/// qmmm tttt  nnnn nnnn    pppp pppp  pppp pppp    iiii iiii  iiii iiii    ____ ____  ____ ____
///
fn starters_to_nonstarters(
    _encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("декомпозиция в нестартеры");

    blocking_checks!(
        precomposition.is_empty(),
        !precomposition.iter().all(|c| c.is_nonstarter())
    );

    // N - т.к. кодпоинт всегда декомпозируется и не собирается обратно
    assert_qc!(qc, 'N');

    expansion_entry!(MARKER_NONSTARTERS_EXCLUSION, qc, 0, &precomposition, exp_position; se, codepoint)
}

/// исключение - стартеры, комбинируемые с предыдущим кодпоинтом
///
/// qmmm ____  ____ ____    ____ ____  ____ ____    iiii iiii  iiii iiii    ____ ____  ____ ____
///
fn combines_backwards_case(
    encoder: &EncodeComposition64,
    codepoint: &Codepoint,
    precomposition: &Vec<Codepoint>,
    qc: char,
    exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64, u32>>
{
    let se = stats.touch("комбинируется с предыдущим");

    blocking_checks!(
        codepoint.is_nonstarter(),
        !combines_backwards(codepoint.code)
    );

    assert_eq!(encoder.combination_info(codepoint.code), 0);
    assert!(precomposition.is_empty());

    // M - может быть скомбинирован с предыдущим или оставлен как есть
    assert_qc!(qc, 'M');

    let combines = encoder.combination_backwards_info(codepoint.code) as u64;

    expansion_entry!(MARKER_COMBINES_BACKWARDS, qc, combines, &precomposition, exp_position; se, codepoint)
}

/// кодпоинт может быть скомбинирован с предыдущим
fn combines_backwards<T: Into<u64>>(code: T) -> bool
{
    COMBINES_BACKWARDS.contains_key(&(code.into() as u32))
}

/// исключение композиции
fn is_exclusion<T: Into<u64>>(code: T) -> bool
{
    COMPOSITION_EXCLUSIONS.contains(&(code.into() as u32))
}
