use unicode_data::codepoint::Codepoint;
use unicode_data::hangul::{is_composable_hangul_jamo, is_syllable};
use unicode_data::{COMBINES_BACKWARDS, NFD, NFKD, UNICODE};
use unicode_data::{NFC, NFKC};

use crate::encode::{EncodeCodepoint, EncodedCodepoint};
use crate::stats::{EncodeCodepointStats, EncodeCodepointStatsBlock};
use compositions::BakedCompositions;

use qc::get_qc;

use self::composed_expansions::{EXPANSIONS_NFC, EXPANSIONS_NFKC};

pub mod composed_expansions;
pub mod compositions;
pub mod qc;

/// стартер без декомпозиции
pub const MARKER_STARTER: u8 = 0b_000;
/// исключения - стартеры, которые комбинируются с предыдущими кодпоинтами
pub const MARKER_COMBINES_BACKWARDS: u8 = 0b_001;
/// нестартер без декомпозиции
pub const MARKER_NONSTARTER: u8 = 0b_010;
/// стартер-синглтон
pub const MARKER_SINGLETON: u8 = 0b_011;
/// - стартер и нестартеры
/// - последовательность стартеров
/// - два стартера + нестартер
/// - исключения - стартеры, которые декомпозируются в нестартеры
pub const MARKER_EXPANSION: u8 = 0b_100;
/// аналогично обычным расширениям, заменяется декомпозиция в NF(K)C
pub const MARKER_EXPANSION_COMBINED_PATCH: u8 = 0b_101;
/// аналогично обычным расширениям, но в NF(K)C нет декомпозиции
pub const MARKER_EXPANSION_COMBINED_EMPTY: u8 = 0b_110;
/// - хангыль (слог или V, T чамо)
pub const MARKER_HANGUL: u8 = 0b_111;

/// максимальное значение маркера
pub const MAX_MARKER: u8 = MARKER_HANGUL;

#[derive(Clone)]
pub struct EncodeNormalization
{
    pub is_canonical: bool,
    pub compositions: BakedCompositions,
}

impl EncodeNormalization
{
    pub fn new(is_canonical: bool) -> Self
    {
        Self {
            is_canonical,
            compositions: BakedCompositions::new(),
        }
    }

    /// информация о записи в таблице комбинирования для кодпоинта, записанная как u16
    /// 0, если кодпоинт не комбинируется с идущими за ним
    pub fn combination_info<T: Into<u64>>(&self, code: T) -> u16
    {
        match self.compositions.index.get(&(code.into() as u32)) {
            Some(info) => info.baked(),
            None => 0,
        }
    }

    /// информация о комбинировании с предыдущим кодпоинтом
    pub fn combination_backwards_info<T: Into<u64>>(&self, code: T) -> u16
    {
        match self.compositions.backwards_index.get(&(code.into() as u32)) {
            Some(info) => info.baked(),
            None => 0,
        }
    }

    /// декомпозиция кодпоинта в зависимости от типа нормализации
    pub fn decomposition(&self, code: u32) -> &Vec<Codepoint>
    {
        match self.is_canonical {
            true => &NFD[&code],
            false => &NFKD[&code],
        }
    }

    /// декомпозиция (прекомпозиция) для NF(K)C
    #[allow(dead_code)]
    pub fn combining_decomposition(&self, code: u32) -> &Vec<Codepoint>
    {
        match self.is_canonical {
            true => &NFC[&code],
            false => &NFKC[&code],
        }
    }
}

impl EncodeCodepoint<u32, u32, Vec<u32>> for EncodeNormalization
{
    fn encode(
        &self,
        codepoint: &Codepoint,
        extra: &mut Vec<u32>,
        stats: &mut EncodeCodepointStats,
    ) -> Option<EncodedCodepoint<u32>>
    {
        let qc = get_qc(codepoint.code, self.is_canonical);

        let composed_expansion = match self.is_canonical {
            true => EXPANSIONS_NFC.get(codepoint.code),
            false => EXPANSIONS_NFKC.get(codepoint.code),
        };

        let variants = &[
            hangul,
            starter,
            singleton,
            nonstarter,
            starter_nonstarter_pair,
            starter_nonstarters_sequence,
            starters_sequence,
            two_starters_nonstarter,
            starters_to_nonstarters,
            combines_backwards_case,
            other_expansions,
        ];

        let value = variants
            .iter()
            .find_map(|f| f(&self, codepoint, qc, extra, composed_expansion, stats));

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
// pp.. - (14 бит) - индекс последовательности кодпоинтов в таблице expansions
// ll.. - (5 бит) - количество кодпоинтов в последовательности в таблице expansions
// ss.. - (5 бит) - позиция последнего стартера

macro_rules! blocking_checks {
    ($($expr: expr),+) => {
        if $($expr ||)+ false {
            return None;
        }
    };
}

/// стартер, хангыль
///
/// qmmm ____  ____ ____    ____ ____  ____ ____
///
fn hangul(
    _encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    _extra: &mut Vec<u32>,
    _composed_expansion: Option<(usize, &[u32])>,
    _stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    // не пишем в статистику, т.к. заранее знаем, что туда попадёт (чамо + слоги)

    blocking_checks!(!is_syllable(codepoint.code) && !is_composable_hangul_jamo(codepoint.code));

    // qc: кодпоинты хангыль - проверяем комбинирование если встретили V, T чамо
    let marker = match is_composable_hangul_jamo(codepoint.code) {
        true => {
            assert_eq!(qc, 1);
            MARKER_COMBINES_BACKWARDS
        }
        false => {
            assert_eq!(qc, 0);
            MARKER_HANGUL
        }
    };

    Some(EncodedCodepoint::new((marker << 1) as u32 | (qc as u32)))
}

/// стартер, нет декомпозиции, не комбинируется с предыдущим
///
/// qmmm ____  ____ ____    iiii iiii  iiii iiii
///
fn starter(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    _extra: &mut Vec<u32>,
    _composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("стартер (комбинируемый)");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(
        codepoint.is_nonstarter(),
        !decomposition.is_empty(),
        combines_backwards(codepoint.code)
    );

    // qc: если за стартером следует стартер, то кодпоинт не изменится
    assert_eq!(qc, 0);

    let combining = encoder.combination_info(codepoint.code) as u32;

    match combining == 0 {
        true => Some(EncodedCodepoint::default()),
        false => encoded(MARKER_STARTER, qc, combining << 16, se, codepoint, None),
    }
}

/// синглтон
///
/// qmmm ____    xxxx xxxx    xxxx xxxx  xx__  ____ ____
///
fn singleton(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    _extra: &mut Vec<u32>,
    _composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("синглтон");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(starters_map(decomposition) != "s");

    let c0 = decomposition[0].code as u32;

    // qc: синглтон - это всегда замена одного кодпоинта другим
    assert_eq!(qc, 1);
    assert!(!combines_backwards(c0));

    encoded(MARKER_SINGLETON, qc, c0 << 8, se, codepoint, None)
}

/// нестартер без декомпозиции
///
/// qmmm ____  cccc cccc    ____ ____  ____ ____
///
fn nonstarter(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    _extra: &mut Vec<u32>,
    _composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("нестартер");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(!decomposition.is_empty(), codepoint.is_starter());

    // qc: нестартер - это всегда маркер, что нужно перейти к декомпозиции предыдущего кодпоинта
    assert_eq!(qc, 1);

    let ccc = codepoint.ccc.compressed() as u32;

    encoded(MARKER_NONSTARTER, qc, ccc << 8, se, codepoint, None)
}

/// пара стартер + нестартер
///
/// qxxxx xxxx  xxxx xxx    yyyy yyyy  yyyy yyyy
///
fn starter_nonstarter_pair(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    _extra: &mut Vec<u32>,
    _composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("пара стартер + нестартер (15/16 бит)");
    let decomposition = encoder.decomposition(codepoint.code);

    // пара: стартер + нестартер, не является исключением композиции,
    // кодпоинты - 15 и 16 бит, первый байт стартера > максимальное значение маркера

    blocking_checks!(
        starters_map(decomposition) != "sn",
        decomposition[0].code > 0x7FFF,
        decomposition[1].code > 0xFFFF
    );

    // qc: может быть как 1 так и 0

    let c0 = decomposition[0].code;
    let c1 = decomposition[1].code;

    assert!(!combines_backwards(c0));

    let encoded = encoded(0, qc, (c0 << 1) | (c1 << 16), se, codepoint, None);

    // проверки для отсева пересечения с маркерами
    if let Some(encoded) = &encoded {
        let first_byte = encoded.value as u8;

        assert!(first_byte >> 2 != 0);

        // перенесём эти кодпоинты во внешний блок
        //
        // 0407 - CYRILLIC CAPITAL LETTER YI
        // 2204 - THERE DOES NOT EXIST
        // 2284 - NOT A SUBSET OF
        // 2285 - NOT A SUPERSET OF
        // 2288 - NEITHER A SUBSET OF NOR EQUAL TO
        // 2289 - NEITHER A SUPERSET OF NOR EQUAL TO

        if first_byte >> 1 <= MAX_MARKER {
            return None;
        }
    }

    encoded
}

/// стартер и последовательность нестартеров
///
/// qmmm ____  ssss snnn    nnpp pppp  pppp pppp
///
fn starter_nonstarters_sequence(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    extra: &mut Vec<u32>,
    composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("стартер + нестартеры");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(
        decomposition.len() < 2,
        !decomposition[0].is_starter(),
        !decomposition[1 ..].iter().all(|c| c.is_nonstarter())
    );

    // qc - аналогично паре

    expansion_entry(
        qc,
        &decomposition,
        extra,
        composed_expansion,
        se,
        codepoint,
        encoder,
    )
}

/// последовательность стартеров
///
/// qmmm ____  ssss snnn    nnpp pppp  pppp pppp
///
fn starters_sequence(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    extra: &mut Vec<u32>,
    composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("последовательность стартеров");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(
        decomposition.len() < 2,
        !decomposition.iter().all(|c| c.is_starter())
    );

    for c in decomposition.iter() {
        if c.is_nonstarter() {
            continue;
        }
    }

    // qc: может быть как 1 так и 0

    expansion_entry(
        qc,
        &decomposition,
        extra,
        composed_expansion,
        se,
        codepoint,
        encoder,
    )
}

/// стартер + стартер + нестартер
///
/// qmmm ____  ssss snnn    nnpp pppp  pppp pppp
///
fn two_starters_nonstarter(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    extra: &mut Vec<u32>,
    composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("два стартера + нестартер");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(starters_map(decomposition) != "ssn");

    // qc: может быть как 1 так и 0

    expansion_entry(
        qc,
        &decomposition,
        extra,
        composed_expansion,
        se,
        codepoint,
        encoder,
    )
}

/// исключение - стартеры с декомпозицией в нестартеры
///
/// qmmm ____  ssss snnn    nnpp pppp  pppp pppp
///
fn starters_to_nonstarters(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    extra: &mut Vec<u32>,
    composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("декомпозиция в нестартеры");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(
        decomposition.is_empty(),
        !decomposition.iter().all(|c| c.is_nonstarter())
    );

    // qc: кодпоинт всегда декомпозируется и не собирается обратно
    assert_eq!(qc, 1);

    expansion_entry(
        qc,
        &decomposition,
        extra,
        composed_expansion,
        se,
        codepoint,
        encoder,
    )
}

/// прочие декомпозиции
///
/// qmmm ____  ssss snnn    nnpp pppp  pppp pppp
///
fn other_expansions(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    extra: &mut Vec<u32>,
    composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("прочие декомпозиции");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(decomposition.is_empty());

    // qc: кодпоинт всегда декомпозируется и не собирается обратно
    assert_eq!(qc, 1);

    expansion_entry(
        qc,
        &decomposition,
        extra,
        composed_expansion,
        se,
        codepoint,
        encoder,
    )
}

/// исключение - стартеры, комбинируемые с предыдущим кодпоинтом
///
/// qmmm ____  ____ ____    iiii iiii  iiii iiii
///
fn combines_backwards_case(
    encoder: &EncodeNormalization,
    codepoint: &Codepoint,
    qc: u8,
    _extra: &mut Vec<u32>,
    _composed_expansion: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32>>
{
    let se = stats.touch("комбинируется с предыдущим");
    let decomposition = encoder.decomposition(codepoint.code);

    blocking_checks!(
        codepoint.is_nonstarter(),
        !combines_backwards(codepoint.code)
    );

    assert_eq!(encoder.combination_info(codepoint.code), 0);
    assert!(decomposition.is_empty());

    // qc: может быть скомбинирован с предыдущим
    assert_eq!(qc, 1);

    let combines = encoder.combination_backwards_info(codepoint.code) as u32;

    encoded(
        MARKER_COMBINES_BACKWARDS,
        qc,
        combines << 16,
        se,
        codepoint,
        None,
    )
}

// ---

/// результат кодирования информации о кодпоинте
fn encoded(
    marker: u8,
    qc: u8,
    value: u32,
    stats: &mut EncodeCodepointStatsBlock,
    codepoint: &Codepoint,
    description: Option<&str>,
) -> Option<EncodedCodepoint<u32>>
{
    let description = match description {
        None => format!("{}", codepoint.name),
        Some(description) => format!("{} : {}", codepoint.name, description),
    };

    stats.inc(codepoint.code, description);

    let qc = qc as u32;

    Some(EncodedCodepoint::new(((marker as u32) << 1) | qc | value))
}

/// запись с данными во внешнем блоке
fn expansion_entry(
    qc: u8,
    decomposition: &Vec<Codepoint>,
    extra: &mut Vec<u32>,
    e_composed: Option<(usize, &[u32])>,
    stats: &mut EncodeCodepointStatsBlock,
    codepoint: &Codepoint,
    encoder: &EncodeNormalization,
) -> Option<EncodedCodepoint<u32>>
{
    let mut marker = MARKER_EXPANSION;

    if !decomposition.is_empty() {
        let c0 = &decomposition[0];

        if c0.is_starter() {
            assert!(!combines_backwards(c0.code));
        }
    }

    let (mut expansion, mut description) = bake_expansions(decomposition);

    // декомпозиция отличается от прекомпозиции
    match e_composed {
        Some((pos, exp)) => {
            let precomposition: Vec<Codepoint> =
                exp.iter().map(|&c| UNICODE[&(c >> 8)].clone()).collect();

            marker = match exp.len() {
                0 => {
                    assert_eq!(qc, 0);

                    expansion.insert(0, encoder.combination_info(codepoint.code) as u32);

                    MARKER_EXPANSION_COMBINED_EMPTY
                }
                _ => {
                    description.push_str(" -> ");

                    precomposition.iter().for_each(|codepoint| {
                        description.push_str(
                            format!("U+{:04X} ({}) ", codepoint.code, codepoint.ccc.u8()).as_str(),
                        )
                    });

                    assert!(
                        precomposition
                            .windows(2)
                            .all(|pair| pair[0].ccc.u8() <= pair[1].ccc.u8()),
                        "{}",
                        description
                    );

                    let expansions_info = bake_expansions_info(pos as u32, &precomposition);

                    let last_starter = expansions_info & 0x1F;
                    let count = (expansions_info >> 5) & 0x1F;
                    let index = expansions_info >> 10;

                    let composition_info = match count == 0 {
                        true => encoder.combination_info(codepoint.code) as u32,
                        false => {
                            let last_starter = &precomposition[(last_starter) as usize];
                            encoder.combination_info(last_starter.code) as u32
                        }
                    };

                    // перезапечём информацию о расширении в патче как 3 - 3 - 10

                    assert!(last_starter <= 0x7);
                    assert!(count <= 0x7);
                    assert!(index <= 0x3FF);

                    let expansions_info = last_starter | (count << 3) | (index << 6);

                    assert!(expansions_info <= 0xFFFF);
                    assert!(composition_info <= 0xFFFF);

                    expansion.insert(0, expansions_info | (composition_info << 16));

                    MARKER_EXPANSION_COMBINED_PATCH
                }
            };
        }
        None => {
            assert!(
                decomposition
                    .windows(2)
                    .all(|pair| pair[0].ccc.u8() <= pair[1].ccc.u8()),
                "{}",
                description
            );
        }
    }

    if marker == MARKER_EXPANSION || marker == MARKER_EXPANSION_COMBINED_PATCH {
        assert_eq!(encoder.combination_info(codepoint.code), 0);
    }

    // если подобная последовательность уже записана - получим её индекс
    let position = find_subsequence(extra, &expansion).unwrap_or_else(|| {
        let position = extra.len();
        extra.extend(expansion);
        position
    });

    // информация - индекс, последний стартер, длина декомпозиции
    let info = bake_expansions_info(position as u32, decomposition);

    encoded(
        marker,
        qc,
        info << 8,
        stats,
        codepoint,
        Some(description.as_str()),
    )
}

/// кодпоинт может быть скомбинирован с предыдущим
fn combines_backwards<T: Into<u64>>(code: T) -> bool
{
    COMBINES_BACKWARDS.contains_key(&(code.into() as u32))
}

/// строка, описывающая прекомпозицию, состоящая из символов s и n, где s - стартер, n - нестартер
fn starters_map(codepoints: &Vec<Codepoint>) -> String
{
    codepoints
        .iter()
        .map(|c| match c.is_starter() {
            true => 's',
            false => 'n',
        })
        .collect()
}

/// запекание последовательностей кодпоинтов + строка их описаний
fn bake_expansions(decomposition: &Vec<Codepoint>) -> (Vec<u32>, String)
{
    let mut expansion = vec![];
    let mut description = String::new();

    decomposition.iter().for_each(|codepoint| {
        expansion.push((codepoint.code << 8) | (codepoint.ccc.compressed() as u32));
        description
            .push_str(format!("U+{:04X} ({}) ", codepoint.code, codepoint.ccc.u8()).as_str());
    });

    (expansion, description)
}

/// запечём информацию о декомпозиции, сохранённой в отдельном блоке
fn bake_expansions_info(index: u32, expansion: &Vec<Codepoint>) -> u32
{
    let trailing_nonstarters = expansion
        .iter()
        .rev()
        .take_while(|&c| c.is_nonstarter())
        .count();

    let last_starter = (expansion.len() - trailing_nonstarters).saturating_sub(1);

    assert!(last_starter <= 0x1F);
    assert!(expansion.len() <= 0x1F);
    assert!(index <= 0x1FFF);

    last_starter as u32 | ((expansion.len() as u32) << 5) | (index << 10)
}

/// найти подпоследовательность
fn find_subsequence(entries: &Vec<u32>, find: &Vec<u32>) -> Option<usize>
{
    for (i, window) in entries.windows(find.len()).enumerate() {
        if window == find {
            return Some(i);
        }
    }

    None
}
