use std::collections::HashMap;

use unicode_data::{codepoint::*, hangul};
use unicode_data::{TrieNode, Weights, NFD, NFKD, UNICODE};

use super::{EncodeCodepoint, EncodedCodepoint};
use crate::encode::weights::implicit::is_implicit;
use crate::stats::EncodeCodepointStats;

pub const MARKER_STARTER_SINGLE_WEIGHTS: u8 = 0b_001;
pub const MARKER_STARTER_EXPANSION: u8 = 0b_010;
pub const MARKER_STARTER_DECOMPOSITION_OR_TRIE: u8 = 0b_011;

pub const MARKER_NONSTARTER_SINGLE_WEIGHTS: u8 = 0b_100;
pub const MARKER_NONSTARTER_TRIE: u8 = 0b_101;

pub mod implicit;

#[cfg(test)]
mod tests;

pub struct EncodeWeights<'a>
{
    pub is_canonical: bool,
    pub trie: &'a HashMap<u32, TrieNode>,
    pub decompositions: &'a HashMap<u32, Vec<Codepoint>>,
}

impl<'a> EncodeWeights<'a>
{
    pub fn new(is_canonical: bool, weights_map: &'a HashMap<u32, TrieNode>) -> Self
    {
        let decompositions: &HashMap<u32, Vec<Codepoint>> = match is_canonical {
            true => &NFD,
            false => &NFKD,
        };

        Self {
            is_canonical,
            trie: weights_map,
            decompositions,
        }
    }

    /// декомпозиция кодпоинта в зависимости от типа нормализации
    pub fn decomposition(&self, code: u32) -> Option<&Vec<Codepoint>>
    {
        match self.decompositions.get(&code) {
            Some(decomposition) if !decomposition.is_empty() => Some(decomposition),
            Some(_) | None => None,
        }
    }

    /// проверить, является-ли кодпоинт частью последовательности
    pub fn check_codepoint_sequence(&self, code: u32) -> Option<(u32, &TrieNode)>
    {
        for (&codepoint, node) in self.trie {
            if let Some(children) = &node.children {
                for (&c, node) in children.iter() {
                    if c == code {
                        return Some((codepoint, node));
                    }

                    if let Some(children) = &node.children {
                        for (&c, node) in children.iter() {
                            if c == code {
                                return Some((codepoint, node));
                            }
                            assert!(node.children.is_none())
                        }
                    }
                }
            }
        }

        None
    }
}

pub struct AdditionalInfo<'a>
{
    /// заполняемый дополнительный блок весов / расширений
    pub tries: &'a mut Vec<u32>,
    /// заполняемый дополнительный блок расширений
    pub expansions: &'a mut Vec<u32>,
}

impl<'a> EncodeCodepoint<u64, u32, AdditionalInfo<'a>> for EncodeWeights<'a>
{
    fn encode(
        &self,
        codepoint: &Codepoint,
        extra: &mut AdditionalInfo,
        stats: &mut EncodeCodepointStats,
    ) -> Option<EncodedCodepoint<u64>>
    {
        #[rustfmt::skip]
        let variants = &[
            // вычисляемые веса
            implicit_weights,
            hangul_syllables,               // слоги хангыль
            // одинарные веса
            starter_single_weights,         // стартеры
            singletons,                     // синглтоны
            nonstarter_single_weights,      // нестартеры
            nonstarter_singletons,          // нестартер с декомпозицией в нестартер
            // расширения
            starter_expansions,             // стартеры без декомпозиции с несколькими весами
            nonstarter_expansions,          // нестартеры без декомпозиции с несколькими весами
            // синглтоны + вычисляемые веса
            expansions_implicit_singletons, // синглтоны - декомпозиция в стартер с вычисляемыми весами
            // можно игнорировать декомпозицию           
            ignore_decomposition_pair_starter_nonstarter,   // пара стартер-нестартер
            ignore_decomposition_starters,                  // декомпозиция из стартеров
            ignore_decomposition_other,                     // прочие случаи - сокращение
            // декомпозиция
            decomposition_to_nonstarters,   // декомпозиция в нестартеры
            has_decomposition,              // декомпозиция как: одиночные веса / расширение / обычная
            // последовательности
            sequences,                      // последовательности кодпоинтов
        ];

        let value = variants
            .iter()
            .find_map(|f| f(&self, codepoint, extra, stats))
            .unwrap();

        match value.value {
            0 => None,
            _ => Some(value),
        }
    }
}

macro_rules! blocking_checks {
    ($($expr: expr),+) => {
        if $($expr ||)+ false {
            return None;
        }
    };
}

macro_rules! encoded {
    ($marker: expr) => {
        Some(EncodedCodepoint::new($marker as u64))
    };
    ($marker: expr, $($expr: expr),+) => {
        Some(EncodedCodepoint::new($(($expr) |)+ ($marker as u64)))
    };

}

/// стартер, одинарные веса
/// mmm_ wwww  wwww wwww    wwww wwww  wwww wwww        wwww ____  ____ ____    ____ ____  _____ ____
macro_rules! encoded_starter_single_weights {
    ($weights: expr) => {{
        assert!($weights <= 0xFFFF_FFFF); // 32 бита

        encoded!(MARKER_STARTER_SINGLE_WEIGHTS, $weights << 4)
    }};
}

/// нестартер, одинарные веса
/// mmm_ wwww  wwww wwww    wwww wwww  wwww wwww        wwww cccc  cc__ ____    ____ ____  _____ ____
macro_rules! encoded_nonstarter_single_weights {
    ($ccc: expr, $weights: expr) => {{
        assert!($ccc <= 0x3F); // 6 бит
        assert!($weights <= 0xFFFF_FFFF); // 32 бита

        encoded!(MARKER_NONSTARTER_SINGLE_WEIGHTS, $weights << 4, $ccc << 36)
    }};
}

/// стартер, расширение
/// mmm_ iiii  iiii iiii    iiii llll  llll ____
macro_rules! encoded_starter_expansion {
    ($pos: expr, $len: expr) => {{
        assert!($len <= 0xFF); // 8 бит
        assert!($pos <= 0xFFFF); // 16 бит

        encoded!(MARKER_STARTER_EXPANSION, $pos << 4, $len << 20)
    }};
}

/// стартер - декомпозиция или последовательности
/// mmmT iiii  iiii iiii    iiii cccc  cc__ ____
macro_rules! encoded_starter_decomposition_or_trie {
    ($is_trie: expr, $ccc: expr, $pos: expr) => {{
        assert!($ccc <= 0xFF); // 8 бит
        assert!($pos <= 0xFFFF); // 16 бит

        let is_trie = $is_trie as u64;

        encoded!(
            MARKER_STARTER_DECOMPOSITION_OR_TRIE,
            is_trie << 3,
            $pos << 4,
            $ccc << 20
        )
    }};
}

/// нестартер - расширение, сокращение, декомпозиция
/// mmm_ iiii  iiii iiii    iiii cccc  cc__ ____
macro_rules! encoded_nonstarter_trie {
    ($ccc: expr, $pos: expr) => {{
        assert!($ccc <= 0x3F); // 6 бит
        assert!($pos <= 0xFFFF); // 16 бит

        encoded!(MARKER_NONSTARTER_TRIE, $pos << 4, $ccc << 20)
    }};
}

macro_rules! stats_codepoint {
    ($stats: ident, $codepoint: expr) => {
        $stats.inc($codepoint.code, format!("{}", $codepoint.name))
    };
    ($stats: ident, $codepoint: expr; $description: expr) => {
        $stats.inc(
            $codepoint.code,
            format!("{} : {}", $codepoint.name, $description),
        )
    };
    ($stats: ident, $codepoint: expr, $trie_node:expr) => {
        let weights_string: String = $trie_node.weights.iter().map(|w| w.formatted()).collect();
        $stats.inc(
            $codepoint.code,
            format!("{} - {}", $codepoint.name, weights_string),
        );
    };
}

/// MARKER_STARTER_SINGLE_WEIGHTS: стартер без декомпозиции, одинарные веса
fn starter_single_weights(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("обычный стартер");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        hangul::is_syllable(codepoint.code), // не слог хангыль
        codepoint.is_nonstarter(),           // не нестартер
        decomposition.is_some(),             // не должен иметь декомпозицию
        trie.children.is_some(),             // нет последовательностей, начинающихся с этого кодпоинта
        trie.weights.len() != 1              // одинарные веса
    );

    // P.S. может участвовать в комбинациях

    let weights = bake_weights(&trie.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_starter_single_weights!(weights)
}

/// MARKER_STARTER_SINGLE_WEIGHTS: синглтон, одинарные веса
fn singletons(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("синглтон, одинарные веса");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    blocking_checks!(
        codepoint.is_nonstarter(),        // только стартеры
        decomposition.len() != 1,         // должен быть синглтоном
        decomposition[0].is_nonstarter(), // декомпозиция - в стартер
        trie.children.is_some(),          // нет последовательностей, начинающихся с этого кодпоинта
        trie.weights.len() != 1           // одинарные веса
    );

    // мы должны быть уверены, что у кодпоинта и декомпозиции одинаковые веса и нет последовательностей
    // более того, кодпоинт декомпозиции - обычный стартер, не являющийся частью каких-либо комбинаций

    assert_eq!(trie, &encoder.trie[&decomposition[0].code]);

    let weights = bake_weights(&trie.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_starter_single_weights!(weights)
}

/// MARKER_NONSTARTER_SINGLE_WEIGHTS: нестартер без декомпозиции
fn nonstarter_single_weights(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("обычный нестартер");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_starter(),            // кодпоинт - нестартер
        decomposition.is_some(),           // не имеет декомпозиции
        trie.children.is_some(),           // нет последовательностей, начинающихся с этого кодпоинта
        trie.weights.len() != 1            // одинарные веса
    );

    // P.S. может участвовать в комбинациях

    let ccc = codepoint.ccc.compressed() as u64;
    let weights = bake_weights(&trie.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_nonstarter_single_weights!(ccc, weights)
}

/// MARKER_NONSTARTER_SINGLE_WEIGHTS: нестартер с декомпозицией в другой нестартер, с теми же весами и CCC
fn nonstarter_singletons(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("нестартер - синглтон");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_starter(),                 // кодпоинт - нестартер
        decomposition.len() != 1,               // декомпозиция - в 1 нестартер
        decomposition[0].ccc != codepoint.ccc,  // CCC оригинального кодпоинта и CCC декомпозиции совпадают
        trie.children.is_some(),                // нет последовательностей, начинающихся с этого кодпоинта
        trie.weights.len() != 1                 // одинарные веса
    );

    // не участвуют в комбинациях
    [codepoint.code, decomposition[0].code]
        .iter()
        .for_each(|&c| {
            assert!(encoder.check_codepoint_sequence(c).is_none());
        });

    // одинаковые веса
    assert_eq!(trie, &encoder.trie[&decomposition[0].code]);

    // U+0340 - COMBINING GRAVE TONE MARK - [.0000.0025.0002]
    // U+0341 - COMBINING ACUTE TONE MARK - [.0000.0024.0002]
    // U+0343 - COMBINING GREEK KORONIS - [.0000.0022.0002]
    assert!([0x0340, 0x0341, 0x0343].contains(&codepoint.code));

    let ccc = codepoint.ccc.compressed() as u64;
    let weights = bake_weights(&trie.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_nonstarter_single_weights!(ccc, weights)
}

/// MARKER_STARTER_EXPANSION: расширения для стартеров (несколько весов)
fn starter_expansions(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("стартер, расширения");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_nonstarter(),      // стартер
        decomposition.is_some(),        // нет декомпозиции
        trie.children.is_some(),        // нет последовательностей, начинающихся с этого кодпоинта
        trie.weights.len() == 1         // является расширением
    );

    // P.S.: может участвовать в комбинациях

    let weights = bake_weights_vec(trie.weights);
    let (len, pos) = bake_extra(&mut extra.expansions, &weights);

    stats_codepoint!(stats, codepoint, trie);
    encoded_starter_expansion!(pos, len)
}

/// MARKER_NONSTARTER_TRIE: расширения для нестартеров (несколько весов) / последовательности (сокращения)
fn nonstarter_expansions(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("нестартер, расширения или сокращения");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_starter(),                                  // нестартер
        decomposition.is_some(),                                 // нет декомпозиции
        (trie.weights.len() == 1) && trie.children.is_none()     // несколько весов (или U+0F71)
    );

    // U+0F71 TIBETAN VOWEL SIGN AA
    if trie.children.is_some() {
        assert!(codepoint.code == 0x0F71);

        for (code, trie) in trie.children.as_ref().unwrap() {
            assert!(trie.children.is_none());
            assert!(UNICODE.get(code)?.is_nonstarter());
        }
    } else {
        // не участвуют в комбинациях
        assert!(encoder.check_codepoint_sequence(codepoint.code).is_none());
    }

    let data = bake_trie(codepoint.code, trie, true);
    let (_, pos) = bake_extra(&mut extra.tries, &data);

    let ccc = codepoint.ccc.compressed() as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_nonstarter_trie!(ccc, pos)
}

/// MARKER_STARTER_EXPANSION: синглтоны - декомпозиция в стартер с вычисляемыми весами
fn expansions_implicit_singletons(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("синглтоны - декомпозиция в стартер с вычисляемыми весами");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        decomposition.len() != 1,                   // синглтон
        decomposition[0].is_nonstarter(),           // декомпозиция в стартер
        !is_implicit(decomposition[0].code, true)   // ... с вычисляемыми весами
    );

    // убедимся, что не существует нестартеров с декомпозицией в стартер с вычисляемыми весами
    assert!(codepoint.is_starter());

    // не участвуют в комбинациях
    [codepoint.code, decomposition[0].code]
        .iter()
        .for_each(|&c| {
            assert!(encoder.check_codepoint_sequence(c).is_none());
        });

    // альтернативный вариант - запечь с отдельным маркером AAAA / BBBB; с записью кодпоинта
    // тут выбор - уменьшить размер данных или избавиться от дополнительной проверки

    let weights = bake_weights_vec(trie.weights);
    let (len, pos) = bake_extra(&mut extra.expansions, &weights);

    stats_codepoint!(stats, codepoint, trie);
    encoded_starter_expansion!(pos, len)
}

/// MARKER_STARTER_SINGLE_WEIGHTS:
/// кодпоинт имеет декомпозицию - стартер + нестартер
/// случай, когда декомпозицию можно игнорировать и запечь кодпоинт как стартер с одинарными весами
fn ignore_decomposition_pair_starter_nonstarter(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("декомпозиция не нужна, кейс - стартер-нестартер");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;
    let c0_trie = &encoder.trie[&decomposition[0].code];

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_nonstarter(),              // стартер
        decomposition.len() != 2,               // декомпозиция в пару
        decomposition[0].is_nonstarter(),       // стартер +
        decomposition[1].is_starter(),          // нестартер
        trie.children.is_some(),                // нет последовательностей, начинающихся с этого кодпоинта
        c0_trie.children.is_none()              // проверяем только сокращения
    );

    /*
        если последовательность, начинающаяся с первого кодпоинта декомпозиции продолжается нестартером
        с CCC < CCC второго кодпоинта декомпозиции, тогда декомпозицию делать надо (кодпоинты U+0622, U+0623):

        U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE -> U+0627(0) U+0653(230)
        U+0622 / U+0627 U+0653 - веса [.278F.0020.0002]

        представим (если не сделаем декомпозицию), что за U+0622 идёт U+0655(220),
        тогда вместо:  [.2794.0020.0002][.0000.0082.0002]
        мы получим:    [.278F.0020.0002][.0000.0084.0002]

        если CCC в последовательности > CCC в декомпозиции:

        U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW -> U+0627(0) U+0655(220)
        U+0625 / U+0627 U+0655 [.2794.0020.0002]

        если не сделаем декомпозицию, а за U+0625 идёт U+0654(230):
        мы получим корректный результат - [.2794.0020.0002][.0000.0083.0002]
        так как уже сделано сокращение
    */

    let c0_trie_children = c0_trie.children.as_ref()?;

    for c1 in c0_trie_children.keys() {
        let c1_ccc = get_ccc(c1);

        if c1_ccc.is_starter() {
            continue;
        }

        if c1_ccc < decomposition[1].ccc {
            return None;
        }
    }

    // убедимся, что стартер декомпозиции не участвует в contractions с предыдущими кодпоинтами
    assert!(encoder
        .check_codepoint_sequence(decomposition[0].code)
        .is_none());

    // проверим сами веса
    let contraction_trie = c0_trie_children.get(&decomposition[1].code);

    match contraction_trie {
        // нашли последовательность - это сокращение (contraction)
        Some(trie) => {
            assert_eq!(trie.weights, trie.weights);
            assert_eq!(trie.weights.len(), 1);
        }
        /*
           если не нашли, значит декомпозиция не является сокращением, и декомпозицию делать надо:

           первая причина - если за кодпоинтом идёт нестартер с CCC < CCC нестартера декомпозиции,
           при NFD нестартеры будут пересортированы.

           вторая причина - если за нашим рассматриваемым кодпоинтом будет идти нестартер, составляющий
           комбинацию "стартер декомпозиции - следующий нестартер", и у этого нестартера ССС отличается от
           CCC нестартера декомпозиции - декомпозицию делать надо.
        */
        None => {
            return None;
        }
    };

    let weights = bake_weights(&trie.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_starter_single_weights!(weights)
}

/// MARKER_STARTER_SINGLE_WEIGHTS | MARKER_EXPANSION: кодпоинт имеет декомпозицию только из стартеров
fn ignore_decomposition_starters(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("декомпозиция не нужна, кейс - только стартеры");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_nonstarter(),                       // стартер
        decomposition.iter().any(|c| c.is_nonstarter()), // декомпозиция из стартеров
        trie.children.is_some()                          // нет последовательностей, начинающихся с этого кодпоинта
    );

    // небольшой оффтопик - сюда могли бы попасть 2 кодпоинта: U+0CCA и U+0DDC, но они - частный случай сокращений

    // убедимся, что первый стартер декомпозиции не участвует в комбинациях с предыдущими кодпоинтами
    assert!(encoder
        .check_codepoint_sequence(decomposition[0].code)
        .is_none());

    // кодпоинты декомпозиции после первого - не являются началом последовательности
    decomposition[1 ..]
        .iter()
        .for_each(|c| assert!(encoder.trie[&c.code].children.is_none()));

    stats_codepoint!(stats, codepoint, trie);

    match trie.weights.len() {
        // сокращения
        1 => {
            let weights = bake_weights(&trie.weights[0]) as u64;

            stats_codepoint!(stats, codepoint, trie);
            encoded_starter_single_weights!(weights)
        }
        // обычная последовательность стартеров
        _ => {
            let weights = bake_weights_vec(trie.weights);
            let (len, pos) = bake_extra(&mut extra.expansions, &weights);

            encoded_starter_expansion!(pos, len)
        }
    }
}

/// MARKER_STARTER_SINGLE_WEIGHTS: частные случаи декомпозиции
fn ignore_decomposition_other(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("декомпозиция не нужна, прочие кейсы");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    let starters_map = decomposition
        .iter()
        .map(|c| match c.is_starter() {
            true => 's',
            false => 'n',
        })
        .collect::<String>();

    // кейс - сокращение стартер-стартер-нестартер
    if !["ssn"].contains(&starters_map.as_str()) {
        return None;
    }

    // кейс [ssn] 0DDD SINHALA VOWEL SIGN KOMBUVA HAA DIGA AELA-PILLA
    // на момент Unicode 15.1 - только один такой случай

    if starters_map == "ssn" {
        // только сокращения
        if trie.weights.len() != 1 {
            return None;
        }

        // убедимся, что кодпоинт / первый стартер декомпозиции не участвует в комбинациях с предыдущими кодпоинтами
        [codepoint.code, decomposition[0].code]
            .iter()
            .for_each(|&c| {
                assert!(encoder.check_codepoint_sequence(c).is_none());
            });

        let c0_trie = encoder.trie.get(&decomposition[0].code)?;

        // убедимся, что второй кодпоинт декомпозиции - часть последовательности
        let c1_trie = c0_trie.children.as_ref()?.get(&decomposition[1].code)?;

        // наконец, проверим последний уровень с нестартером
        for c2 in c1_trie.children.as_ref()?.keys() {
            let c2_ccc = get_ccc(c2);

            if c2_ccc.is_starter() {
                continue;
            }

            if c2_ccc < decomposition[2].ccc {
                return None;
            }
        }

        // прошли все проверки - значит, сокращение
        let weights = bake_weights(&trie.weights[0]) as u64;

        stats_codepoint!(stats, codepoint, trie);
        return encoded_starter_single_weights!(weights);
    }

    // место под следующие кейсы
    unreachable!()
}

/// MARKER_NONSTARTER_TRIE: декомпозиция в нестартеры
fn decomposition_to_nonstarters(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("декомпозиция в нестартеры");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        decomposition.iter().any(|c| c.is_starter())    // декомпозиция в нестартеры
    );

    let mut data = vec![];

    for (i, entry_codepoint) in decomposition.iter().enumerate() {
        let trie = encoder.trie.get(&entry_codepoint.code)?;
        let is_last = i == decomposition.len() - 1;

        data.extend(bake_trie(entry_codepoint.code, trie, is_last));
    }

    let (_, pos) = bake_extra(&mut extra.tries, &data);

    // CCC последнего кодпоинта декомпозиции
    let ccc = decomposition.last()?.ccc.compressed() as u64;

    stats_codepoint!(stats, codepoint, trie);
    encoded_nonstarter_trie!(ccc, pos)
}

/// MARKER_STARTER_DECOMPOSITION_OR_TRIE: кодпоинт с декомпозицией
fn has_decomposition(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("кодпоинты с декомпозицией");
    let decomposition = encoder.decomposition(codepoint.code)?;
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        decomposition.len() <= 1,           // декомпозиция > 1 кодпоинта
        decomposition[0].is_nonstarter()    // декомпозиция начинается со стартера
    );

    // карта стартеров. для NFD случая это "ss", "sn", "snn", "snnn"
    let starters_map = decomposition
        .iter()
        .map(|c| match c.is_starter() {
            true => 's',
            false => 'n',
        })
        .collect::<String>();

    // есть 2 особенных кодпоинта - U+0CCA, U+0DDC, они являются первыми кодпоинтами сокращений,
    // с декомпозицией в 2 стартера. имеет смысл убрать их из этого блока
    if trie.children.is_some() {
        assert!([0x0CCA, 0x0DDC].contains(&codepoint.code));
        return None;
    }

    // кодпоинты декомпозиции, кроме первого, не являются первыми элементами последовательностей
    assert!(decomposition[1 ..].iter().all(|c| encoder
        .trie
        .get(&c.code)
        .unwrap()
        .children
        .is_none()));

    // можно быть уверенными, что если встретили декомпозицию - она не является продолжением последовательности
    assert!(encoder.check_codepoint_sequence(codepoint.code).is_none());
    assert!(encoder
        .check_codepoint_sequence(decomposition[0].code)
        .is_none());

    let mut description = format!("[{}] ", starters_map);

    let mut data = vec![];

    // в начале данных декомпозиции запишем кодпоинт с весами
    data.extend(bake_trie(codepoint.code, trie, true));

    // затем - декомпозицию
    for (i, codepoint) in decomposition.iter().enumerate() {
        let trie = encoder.trie.get(&codepoint.code)?;
        let is_last = i == decomposition.len() - 1;

        description
            .push_str(format!("U+{:04X} ({}) ", codepoint.code, codepoint.ccc.u8()).as_str());

        data.extend(bake_trie(codepoint.code, trie, is_last));
    }

    let description = format!(
        "{}{}",
        description,
        get_trie_description(codepoint, &encoder.trie[&decomposition[0].code])
    );

    let (_, pos) = bake_extra(&mut extra.tries, &data);

    // CCC последнего кодпоинта декомпозиции
    let ccc = decomposition.last()?.ccc.compressed() as u64;

    assert!(ccc != 0);

    stats_codepoint!(stats, codepoint; description);
    encoded_starter_decomposition_or_trie!(false, ccc, pos)
}

/// 0: вычисляемые веса
fn implicit_weights(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    _stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let decomposition = encoder.decomposition(codepoint.code);
    let trie = encoder.trie.get(&codepoint.code);

    // чтобы уменьшить количество проверок, кодпоинты (Unified_Ideograph=True AND Block=CJK_Compatibility_Ideographs)
    // записаны в allkeys
    #[rustfmt::skip]
    blocking_checks!(
        trie.is_some(),                         // веса не заданы
        decomposition.is_some(),                // нет декомпозиции
        !is_implicit(codepoint.code, false)     // является вычисляемым значением
    );

    encoded!(0)
}

/// MARKER_STARTER_DECOMPOSITION_OR_TRIE: слоги хангыль
fn hangul_syllables(
    _encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    _stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    blocking_checks!(!(0xAC00 ..= 0xD7A3).contains(&codepoint.code));
    encoded_starter_decomposition_or_trie!(false, 0xFF, 0)
}

/// MARKER_STARTER_DECOMPOSITION_OR_TRIE: последовательности кодпоинтов
fn sequences(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("последовательности");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie = encoder.trie.get(&codepoint.code)?;

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_nonstarter(),     // стартер
        trie.children.is_none()        // кодпоинт является началом последовательности
    );

    // U+0CCA KANNADA VOWEL SIGN O
    // U+0DDC SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA
    if decomposition.is_some() {
        assert!([0x0CCA, 0x0DDC].contains(&codepoint.code));
    }

    let trie_vec = bake_trie(codepoint.code, trie, true);
    let (_, pos) = bake_extra(&mut extra.tries, &trie_vec);

    // вместо CCC (0) запишем 0xFF как маркер необходимости обработки
    let ccc = 0xFF;

    stats_codepoint!(stats, codepoint; get_trie_description(codepoint, trie));
    encoded_starter_decomposition_or_trie!(true, ccc, pos)
}

// --------------------------------------------------------------------------------------------------------------------

/// запечь кодпоинт:
/// FLcc cccc  nnnn nnnn    nnnn nnnn  nnww wwww
///  - F - флаг наличия потомков
///  - L - последний элемент текущего уровня
///  - ccc - ССС в сжатом виде (индекс значения CCC) - 6 бит
///  - n - кодпоинт - 18 бит
///  - w - длина весов - 6 бит
fn bake_codepoint(code: u32, has_children: bool, is_last: bool, w_len: u32) -> u32
{
    let ccc = get_ccc(&code).compressed() as u32;
    let has_children = has_children as u32;
    let is_last = is_last as u32;

    assert!(w_len <= 0x3F);

    has_children | (is_last << 1) | (ccc << 2) | (code << 8) | (w_len << 26)
}

/// запечь последовательности весов
fn bake_trie(code: u32, node: &TrieNode, is_last: bool) -> Vec<u32>
{
    let mut data: Vec<u32> = vec![];

    let entry = bake_codepoint(
        code,
        node.children.is_some(),
        is_last,
        node.weights.len() as u32,
    );
    let weights = bake_weights_vec(node.weights);

    data.push(entry);
    data.extend(weights);

    let children = match &node.children {
        Some(children) => children,
        None => return data,
    };

    // отсортируем потомков по CCC
    let mut sorted_keys: Vec<&u32> = children.keys().collect();
    sorted_keys.sort_by(|a, b| match get_ccc(a).cmp(&get_ccc(b)) {
        std::cmp::Ordering::Equal => a.cmp(b),
        other => other,
    });

    let last = sorted_keys.len() - 1;

    for (i, &code) in sorted_keys.iter().enumerate() {
        let node = &children[code];
        data.extend(bake_trie(*code, node, i == last));
    }

    data
}

/// цепочка весов из их запечённых значений
fn bake_weights_vec(weights: &Vec<Weights>) -> Vec<u32>
{
    weights.iter().map(|w| bake_weights(w)).collect()
}

/// веса L1, L2, L3 / маркер переменного веса как u32
/// 1111 1111  1111 1111    2222 2222  2333 33v_
fn bake_weights(weights: &Weights) -> u32
{
    // максимальные значения весов в DUCET / CLDR UND:
    // L1: 0xFFFE
    // L2: 0x0124
    // L3: 0x001E

    assert!(weights.l2 <= 0x1FF); // 9 бит
    assert!(weights.l3 <= 0x1F); // 5 бит

    let is_variable = weights.is_variable as u32;

    (weights.l1 as u32)
        | ((weights.l2 as u32) << 16)
        | ((weights.l3 as u32) << (16 + 9))
        | (is_variable << (16 + 9 + 5))
}

/// получить CCC
fn get_ccc(code: &u32) -> CanonicalCombiningClass
{
    match UNICODE.get(&code) {
        Some(codepoint) => codepoint.ccc,
        None => CanonicalCombiningClass::NotReordered,
    }
}

/// найти подпоследовательность
fn find_subsequence(entries: &Vec<u32>, find: &Vec<u32>) -> Option<usize>
{
    if find.len() < entries.len() {
        return None;
    }

    for (i, window) in entries.windows(find.len()).enumerate() {
        if window == find {
            return Some(i);
        }
    }

    None
}

/// записать данные в блок весов (или, если последовательность существует - вернуть существующий оффсет)
fn bake_extra(data: &mut Vec<u32>, entry: &Vec<u32>) -> (u64, u64)
{
    let len = entry.len() as u64;
    let pos = match find_subsequence(&data, &entry) {
        Some(pos) => pos,
        None => {
            let pos = data.len();
            data.extend(entry);
            pos
        }
    } as u64;

    (len, pos)
}

// описание деревьев
fn get_trie_description(codepoint: &Codepoint, trie: &TrieNode) -> String
{
    if trie.children.is_none() {
        return "".to_owned();
    }

    let mut description = "\n".to_owned();
    for (code2, node) in trie.children.as_ref().unwrap().iter() {
        description.push_str(
            format!(
                "\t{:04X}({}) + {:04X}({}) - {}\n",
                codepoint.code,
                codepoint.ccc.u8(),
                code2,
                UNICODE.get(code2).unwrap().ccc.u8(),
                node.weights
                    .iter()
                    .map(|w| w.formatted())
                    .collect::<String>()
            )
            .as_str(),
        );

        if let Some(children) = node.children.as_ref() {
            for (code3, node) in children.iter() {
                description.push_str(
                    format!(
                        "\t\t{:04X}({}) + {:04X}({}) + {:04X}({}) - {}\n",
                        codepoint.code,
                        codepoint.ccc.u8(),
                        code2,
                        UNICODE.get(code2).unwrap().ccc.u8(),
                        code3,
                        UNICODE.get(code3).unwrap().ccc.u8(),
                        node.weights
                            .iter()
                            .map(|w| w.formatted())
                            .collect::<String>()
                    )
                    .as_str(),
                );

                assert!(node.children.is_none());
            }
        }
    }

    description.trim_end().to_owned()
}
