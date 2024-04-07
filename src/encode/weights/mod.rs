use std::collections::HashMap;

use unicode_data::{codepoint::*, hangul};
use unicode_data::{TrieNode, Weights, NFD, NFKD, UNICODE};

use super::{EncodeCodepoint, EncodedCodepoint};
use crate::encode::weights::implicit::is_implicit;
use crate::stats::EncodeCodepointStats;

pub const MARKER_STARTER_SINGLE_WEIGHTS: u8 = 0b_001;
pub const MARKER_NONSTARTER_SINGLE_WEIGHTS: u8 = 0b_010;
pub const MARKER_DECOMPOSITION: u8 = 0b_011;
pub const MARKER_EXPANSION: u8 = 0b_100;
pub const MARKER_NONSTARTER_EXPANSION: u8 = 0b_101;
pub const MARKER_CHILDREN: u8 = 0b_110;
pub const MARKER_HANGUL: u8 = 0b_111;

pub mod implicit;

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
    pub fn check_codepoint_sequence(&self, code: u32) -> Option<&Vec<Weights>>
    {
        for node in self.trie.values() {
            if let Some(children) = &node.children {
                for (&c, node) in children.iter() {
                    if c == code {
                        return Some(node.weights);
                    }

                    if let Some(children) = &node.children {
                        for (&c, node) in children.iter() {
                            if c == code {
                                return Some(node.weights);
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
    /// информация о весах кодпоинта
    pub trie_node: Option<&'a TrieNode>,
    /// заполняемый дополнительный блок весов / расширений
    pub weights: &'a mut Vec<u32>,
    /// заполняемый массив декомпозиций
    pub decompositions: &'a mut Vec<u32>,
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
            implicit_weights,
            // одинарные веса
            starter_single_weights,         // стартеры
            singletons,                     // синглтоны
            nonstarter_single_weights,      // нестартеры
            // расширения
            expansions,                     // стартеры и не-стартеры без декомпозиции с несколькими весами
            expansions_implicit_singletons, // синглтоны - декомпозиция в стартер с вычисляемыми весами
            // декомпозиция           
            has_decomposition,              // декомпозиция как: одиночные веса / расширение / обычная
            hangul_syllables,               // слоги хангыль
            has_children,                   // последовательности кодпоинтов
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

macro_rules! trie_node_or_return {
    ($node: expr) => {
        match $node {
            Some(trie_node) => trie_node,
            None => return None,
        }
    };
}

macro_rules! decomposition_or_return {
    ($encoder: expr, $codepoint: expr) => {
        match $encoder.decomposition($codepoint.code) {
            Some(decomposition) => decomposition,
            None => return None,
        }
    };
}

macro_rules! encoded {
    ($marker: expr) => {
        Some(EncodedCodepoint::new($marker as u64))
    };
    ($marker: expr, $($expr: expr),+) => {
        Some(EncodedCodepoint::new($(($expr) |)+ ($marker as u64)))
}}

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

/// стартер без декомпозиции, одинарные веса
///
/// mmm_ wwww  wwww wwww    wwww wwww  wwww wwww        wwww ____  ____ ____    ____ ____  _____ ____
///
fn starter_single_weights(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("обычный стартер");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie_node = trie_node_or_return!(extra.trie_node);

    #[rustfmt::skip]
    blocking_checks!(
        hangul::is_syllable(codepoint.code), // не слог хангыль
        codepoint.is_nonstarter(),           // не нестартер
        decomposition.is_some(),             // не должен иметь декомпозицию
        trie_node.children.is_some(),        // нет последовательностей, начинающихся с этого кодпоинта
        trie_node.weights.len() != 1         // одинарные веса
    );

    let weights = bake_weights(&trie_node.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie_node);
    encoded!(MARKER_STARTER_SINGLE_WEIGHTS, weights << 4)
}

/// синглтон, одинарные веса
///
/// mmm_ wwww  wwww wwww    wwww wwww  wwww wwww        wwww ____  ____ ____    ____ ____  _____ ____
///
fn singletons(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("синглтон, одинарные веса");
    let decomposition = decomposition_or_return!(encoder, codepoint);
    let trie_node = trie_node_or_return!(extra.trie_node);

    blocking_checks!(
        codepoint.is_nonstarter(),        // только стартеры
        decomposition.len() != 1,         // должен быть синглтоном
        decomposition[0].is_nonstarter(), // декомпозиция - в стартер
        trie_node.children.is_some(),     // нет последовательностей, начинающихся с этого кодпоинта
        trie_node.weights.len() != 1      // одинарные веса
    );

    // мы должны быть уверены, что у кодпоинта и декомпозиции одинаковые веса и нет последовательностей
    assert_eq!(trie_node, &encoder.trie[&decomposition[0].code]);

    let weights = bake_weights(&trie_node.weights[0]) as u64;

    stats_codepoint!(stats, codepoint, trie_node);
    encoded!(MARKER_STARTER_SINGLE_WEIGHTS, weights << 4)
}

/// нестартер без декомпозиции / одинарная декомпозиция, одинарные веса
///
/// mmm_ cccc  ccww wwww    wwww wwww  wwww wwww        wwww wwww  ww__ ____    ____ ____  _____ ____
///
fn nonstarter_single_weights(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("обычный нестартер");
    let decomposition = encoder.decomposition(codepoint.code);
    let trie_node = trie_node_or_return!(extra.trie_node);

    #[rustfmt::skip]
    blocking_checks!(
        codepoint.is_starter(),                                         // кодпоинт - нестартер
        decomposition.is_some() && decomposition.unwrap().len() != 1,   // не имеет декомпозиции 
        trie_node.children.is_some(),                                   // нет последовательностей, начинающихся с этого кодпоинта
        trie_node.weights.len() != 1                                    // одинарные веса
    );

    // нестартер с декомпозицией: одинаковые CCC у кодпоинта и его декомпозиции
    if let Some(decomposition) = decomposition {
        let decomposed = &decomposition[0];
        assert_eq!(codepoint.ccc, decomposed.ccc);

        // не участвуют в contrations
        assert!(
            encoder.check_codepoint_sequence(decomposed.code).is_none(),
            "{:04X} {}",
            codepoint.code,
            codepoint.name
        );

        // 0340 - COMBINING GRAVE TONE MARK [.0000.0025.0002]
        // 0341 - COMBINING ACUTE TONE MARK [.0000.0024.0002]
        // 0343 - COMBINING GREEK KORONIS [.0000.0022.0002]
    }

    let ccc = codepoint.ccc.compressed() as u64;
    let weights = bake_weights(&trie_node.weights[0]) as u64;

    assert!(ccc <= 0x3F); // 6 бит

    stats_codepoint!(stats, codepoint, trie_node);
    encoded!(MARKER_NONSTARTER_SINGLE_WEIGHTS, ccc << 4, weights << 10)
}

/// расширения (несколько весов)
///
/// mmm_ nnnn  nncc cccc    iiii iiii  iiii iiii        ____ ____  ____ ____    ____ ____  _____ ____
///
fn expansions(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let (group, marker) = match codepoint.is_starter() {
        true => ("стартер, расширения", MARKER_EXPANSION),
        false => ("нестартер, расширения", MARKER_NONSTARTER_EXPANSION),
    };

    let stats = stats.touch(group);
    let decomposition = encoder.decomposition(codepoint.code);
    let trie_node = trie_node_or_return!(extra.trie_node);

    #[rustfmt::skip]
    blocking_checks!(
        decomposition.is_some(),        // нет декомпозиции
        trie_node.children.is_some(),   // нет последовательностей, начинающихся с этого кодпоинта
        trie_node.weights.len() == 1    // несколько весов
    );

    let weights = bake_weights_vec(trie_node.weights, false);
    let (len, pos) = bake_extra(&mut extra.weights, &weights);
    let ccc = get_ccc(&codepoint.code).compressed() as u64;

    assert!(len <= 0x3F); // 6 бит
    assert!(ccc <= 0x3F); // 6 бит
    assert!(pos <= 0xFFFF); // u16 достаточно

    stats_codepoint!(stats, codepoint, trie_node);
    encoded!(marker, len << 4, ccc << 10, pos << 16)
}

/// синглтоны - декомпозиция в стартер с вычисляемыми весами
///
/// mmm_ nnnn  nncc cccc    iiii iiii  iiii iiii        ____ ____  ____ ____    ____ ____  _____ ____
///
fn expansions_implicit_singletons(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("синглтоны - декомпозиция в стартер с вычисляемыми весами");
    let decomposition = decomposition_or_return!(encoder, codepoint);
    let trie_node = trie_node_or_return!(extra.trie_node);

    #[rustfmt::skip]
    blocking_checks!(
        decomposition.len() != 1,                   // синглтон
        decomposition[0].is_nonstarter(),           // декомпозиция в стартер
        !is_implicit(decomposition[0].code, true)   // ... с вычисляемыми весами
    );

    let weights = bake_weights_vec(trie_node.weights, false);
    let (len, pos) = bake_extra(&mut extra.weights, &weights);

    stats_codepoint!(stats, codepoint, trie_node);
    encoded!(MARKER_EXPANSION, len << 4, pos << 16)
}

/// кодпоинт с декомпозицией
///
/// mmm_ iiii  iiii iiii    iiii llll  llll ____        ____ ____  ____ ____    ____ ____  _____ ____
///
fn has_decomposition(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let decomposition = decomposition_or_return!(encoder, codepoint);
    let trie_node = trie_node_or_return!(extra.trie_node);

    // декомпозиция всегда > 1 элемента, т.к. синглтоны обрабатываются ранее
    assert!(decomposition.len() > 1);
    // кодпоинты декомпозиции, кроме первого, не являются первыми элементами последовательностей
    assert!(decomposition[1 ..].iter().all(|c| encoder
        .trie
        .get(&c.code)
        .unwrap()
        .children
        .is_none()));

    let first = decomposition.first().unwrap();
    let last = decomposition.last().unwrap();

    let first_trie = encoder.trie.get(&first.code).unwrap();
    let last_trie = encoder.trie.get(&last.code).unwrap();

    // кейс: первый и последний кодпоинт декомпозиции - стартеры,
    // вся декомпозиция является последовательностью (сокращением)

    if first.is_starter() && last.is_starter() && first_trie.children.is_some() {
        let mut current_node = first_trie;
        let mut iter = decomposition[1 ..].iter();

        let is_contraction = loop {
            let dec_element = match iter.next() {
                Some(e) => e,
                None => break true,
            };

            match &current_node.children {
                Some(children) => match children.get(&dec_element.code) {
                    Some(node) => current_node = node,
                    None => break false,
                },
                None => break false,
            };
        };

        // если кодпоинт является сокращением - запишем его как обычный стартер с одинарными весами

        if is_contraction {
            let stats = stats.touch("обычный стартер");

            assert!(current_node.weights.len() == 1);
            assert_eq!(current_node.weights, trie_node.weights);

            let weights = bake_weights(&trie_node.weights[0]) as u64;

            stats_codepoint!(stats, codepoint, trie_node);
            return encoded!(MARKER_STARTER_SINGLE_WEIGHTS, weights << 4);
        }
    }

    // кейс: первый и последний кодпоинт декомпозиции - стартеры
    // декомпозиция не является сокращением

    if first.is_starter() && last.is_starter() && first_trie.children.is_none() {
        assert!(last_trie.children.is_none());

        let stats = stats.touch("стартер, расширения");

        let weights = bake_weights_vec(trie_node.weights, false);
        let (len, pos) = bake_extra(&mut extra.weights, &weights);

        assert!(len <= 0x3F); // 6 бит
        assert!(pos <= 0xFFFF); // u16 достаточно

        stats_codepoint!(stats, codepoint, trie_node);
        return encoded!(MARKER_EXPANSION, len << 4, pos << 16);
    }

    // декомпозиция

    let stats = stats.touch("кодпоинт с декомпозицией");

    let mut description = "".to_owned();
    let mut baked_decomposition = vec![];

    for c in decomposition {
        let trie_node = encoder.trie.get(&c.code).unwrap();
        let baked = bake_trie(c.code, trie_node, true);

        baked_decomposition.extend(baked);
        description.push_str(format!("U+{:04X} ({}) ", c.code, c.ccc.u8()).as_str());
    }

    let pos = match find_subsequence(&baked_decomposition, &extra.decompositions) {
        Some(found) => found,
        None => {
            let pos = extra.decompositions.len();
            extra.decompositions.extend(baked_decomposition);

            pos
        }
    } as u64;

    let len = decomposition.len() as u64;

    assert!(pos < 0xFFFF);
    assert!(len < 0xFF);

    stats_codepoint!(stats, codepoint; description);
    encoded!(MARKER_DECOMPOSITION, pos << 4, len << 20)
}

/// вычисляемые веса
///
/// 0000 ____  ____ ____    ____ ____  ____ ____        ____ ____  ____ ____    ____ ____  _____ ____
///
fn implicit_weights(
    encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    _stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let decomposition = encoder.decomposition(codepoint.code);

    // чтобы уменьшить количество проверок, кодпоинты (Unified_Ideograph=True AND Block=CJK_Compatibility_Ideographs)
    // записаны в allkeys
    #[rustfmt::skip]
    blocking_checks!(
        extra.trie_node.is_some(),              // веса не заданы
        decomposition.is_some(),                // нет декомпозиции
        !is_implicit(codepoint.code, false)     // является вычисляемым значением
    );

    encoded!(0)
}

/// слоги хангыль
///
/// mmm_ ____  ____ ____    ____ ____  ____ ____        ____ ____  ____ ____    ____ ____  _____ ____
///
fn hangul_syllables(
    _encoder: &EncodeWeights,
    codepoint: &Codepoint,
    _extra: &mut AdditionalInfo,
    _stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    // TODO: возможно, сделать как-то по другому?

    blocking_checks!(!(0xAC00 ..= 0xD7A3).contains(&codepoint.code));

    encoded!(MARKER_HANGUL)
}

/// последовательности кодпоинтов
///
/// mmm_ iiii  iiii iiii    iiii ____  ____ ____        ____ ____  ____ ____    ____ ____  _____ ____
///
fn has_children(
    _encoder: &EncodeWeights,
    codepoint: &Codepoint,
    extra: &mut AdditionalInfo,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u64>>
{
    let stats = stats.touch("последовательности");
    let trie_node = trie_node_or_return!(extra.trie_node);

    blocking_checks!(
        trie_node.children.is_none() // кодпоинт является началом последовательности
    );

    let trie = bake_trie(codepoint.code, extra.trie_node.unwrap(), true);
    let (_, pos) = bake_extra(&mut extra.weights, &trie);

    assert!(pos <= 0xFFFF); // укладывается в u16

    stats_codepoint!(stats, codepoint);
    encoded!(MARKER_CHILDREN, pos << 4)
}

/// запечь последовательности весов
fn bake_trie(code: u32, node: &TrieNode, is_last: bool) -> Vec<u32>
{
    let mut data: Vec<u32> = vec![];

    // запечь кодпоинт:
    // FLcc cccc  nnnn nnnn    nnnn nnnn  nnww wwww
    //  - F - флаг наличия потомков
    //  - L - последний элемент текущего уровня
    //  - ccc - ССС в сжатом виде (индекс значения CCC) - 6 бит
    //  - n - кодпоинт - 18 бит
    //  - w - длина весов - 6 бит
    let bake_codepoint = |code: u32, has_children: bool, is_last: bool, w_len: u32| -> u32 {
        let ccc = get_ccc(&code).compressed() as u32;
        let has_children = has_children as u32;
        let is_last = is_last as u32;

        assert!(w_len <= 0x3F);

        has_children | (is_last << 1) | (ccc << 2) | (code << 8) | (w_len << 26)
    };

    let has_children = node.children_len() != 0;
    let entry = bake_codepoint(code, has_children, is_last, node.weights.len() as u32);
    let weights = bake_weights_vec(node.weights, false);

    data.push(entry);
    data.extend(weights);

    let children = match &node.children {
        Some(children) => children,
        None => return data,
    };

    // отсортируем потомков по CCC
    let mut sorted_keys: Vec<&u32> = children.keys().collect();
    sorted_keys.sort_by(|a, b| get_ccc(a).u8().cmp(&get_ccc(b).u8()));

    let last = sorted_keys.len() - 1;

    for (i, &code) in sorted_keys.iter().enumerate() {
        let node = &children[code];
        data.extend(bake_trie(*code, node, i == last));
    }

    data
}

/// количество всех узлов
#[allow(dead_code)]
fn trie_total_count(node: &TrieNode) -> usize
{
    let mut count = 1;

    if let Some(children) = &node.children {
        children.iter().for_each(|(_, child_node)| {
            count += trie_total_count(child_node);
        });
    }

    count
}

/// цепочка весов из их запечённых значений, где присутствует маркер продолжения цепочки
/// (в незадействованном старшем бите)
fn bake_weights_vec(weights: &Vec<Weights>, use_last_marker: bool) -> Vec<u32>
{
    let last = weights.len().saturating_sub(1);
    let use_last_marker = use_last_marker as u32;

    weights
        .iter()
        .enumerate()
        .map(|(i, w)| bake_weights(w) | ((((i != last) as u32) & use_last_marker) << 31))
        .collect()
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
