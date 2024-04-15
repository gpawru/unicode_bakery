use std::collections::HashMap;

use unicode_data::codepoint::*;
use unicode_data::{TrieNode, UNICODE};

use crate::encode::weights::AdditionalInfo;
use crate::encode::weights::EncodeWeights;
use crate::encode::{EncodeCodepoint, EncodedCodepoint};
use crate::stats::EncodeCodepointStats;

use super::LAST_DECOMPOSITION_CODE;

/// таблицы запечённых данных весов
pub struct WeightsTables
{
    pub index: Vec<u16>,
    pub scalars32: Vec<u32>,
    pub scalars64: Vec<u64>,
    pub weights: Vec<u32>,
    pub bits_total: u8,
    pub bits_big_block: u8,
    pub bits_small_block: u8,
    pub continuous_block_end: u32,
    pub stats: EncodeCodepointStats,
}

impl WeightsTables
{
    /// создать таблицы
    pub fn build(
        bits_big_block: u8,
        bits_small_block: u8,
        continuous_block_end: u32,
        is_canonical: bool,
        weights_map: &HashMap<u32, TrieNode>,
    ) -> Self
    {
        let encoder = EncodeWeights::new(is_canonical, weights_map);

        let mut tables = Self {
            index: vec![],
            scalars32: vec![],
            scalars64: vec![],
            weights: vec![],
            bits_total: 18,
            bits_big_block,
            bits_small_block,
            continuous_block_end,
            stats: EncodeCodepointStats::new(),
        };

        tables.compose(&encoder);

        tables
    }

    /// размер данных
    pub fn size(&self) -> usize
    {
        self.index.len() * 2
            + self.scalars32.len() * 4
            + self.scalars64.len() * 8
            + self.weights.len() * 4
    }

    /// позиция записи кодпоинта в data
    #[allow(dead_code)]
    pub fn get_codepoint_data_position(&self, code: u32) -> Option<u16>
    {
        let big_block = code >> (self.bits_total - self.bits_big_block);

        // последний большой блок последовательного блока
        let big_block_base = match big_block < self.continuous_big_blocks_count() {
            true => (self.primary_index_len() + (big_block * self.small_blocks_in_big())) as u16,
            false => {
                if big_block >= self.big_block_count() {
                    return None;
                }

                self.index[big_block as usize]
            }
        };

        let middle_and_small_mask = (1 << (self.bits_total - self.bits_big_block)) - 1;
        let middle_and_small = code & middle_and_small_mask;
        let middle_index = (middle_and_small >> self.bits_small_block) as u16;

        let secondary_index = big_block_base | middle_index;

        let small_mask = (1 << self.bits_small_block) - 1;
        let index = self.index[secondary_index as usize] | (code & small_mask) as u16;

        Some(index)
    }

    /// заполнение таблиц
    fn compose(&mut self, encoder: &EncodeWeights)
    {
        let bb_size = self.big_block_size();
        let bb_count = self.big_block_count();
        let sb_count = self.small_blocks_in_big();

        // место под индексы больших блоков

        self.index
            .extend(vec![0; self.primary_index_len() as usize]);

        // заполняем

        let mut big_block = vec![];
        let mut small_block = vec![];

        for code in 0 .. bb_count * bb_size {
            small_block.push(match self.encode(code, encoder) {
                Some(encoded) => encoded.value,
                None => 0,
            });

            if small_block.len() as u32 == self.small_block_size() {
                let is_u32 = small_block.iter().all(|e| (e >> 32) == 0);

                let small_block_index = if is_u32 {
                    let small_block = small_block.iter().map(|&e| e as u32).collect::<Vec<u32>>();

                    let index = match self.find_block(&small_block, &self.scalars32) {
                        Some(index) => index,
                        None => {
                            let index = self.scalars32.len() as u32;
                            self.scalars32.extend(&small_block);
                            index
                        }
                    };

                    assert!(index < 0x7FFF);
                    index << 1
                } else {
                    let index = match self.find_block(&small_block, &self.scalars64) {
                        Some(index) => index,
                        None => {
                            let index = self.scalars64.len() as u32;
                            self.scalars64.extend(&small_block);
                            index
                        }
                    };

                    assert!(index < 0x7FFF);
                    (index << 1) | 1
                };

                small_block.clear();

                small_block.clear();
                big_block.push(small_block_index as u16);

                if big_block.len() as u32 == sb_count {
                    let existing_big_block = match code <= self.continuous_block_end {
                        true => None,
                        false => self.find_block(&big_block, &self.index),
                    };

                    let index = match existing_big_block {
                        Some(index) => index as u16,
                        None => {
                            let index = self.index.len() as u16;
                            self.index.append(&mut big_block);

                            index
                        }
                    };

                    big_block.clear();

                    let current_block = (code / bb_size) as usize;

                    self.index[current_block] = index;
                }
            }
        }
    }

    /// закодировать кодпоинт
    fn encode(&mut self, code: u32, encoder: &EncodeWeights) -> Option<EncodedCodepoint<u64>>
    {
        let weights_entry = encoder.trie.get(&code);
        let codepoint = UNICODE.get(&code);

        // кодпоинт не задан вообще -> implicit weights (unknown)
        if weights_entry.is_none() && codepoint.is_none() {
            return None;
        }

        // если кодпоинт не найден, то это означает только вариант с U+FFFE или U+FFFF
        let codepoint = match weights_entry.is_some() && codepoint.is_none() {
            true => {
                assert!(code == 0xFFFE || code == 0xFFFF);
                Codepoint {
                    code,
                    name: format!("U+{:04X}", code),
                    gc: GeneralCategory::Unassigned,
                    ccc: CanonicalCombiningClass::NotReordered,
                    bc: BidiClass::OtherNeutral,
                    numeric: NumericType::None,
                    bidi_mirrored: BidiMirrored::try_from("N").unwrap(),
                    simple_uppercase_mapping: SimpleCaseMapping::None,
                    simple_lowercase_mapping: SimpleCaseMapping::None,
                    simple_titlecase_mapping: SimpleCaseMapping::None,
                    decomposition_tag: None,
                    decomposition: vec![],
                    block: None,
                }
            }
            false => codepoint.unwrap().clone(),
        };

        // не заданы веса - вычисляемые веса или слоги хангыль
        if weights_entry.is_none() {
            assert!(
                [
                    "Hangul Syllable",
                    "CJK Ideograph",
                    "Tangut",
                    "Nushu",
                    "Khitan"
                ]
                .iter()
                .any(|variant| {
                    codepoint
                        .name
                        .to_lowercase()
                        .contains(&variant.to_lowercase())
                }),
                "U+{:04X} - {}",
                codepoint.code,
                codepoint.name
            );
        }

        let mut extra = AdditionalInfo {
            trie_node: weights_entry,
            weights: &mut self.weights,
        };

        encoder.encode(&codepoint, &mut extra, &mut self.stats)
    }

    /// найти полностью совпадающий существующий блок
    fn find_block<T: PartialEq>(&self, find: &Vec<T>, source: &Vec<T>) -> Option<u32>
    {
        for (i, window) in source.windows(find.len()).enumerate() {
            if window == find {
                return Some(i as u32);
            }
        }

        None
    }

    /// размер первичного индекса
    /// (количество больших блоков + некоторое пустое место для выравнивания адреса следующих блоков)
    fn primary_index_len(&self) -> u32
    {
        let bb_count = self.big_block_count();
        let sb_count = self.small_blocks_in_big();

        bb_count + (sb_count - bb_count % sb_count) % sb_count
    }

    /// кол-во кодпоинтов в большом блоке
    fn big_block_size(&self) -> u32
    {
        1 << (self.bits_total - self.bits_big_block)
    }

    /// количество больших блоков, необходимых для таблицы
    fn big_block_count(&self) -> u32
    {
        (LAST_DECOMPOSITION_CODE + self.big_block_size() - 1) / self.big_block_size()
    }

    /// количество кодпоинтов в маленьком блоке
    fn small_block_size(&self) -> u32
    {
        1 << self.bits_small_block
    }

    /// количество маленьких блоков в большом блоке
    fn small_blocks_in_big(&self) -> u32
    {
        self.big_block_size() / self.small_block_size()
    }

    /// количество больших блоков, занимаемых непрерывным стартовым блоком
    fn continuous_big_blocks_count(&self) -> u32
    {
        (self.continuous_block_end + self.big_block_size() - 1) / self.big_block_size()
    }
}

impl core::fmt::Debug for WeightsTables
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        writeln!(f, "{{")?;

        writeln!(
            f,
            "  big block size: {}, count: {}",
            self.big_block_size(),
            self.big_block_count()
        )?;

        writeln!(
            f,
            "  small block size: {}, in block: {}",
            self.small_block_size(),
            self.small_blocks_in_big()
        )?;

        writeln!(
            f,
            "  continuous big blocks: {}",
            self.continuous_big_blocks_count()
        )?;

        writeln!(f, "}}")?;

        Ok(())
    }
}
