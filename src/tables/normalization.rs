use unicode_data::UNICODE;

use crate::encode::EncodeCodepoint;
use crate::stats::EncodeCodepointStats;

use super::LAST_DECOMPOSITION_CODE;

/// таблицы запечённых данных нормализации
pub struct NormalizationTables<'a>
{
    pub index: Vec<u16>,
    pub data: Vec<u32>,
    pub expansions: Vec<u32>,
    pub bits_total: u8,
    pub bits_big_block: u8,
    pub bits_small_block: u8,
    pub continuous_block_end: u32,
    pub stats: EncodeCodepointStats,
    encoder: &'a dyn EncodeCodepoint<u32, u32, Vec<u32>>,
}

impl<'a> NormalizationTables<'a>
{
    /// создать таблицы для нормализации
    pub fn build(
        bits_big_block: u8,
        bits_small_block: u8,
        continuous_block_end: u32,
        encoder: &'a dyn EncodeCodepoint<u32, u32, Vec<u32>>,
    ) -> Self
    {
        let mut tables = Self {
            index: vec![],
            data: vec![],
            expansions: vec![],
            bits_total: 18,
            bits_big_block,
            bits_small_block,
            continuous_block_end,
            stats: EncodeCodepointStats::new(),
            encoder,
        };

        tables.compose();

        tables
    }

    /// размер данных
    pub fn size(&self) -> usize
    {
        self.index.len() * 2 + self.data.len() * 4 + self.expansions.len() * 4
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
    fn compose(&mut self)
    {
        macro_rules! encode {
            ($codepoint:expr) => {
                match self
                    .encoder
                    .encode($codepoint, &mut self.expansions, &mut self.stats)
                {
                    Some(encoded) => encoded.value,
                    None => 0,
                }
            };
        }

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
            small_block.push(match UNICODE.get(&code) {
                Some(codepoint) => encode!(codepoint),
                None => 0,
            });

            if small_block.len() as u32 == self.small_block_size() {
                let small_block_index = match self.find_small_block(&small_block) {
                    Some(index) => index,
                    None => {
                        let index = self.data.len() as u32;
                        self.data.append(&mut small_block);
                        index
                    }
                };

                small_block.clear();
                big_block.push(small_block_index as u16);

                if big_block.len() as u32 == sb_count {
                    let existing_big_block = match code <= self.continuous_block_end {
                        true => None,
                        false => self.find_big_block(&big_block),
                    };

                    let index = match existing_big_block {
                        Some(index) => index,
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

    /// найти полностью совпадающий существующий большой блок (возвращает индекс, если блок найден)
    fn find_big_block(&self, find: &Vec<u16>) -> Option<u16>
    {
        let sb_count = self.small_blocks_in_big();
        let primary_sb = (self.primary_index_len() / sb_count) as usize;

        let blocks = self.index.len() / sb_count as usize;

        for block in primary_sb .. blocks {
            let start = block * sb_count as usize;
            let end = (block + 1) * sb_count as usize - 1;

            let left = &self.index[start ..= end];
            let right = find.as_slice();

            if left == right {
                return Some(block as u16 * sb_count as u16);
            }
        }

        None
    }

    /// найти полностью совпадающий существующий блок
    fn find_small_block(&self, find: &Vec<u32>) -> Option<u32>
    {
        assert_eq!(find.len() as u32, self.small_block_size());

        let blocks = self.data.len() as u32 / self.small_block_size();

        for block in 0 .. blocks {
            let start = (block * self.small_block_size()) as usize;
            let end = ((block + 1) * self.small_block_size() - 1) as usize;

            let left = &self.data[start ..= end];
            let right = find.as_slice();

            if left == right {
                return Some(block * self.small_block_size());
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

impl core::fmt::Debug for NormalizationTables<'_>
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
