use unicode_data::UNICODE;

use crate::{
    encode::{EncodeCodepoint, EncodedCodepoint, PatchTables},
    filter::CodepointFilter,
    stats::EncodeCodepointStats,
};

/// последний кодпоинт таблицы с декомпозицией
pub const LAST_DECOMPOSITION_CODE: u32 = 0x2FA1D;

/// таблицы запечённых данных нормализации
pub struct NormalizationTables<'a, T, E>
{
    pub index: Vec<u32>,
    pub data: Vec<T>,
    pub expansions: Vec<E>,
    pub block_size: u32,
    pub continuous_block_end: u32,
    pub stats: EncodeCodepointStats,
    encoder: &'a dyn EncodeCodepoint<T, E>,
    filters: Option<&'a [&'a dyn CodepointFilter]>,
}

impl<'a, T, E> NormalizationTables<'a, T, E>
where
    T: Copy,
    E: Copy,
{
    /// создать таблицы для нормализации
    pub fn build(
        block_size: u32,
        continuous_block_end: u32,
        encoder: &'a dyn EncodeCodepoint<T, E>,
        patches: Option<&'a [&'a dyn PatchTables<T, E>]>,
        filters: Option<&'a [&'a dyn CodepointFilter]>,
    ) -> Self
    {
        assert!(block_size.is_power_of_two());

        let mut tables = Self {
            index: vec![],
            data: vec![],
            expansions: vec![],
            block_size,
            continuous_block_end,
            stats: EncodeCodepointStats::new(),
            encoder,
            filters,
        };

        tables.compose();

        if let Some(patches) = patches {
            patches.iter().for_each(|p| p.patch(&mut tables));
        }

        tables
    }

    /// заполнение таблиц
    fn compose(&mut self)
    {
        let last_block = LAST_DECOMPOSITION_CODE / self.block_size;

        for block_index in 0 ..= last_block {
            let block = self.compose_block(block_index, self.expansions.len());

            if block.iter().all(|e| e.is_none())
                && (block_index > (self.continuous_block_end / self.block_size))
            {
                self.index.push(u32::MAX);
                continue;
            }

            self.index.push(self.data.len() as u32 / self.block_size);

            block.iter().for_each(|entry| {
                let entry = match entry {
                    Some(entry) => entry,
                    None => self.encoder.default(),
                };

                self.data.push(entry.value);

                if let Some(expansions) = &entry.extra {
                    expansions.iter().for_each(|&e| self.expansions.push(e))
                }
            });
        }
    }

    /// запечь блок кодпоинтов
    fn compose_block(
        &mut self,
        index: u32,
        exp_position: usize,
    ) -> Vec<Option<EncodedCodepoint<T, E>>>
    {
        let base = index * self.block_size;
        let mut exp_position = exp_position;

        let block = (0 .. self.block_size)
            .map(|offset| match &UNICODE.get(&((base + offset) as u32)) {
                Some(codepoint) => {
                    if let Some(filters) = &self.filters {
                        if !filters.iter().all(|f| f.filter(codepoint)) {
                            return None;
                        }
                    }

                    match self
                        .encoder
                        .encode(codepoint, exp_position, &mut self.stats)
                    {
                        Some(encoded) => {
                            if let Some(extra) = &encoded.extra {
                                exp_position += extra.len();
                            }

                            Some(encoded)
                        }
                        None => None,
                    }
                }
                None => None,
            })
            .collect();

        block
    }

    /// размер данных. тип индекса при запекании не задаётся, поэтому предполагается
    // исходя из максимального значения
    pub fn size(&self) -> usize
    {
        let max_index = *self.index.iter().max().unwrap_or(&0);

        let index_size = match max_index {
            ..= 0xFF => 1,
            ..= 0xFFFF => 2,
            _ => 4,
        };

        self.index.len() * index_size
            + self.data.len() * core::mem::size_of::<T>()
            + self.expansions.len() * core::mem::size_of::<E>()
    }
}
