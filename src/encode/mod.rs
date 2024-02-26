use unicode_data::codepoint::Codepoint;

use crate::{stats::EncodeCodepointStats, tables::NormalizationTables};

pub mod composing;
pub mod decomposing;

/// закодированный кодпоинт
#[derive(Debug, Clone)]
pub struct EncodedCodepoint<T, E>
{
    /// данные
    pub value: T,
    /// дополнительные данные
    pub extra: Option<Vec<E>>,
}

pub trait EncodeCodepoint<T, E>
{
    /// закодировать кодпоинт
    fn encode(
        &self,
        codepoint: &Codepoint,
        exp_position: usize,
        stats: &mut EncodeCodepointStats,
    ) -> Option<EncodedCodepoint<T, E>>;

    /// значение по умолчанию для пропускаемых элементов
    fn default(&self) -> &EncodedCodepoint<T, E>;
}

pub trait PatchTables<T, E>
{
    /// применить патч к сформированной таблице кодпоинтов
    fn patch(&self, tables: &mut NormalizationTables<T, E>);
}
