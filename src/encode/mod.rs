use crate::stats::EncodeCodepointStats;
use unicode_data::codepoint::Codepoint;

pub mod normalization;
pub mod weights;

/// закодированный кодпоинт
#[derive(Debug, Clone)]
pub struct EncodedCodepoint<T>
{
    /// данные
    pub value: T,
}

impl<T> EncodedCodepoint<T>
{
    pub fn new(value: T) -> Self
    {
        Self { value }
    }
}

impl<T> Default for EncodedCodepoint<T>
where
    T: From<u8>,
{
    fn default() -> Self
    {
        Self { value: T::from(0) }
    }
}

pub trait EncodeCodepoint<T, E, X>
{
    /// закодировать кодпоинт
    fn encode(
        &self,
        codepoint: &Codepoint,
        extra: &mut X,
        stats: &mut EncodeCodepointStats,
    ) -> Option<EncodedCodepoint<T>>;
}
