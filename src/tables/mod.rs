/// последний кодпоинт таблицы с декомпозицией
pub const LAST_DECOMPOSITION_CODE: u32 = 0x2FA1D;

mod normalization;
mod weights;

pub use normalization::NormalizationTables;
pub use weights::WeightsTables;