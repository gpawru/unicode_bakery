use unicode_data::{codepoint::Codepoint, UNICODE};

use super::{EncodeCodepoint, EncodeCodepointStats, EncodedCodepoint};
use crate::stats;

/// стартер без декомпозиции
pub const MARKER_STARTER: u32 = 0b_000;
/// нестартер без декомпозиции
pub const MARKER_NONSTARTER: u32 = 0b_001;
/// синглтон
pub const MARKER_SINGLETON: u32 = 0b_010;
/// декомпозиция, вынесенная во внешний блок
pub const MARKER_EXPANSION: u32 = 0b_011;
/// слог хангыль
pub const MARKER_HANGUL: u32 = 0b_100;

pub struct EncodeDecomposition32
{
    pub is_canonical: bool,
}

impl EncodeCodepoint<u32, u32> for EncodeDecomposition32
{
    fn encode(
        &self,
        codepoint: &Codepoint,
        exp_position: usize,
        stats: &mut stats::EncodeCodepointStats,
    ) -> Option<EncodedCodepoint<u32, u32>>
    {
        let decomposition = match self.is_canonical {
            true => &codepoint.canonical_decomposition,
            false => &codepoint.compat_decomposition,
        };

        let variants = &[
            starter,
            nonstarter,
            singleton,
            pair16,
            starters_to_nonstarters,
            nonstarter_decomposition,
            starters_start_end,
            other,
        ];

        let value = variants
            .iter()
            .find_map(|f| f(codepoint, decomposition, exp_position, stats));

        if value.is_none() {
            panic!(
                "не определили тип хранения кодпоинта: U+{:04X} - {} ({})",
                codepoint.code,
                codepoint.name,
                codepoint.ccc.u8(),
            );
        };

        let value = value.unwrap();

        match (value.value as u8) == (MARKER_STARTER as u8) {
            true => None,
            false => Some(value),
        }
    }

    fn default(&self) -> &EncodedCodepoint<u32, u32>
    {
        &EncodedCodepoint {
            value: MARKER_STARTER as u32,
            extra: None,
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
    ($marker:expr, $value:expr, $expansion:expr; $stats:expr, $codepoint:expr, $description:expr) => {{
        let description = match $description.is_empty() {
            true => format!("{}", $codepoint.name),
            false => format!("{} : {}", $codepoint.name, $description),
        };

        $stats.inc($codepoint.code, description);
        Some(EncodedCodepoint {
            value: ($marker as u32) | ($value as u32),
            extra: $expansion,
        })
    }};
    ($marker:expr, $value:expr, $expansion:expr; $stats:expr, $codepoint:expr) => {{
        encoded!($marker, $value, $expansion; $stats, $codepoint, &"")
    }};
}

macro_rules! codepoint_values {
    ($code:expr) => {{
        let codepoint = &UNICODE[&$code];
        let ccc = codepoint.ccc.compressed() as u32;
        (codepoint, $code, ccc)
    }};
}

macro_rules! expansion {
    ($decomposition:expr) => {{
        let mut expansion = vec![];
        let mut description = String::new();

        $decomposition.iter().for_each(|c| {
            let codepoint = &UNICODE[c];

            expansion.push((codepoint.code << 8) | (codepoint.ccc.compressed() as u32));
            description
                .push_str(format!("U+{:04X} ({}) ", codepoint.code, codepoint.ccc.u8()).as_str());
        });

        (expansion, description)
    }};
}

/// обычный стартер без декомпозиции
///
/// mmmm mmmm  ____ ____    ____ ____  ____ ____
///
fn starter(
    codepoint: &Codepoint,
    decomposition: &Vec<u32>,
    _exp_position: usize,
    _stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32, u32>>
{
    blocking_checks!(codepoint.is_nonstarter(), !decomposition.is_empty());

    Some(EncodedCodepoint {
        value: MARKER_STARTER as u32,
        extra: None,
    })
}

/// нестартер
///
/// mmmm mmmm  cccc ccxx    xxxx xxxx  xxxx xxxx
///
fn nonstarter(
    codepoint: &Codepoint,
    decomposition: &Vec<u32>,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32, u32>>
{
    let se = stats.touch("нестартер");

    blocking_checks!(codepoint.is_starter(), !decomposition.is_empty());

    let c0 = codepoint.code as u64;
    let c0_ccc = codepoint.ccc.compressed() as u64;

    let description = format!("({})", codepoint.ccc.u8());

    encoded!(
        MARKER_NONSTARTER,
        (c0_ccc << 8) | (c0 << 14),
        None;
        se,
        codepoint,
        description
    )
}

/// синглтон
///
/// mmmm mmmm  0000 00xx    xxxx xxxx  xxxx xxxx
///
fn singleton(
    codepoint: &Codepoint,
    decomposition: &Vec<u32>,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32, u32>>
{
    let se = stats.touch("синглтон");

    blocking_checks!(
        codepoint.is_nonstarter(),
        decomposition.len() != 1,
        UNICODE[&decomposition[0]].is_nonstarter()
    );

    let c0 = decomposition[0];

    encoded!(MARKER_SINGLETON, c0 << 14, None; se, codepoint)
}

/// пара (первый кодпоинт - стартер)
///
/// xxxx xxxx  xxxx xxxx    yyyy yyyy  yyyy yyyy
///
fn pair16(
    codepoint: &Codepoint,
    decomposition: &Vec<u32>,
    _exp_position: usize,
    stats: &mut EncodeCodepointStats,
) -> Option<EncodedCodepoint<u32, u32>>
{
    let se = stats.touch("пара");

    blocking_checks!(
        codepoint.is_nonstarter(),
        decomposition.len() != 2,
        UNICODE[&decomposition[0]].is_nonstarter(),
        decomposition.iter().any(|&c| c > 0xFFFF),
        (decomposition[0] as u8) < (MARKER_HANGUL as u8)
    );

    let c0 = decomposition[0] as u32;
    let (c1_codepoint, c1, _) = codepoint_values!(decomposition[1]);

    let description = format!("U+{:04X} (0) U+{:04X} ({})", c0, c1, c1_codepoint.ccc.u8());

    encoded!(
        0,
        c0 | (c1 << 16),
        None;
        se,
        codepoint,
        description
    )
}

macro_rules! fn_expansion {
    ($fn:ident, $description:expr, $codepoint:ident, $decomposition: ident; $($expr: expr),+) => {
        #[doc = $description]
        #[doc = ""]
        #[doc = "mmmm mmmm  nnnn nnnn    iiii iiii  iiii iiii"]
        fn $fn(
            $codepoint: &Codepoint,
            $decomposition: &Vec<u32>,
            exp_position: usize,
            stats: &mut EncodeCodepointStats,
        ) -> Option<EncodedCodepoint<u32, u32>>
        {
            let se = stats.touch($description);

            if $($expr ||)+ false {
                return None;
            }

            let n = $decomposition.len() as u32;
            let p = exp_position as u32;
            let (expansion, description) = expansion!($decomposition);

            encoded!(MARKER_EXPANSION, (n << 8) | (p << 16), Some(expansion); se, $codepoint, description)
        }
    }
}

fn_expansion!(
    starters_to_nonstarters,
    "стартер в нестартеры",
    codepoint, decomposition;

    codepoint.is_nonstarter(),
    decomposition.iter().any(|c| UNICODE[c].is_starter())
);

fn_expansion!(
    nonstarter_decomposition,
    "декомпозиция нестартера",
    codepoint, decomposition;

    codepoint.is_starter(),
    decomposition.iter().any(|c| UNICODE[c].is_starter())
);

fn_expansion!(
    starters_start_end,
    "первый и последний - стартеры",
    codepoint, decomposition;

    codepoint.is_nonstarter(),
    decomposition.len() < 2,
    UNICODE[&decomposition[0]].is_nonstarter(),
    UNICODE[&decomposition.last().unwrap()].is_nonstarter()
);

fn_expansion!(
    other,
    "прочие кейсы",
    codepoint, decomposition;

    codepoint.is_nonstarter(),
    decomposition.len() < 2
);
