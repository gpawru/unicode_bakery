use encode::decomposing::patches::default::DefaultPatch;
use encode::decomposing::patches::hangul::HangulPatch;
use encode::decomposing::EncodeDecomposition;
use output::*;
use tables::NormalizationTables;

use crate::encode::decomposing::u32::EncodeDecomposition32;

mod encode;
mod filter;
mod macros;
mod output;
mod stats;
mod tables;

fn main()
{
    // NFD
    tables!(
        "nfd",
        EncodeDecomposition,
        128,
        0xFFF,
        true,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFKD
    tables!(
        "nfkd",
        EncodeDecomposition,
        128,
        0xFFF,
        false,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFD
    tables!(
        "nfd.u32",
        EncodeDecomposition32,
        128,
        0xFFF,
        true,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFKD
    tables!(
        "nfkd.u32",
        EncodeDecomposition32,
        128,
        0xFFF,
        false,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );
}
