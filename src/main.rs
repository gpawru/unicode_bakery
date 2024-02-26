use encode::decomposing::default::DefaultPatch;
use encode::decomposing::hangul::HangulPatch;
use encode::decomposing::EncodeDecomposition;
use output::*;
use tables::NormalizationTables;

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
        128,
        0xFFF,
        true,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFKD
    tables!(
        "nfkd",
        128,
        0xFFF,
        false,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );
}
