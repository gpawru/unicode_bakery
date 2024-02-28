use encode::decomposing::patches::default::DefaultPatch;
use encode::decomposing::patches::hangul::HangulPatch;
use encode::decomposing::EncodeDecomposition;
use output::*;
use tables::NormalizationTables;

use crate::encode::composing::EncodeComposition;
use crate::encode::decomposing::u32::EncodeDecomposition32;

mod common;
mod encode;
mod filter;
mod output;
mod stats;
mod tables;

fn main()
{
    // NFD
    decomposition_tables!(
        "nfd",
        EncodeDecomposition,
        128,
        0xFFF,
        true,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFKD
    decomposition_tables!(
        "nfkd",
        EncodeDecomposition,
        128,
        0xFFF,
        false,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFD
    decomposition_tables!(
        "nfd.u32",
        EncodeDecomposition32,
        128,
        0xFFF,
        true,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFKD
    decomposition_tables!(
        "nfkd.u32",
        EncodeDecomposition32,
        128,
        0xFFF,
        false,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFC
    composition_tables!(
        "nfc",
        EncodeComposition::new(true),
        128,
        0xFFF,
        Some(&[&DefaultPatch]),
        None
    );

    // // NFKC
    composition_tables!(
        "nfkc",
        EncodeComposition::new(false),
        128,
        0xFFF,
        Some(&[&DefaultPatch]),
        None
    );
}
