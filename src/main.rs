use encode::decomposing::patches::default::DefaultPatch;
use encode::decomposing::patches::hangul::HangulPatch;
use output::*;
use tables::NormalizationTables;

use encode::composing::u32::EncodeComposition32;
use encode::composing::EncodeComposition;
use encode::decomposing::u32::EncodeDecomposition32;
use encode::decomposing::EncodeDecomposition;

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
        "nfd.u64",
        EncodeDecomposition,
        128,
        0xFFF,
        true,
        Some(&[&DefaultPatch, &HangulPatch]),
        None
    );

    // NFKD
    decomposition_tables!(
        "nfkd.u64",
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
        "nfc.u64",
        EncodeComposition::new(true),
        128,
        0xFFF,
        Some(&[&DefaultPatch]),
        None
    );

    // // NFKC
    composition_tables!(
        "nfkc.u64",
        EncodeComposition::new(false),
        128,
        0xFFF,
        Some(&[&DefaultPatch]),
        None
    );

    // NFC
    composition_tables!(
        "nfc.u32",
        EncodeComposition32::new(true),
        128,
        0xFFF,
        Some(&[&DefaultPatch]),
        None
    );

    // // NFKC
    composition_tables!(
        "nfkc.u32",
        EncodeComposition32::new(false),
        128,
        0xFFF,
        Some(&[&DefaultPatch]),
        None
    );
}
