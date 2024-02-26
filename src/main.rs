use encode::decomposing::default::DefaultPatch;
use encode::decomposing::hangul::HangulPatch;
use encode::decomposing::EncodeDecomposition;
use output::*;
use tables::NormalizationTables;

mod encode;
mod filter;
mod output;
mod stats;
mod tables;

fn main()
{
    let nfd = NormalizationTables::build(
        128,
        0xFFF,
        &EncodeDecomposition { is_canonical: true },
        Some(&[&DefaultPatch, &HangulPatch]),
        None,
    );

    write_normalization_tables("./data/nfd.rs.txt", &nfd);
    write_normalization_stats("./data_stats/nfd.stats.txt", &nfd.stats);
    write_normalization_blocks("./data_stats/nfd.blocks.txt", &nfd);

    println!("NFD: {} kb", nfd.size() / 1024);
}
