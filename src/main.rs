#![allow(dead_code)]
#![allow(unused)]

#[macro_use]
extern crate lazy_static;

use encode::normalization::compositions::BakedCompositions;
use encode::normalization::EncodeNormalization;
use output::*;
use tables::NormalizationTables;

use crate::encode::normalization::composed_expansions::ComposedExpansions;
use crate::tables::WeightsTables;

mod encode;
mod output;
mod stats;
mod tables;

fn main()
{
    // let nfd_encoder = EncodeNormalization::new(true);
    // let nfd = NormalizationTables::build(11, 3, 0xFFF, &nfd_encoder);

    // let nfkd_encoder = EncodeNormalization::new(false);
    // let nfkd = NormalizationTables::build(11, 3, 0xFFF, &nfkd_encoder);

    // let compositions = BakedCompositions::new();
    // let nfc_expansions = ComposedExpansions::new(true);
    // let nfkc_expansions = ComposedExpansions::new(false);

    // write_normalization("DecompositionData", "./data/nfd.txt", &nfd);
    // write_normalization("DecompositionData", "./data/nfkd.txt", &nfkd);

    // write_compositions("CompositionData", "./data/compositions.txt", &compositions);

    // write_expansions("ExpansionsPatch", "./data/nfc.txt", &nfc_expansions);
    // write_expansions("ExpansionsPatch", "./data/nfkc.txt", &nfkc_expansions);

    // write_stats("./data_stats/nfd.txt", &nfd.stats);
    // write_stats("./data_stats/nfkd.txt", &nfkd.stats);

    // println!("NFD: {} Kb", nfd.size() / 1024);
    // println!("NFKD: {} Kb", nfkd.size() / 1024);
    // println!("Compositions: {} Kb", compositions.size() / 1024);
    // println!("NFC expansions: {} b", nfc_expansions.size());
    // println!("NFKC expansions: {} b", nfkc_expansions.size());

    let cldr_und = WeightsTables::build(11, 3, 0xFFF, true, &unicode_data::CLDR_UND_TRIE);

    write_weights("WeightsData", "./data/cldr_und.txt", &cldr_und);
    write_stats("./data_stats/cldr_und.txt", &cldr_und.stats);

    println!("CLDR UND: {} Kb", cldr_und.size() / 1024);
}
