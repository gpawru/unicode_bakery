#[macro_export]
macro_rules! tables {
    ($name:expr, $encoder:ident, $bsize:expr, $cont:expr, $can:expr, $patches:expr, $filters:expr) => {
        let tables = NormalizationTables::build(
            $bsize,
            $cont,
            &$encoder { is_canonical: $can },
            $patches,
            $filters,
        );

        write_normalization_tables(format!("./data/{}.rs.txt", $name), &tables);
        write_normalization_stats(format!("./data_stats/{}.stats.txt", $name), &tables.stats);
        write_normalization_blocks(format!("./data_stats/{}.blocks.txt", $name), &tables);

        println!("{}: {} kb", $name.to_uppercase(), tables.size() / 1024);
    };
}
