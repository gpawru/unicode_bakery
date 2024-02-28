use unicode_data::codepoint::Codepoint;

#[macro_export]
macro_rules! decomposition_tables {
    ($name:expr, $encoder:ident, $bsize:expr, $cont:expr, $can:expr, $patches:expr, $filters:expr) => {
        let tables = NormalizationTables::build(
            $bsize,
            $cont,
            &$encoder { is_canonical: $can },
            $patches,
            $filters,
        );

        write_normalization_tables(
            "DecompositionData",
            format!("./data/{}.rs.txt", $name),
            &tables,
            "",
        );
        write_normalization_stats(format!("./data_stats/{}.stats.txt", $name), &tables.stats);
        write_normalization_blocks(format!("./data_stats/{}.blocks.txt", $name), &tables);

        println!("{}: {} kb", $name.to_uppercase(), tables.size() / 1024);
    };
}

#[macro_export]
macro_rules! composition_tables {
    ($name:expr, $encoder:expr, $bsize:expr, $cont:expr, $patches:expr, $filters:expr) => {
        let encoder = $encoder;
        let tables = NormalizationTables::build($bsize, $cont, &encoder, $patches, $filters);

        let table = crate::output::format_num_vec(
            encoder.composition_table.as_slice(),
            crate::output::FORMAT_STRING_LENGTH,
        );

        let extra = format!("compositions: &[{}  ],\n  ", table);

        write_normalization_tables(
            "CompositionData",
            format!("./data/{}.rs.txt", $name),
            &tables,
            extra,
        );
        write_normalization_stats(format!("./data_stats/{}.stats.txt", $name), &tables.stats);
        write_normalization_blocks(format!("./data_stats/{}.blocks.txt", $name), &tables);

        println!(
            "{}: {} kb",
            $name.to_uppercase(),
            (tables.size() + encoder.composition_table.len() * 8) / 1024
        );
    };
}

#[macro_export]
macro_rules! blocking_checks {
    ($($expr: expr),+) => {
        if $($expr ||)+ false {
            return None;
        }
    };
}

/// запекание последовательностей кодпоинтов + строка их описаний
#[macro_export]
macro_rules! expansion {
    ($decomposition:expr) => {{
        let mut expansion = vec![];
        let mut description = String::new();

        $decomposition.iter().for_each(|codepoint| {
            expansion.push((codepoint.code << 8) | (codepoint.ccc.compressed() as u32));
            description
                .push_str(format!("U+{:04X} ({}) ", codepoint.code, codepoint.ccc.u8()).as_str());
        });

        (expansion, description)
    }};
}

/// строка, описывающая прекомпозицию, состоящая из символов s и n, где s - стартер, n - нестартер
pub fn starters_map(codepoints: &Vec<Codepoint>) -> String
{
    codepoints
        .iter()
        .map(|c| match c.is_starter() {
            true => 's',
            false => 'n',
        })
        .collect()
}
