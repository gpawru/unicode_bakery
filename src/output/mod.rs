use std::fs::File;
use std::io::Write;

use crate::encode::normalization::composed_expansions::ComposedExpansions;
use crate::encode::normalization::compositions::BakedCompositions;
use crate::stats::EncodeCodepointStats;
use crate::tables::NormalizationTables;
use crate::tables::WeightsTables;

use format::format_num_vec;

pub mod format;

/// длина строки в файле с подготовленными данными
pub const FORMAT_STRING_LENGTH: usize = 120;

/// записать таблицы
pub fn write_normalization(
    classname: impl AsRef<str>,
    filename: impl AsRef<str>,
    tables: &NormalizationTables,
)
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let output = format!(
        "{} {{\n  \
            index: &[{}  ],\n  \
            data: &[{}  ],\n  \
            expansions: &[{}  ],\n  \
            continuous_block_end: 0x{:04X},\n\
        }}\n",
        classname.as_ref(),
        format_num_vec(tables.index.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.data.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.expansions.as_slice(), FORMAT_STRING_LENGTH),
        tables.continuous_block_end,
    );

    write!(file, "{}", output).unwrap();
}

/// записать композиции
pub fn write_compositions(
    classname: impl AsRef<str>,
    filename: impl AsRef<str>,
    compositions: &BakedCompositions,
)
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let output = format!(
        "{} {{\n  \
            compositions: &[{}  ],\n\
        }}\n",
        classname.as_ref(),
        format_num_vec(compositions.table.as_slice(), FORMAT_STRING_LENGTH),
    );

    write!(file, "{}", output).unwrap();
}

/// записать корректировки декомпозиций для NFC / NFKC
pub fn write_expansions(
    classname: impl AsRef<str>,
    filename: impl AsRef<str>,
    precomposed_expansions: &ComposedExpansions,
)
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let output = format!(
        "{} {{\n  \
            expansions: &[{}  ],\n\
        }}\n",
        classname.as_ref(),
        format_num_vec(
            precomposed_expansions.values.as_slice(),
            FORMAT_STRING_LENGTH
        ),
    );

    write!(file, "{}", output).unwrap();
}

/// записать статистику по кодпоинтам в таблицах нормализации / таблицах весов
pub fn write_stats(filename: impl AsRef<str>, stats: &EncodeCodepointStats)
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let mut headers: Vec<&String> = stats.blocks.keys().collect();
    headers.sort_by_key(|k| stats.blocks[*k].order);

    headers
        .iter()
        .map(|&k| (k, &stats.blocks[k]))
        .for_each(|(header, block)| {
            writeln!(file, "{}. {} ({})", block.order, header, block.count).unwrap();
        });

    writeln!(file).unwrap();

    headers
        .iter()
        .map(|&k| (k, &stats.blocks[k]))
        .for_each(|(header, block)| {
            write!(file, "{}. {} ({})\n\n", block.order, header, block.count).unwrap();

            let mut codes: Vec<&u32> = block.entries.keys().collect();
            codes.sort();

            codes
                .iter()
                .map(|&c| (c, &block.entries[c]))
                .for_each(|(code, description)| {
                    writeln!(file, "U+{:04X} - {}", code, description).unwrap();
                });

            writeln!(file).unwrap();
        });
}

/// записать веса
pub fn write_weights(classname: impl AsRef<str>, filename: impl AsRef<str>, tables: &WeightsTables)
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let output = format!(
        "{} {{\n  \
            index: &[{}  ],\n  \
            scalars32: &[{}  ],\n  \
            scalars64: &[{}  ],\n  \
            weights: &[{}  ],\n  \
            decompositions: &[{}  ],\n  \
            continuous_block_end: 0x{:04X},\n\
        }}\n",
        classname.as_ref(),
        format_num_vec(tables.index.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.scalars32.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.scalars64.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.weights.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.decompositions.as_slice(), FORMAT_STRING_LENGTH),
        tables.continuous_block_end,
    );

    write!(file, "{}", output).unwrap();
}
