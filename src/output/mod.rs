use std::fmt::UpperHex;
use std::fs::File;
use std::io::Write;

use unicode_data::UNICODE;

use crate::stats::EncodeCodepointStats;
use crate::tables::NormalizationTables;

use self::format::format_num_vec;

pub mod format;

/// длина строки в файле с подготовленными данными
const FORMAT_STRING_LENGTH: usize = 120;

/// записать таблицы нормализации
pub fn write_normalization_tables<T, E>(
    filename: impl AsRef<str>,
    tables: &NormalizationTables<T, E>,
) where
    T: UpperHex + Into<u64> + Copy,
    E: UpperHex + Into<u64> + Copy,
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let output = format!(
        "DecompositionData {{\n  \
            index: &[{}  ],\n  \
            data: &[{}  ],\n  \
            expansions: &[{}  ],\n  \
            continuous_block_end: 0x{:04X},\n\
        }}\n",
        format_num_vec(tables.index.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.data.as_slice(), FORMAT_STRING_LENGTH),
        format_num_vec(tables.expansions.as_slice(), FORMAT_STRING_LENGTH),
        tables.continuous_block_end,
    );

    write!(file, "{}", output).unwrap();
}

/// записать статистику таблиц нормализации
pub fn write_normalization_stats(filename: impl AsRef<str>, stats: &EncodeCodepointStats)
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let mut headers: Vec<&String> = stats.blocks.keys().collect();
    headers.sort_by_key(|k| stats.blocks[*k].order);

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

/// записать кодпоинты блоков таблицы норализации
pub fn write_normalization_blocks<T, E>(
    filename: impl AsRef<str>,
    tables: &NormalizationTables<T, E>,
) where
    T: Into<u64> + Copy,
{
    let mut file = File::create(filename.as_ref()).unwrap();

    let empty_block_index = *tables
        .index
        .iter()
        .find(|&&b| {
            tables.data[(b * tables.block_size) as usize .. ((b + 1) * tables.block_size) as usize]
                .iter()
                .all(|&e| e.into() == 0)
        })
        .unwrap();

    tables
        .index
        .iter()
        .enumerate()
        .for_each(|(index, &data_index)| {
            if data_index == empty_block_index {
                return;
            }

            let start = data_index * tables.block_size;
            let end = (data_index + 1) * tables.block_size;

            if tables.data[start as usize .. end as usize]
                .iter()
                .all(|&e| e.into() == 0)
            {
                assert!(end - 1 <= tables.continuous_block_end);
                return;
            }

            writeln!(file, "#{} -> 0x{:02X}\n", index, data_index).unwrap();

            for offset in 0 .. tables.block_size {
                if tables.data[(start + offset) as usize].into() == 0 {
                    continue;
                }

                let code = (index as u32) * tables.block_size + offset as u32;

                match UNICODE.get(&code) {
                    Some(codepoint) => {
                        writeln!(file, "U+{:04X} - {}", codepoint.code, codepoint.name).unwrap()
                    }
                    None => writeln!(file, "U+{:04X}", code).unwrap(),
                };
            }

            writeln!(file).unwrap();
        })
}
