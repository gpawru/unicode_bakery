use super::{PatchTables, MARKER_HANGUL};

const HANGUL_SYLLABLES_FIRST: u32 = 0xAC00;
const HANGUL_SYLLABLES_LAST: u32 = 0xD7A3;

pub struct HangulPatch;

impl PatchTables<u64, u32> for HangulPatch
{
    /// слоги хангыль будут ссылаться на блоки в конце таблицы данных
    fn patch(&self, tables: &mut super::NormalizationTables<u64, u32>)
    {
        let empty_block_index = tables.index.iter().find(|&&b| {
            tables.data[(b * tables.block_size) as usize .. ((b + 1) * tables.block_size) as usize]
                .iter()
                .all(|e| *e == 0)
        });

        let empty_block_index = match empty_block_index {
            Some(&value) => value,
            None => panic!("не нашли пустой блок"),
        };

        let first_block_index = HANGUL_SYLLABLES_FIRST / tables.block_size;
        let last_block_index = HANGUL_SYLLABLES_LAST / tables.block_size;

        let mut all_hangul_block_index = 0;

        for current_block_index in first_block_index ..= last_block_index {
            // формируем текущий блок
            let mut block = vec![0; tables.block_size as usize];

            block.iter_mut().enumerate().for_each(|(offset, e)| {
                let code = (current_block_index * tables.block_size) + offset as u32;

                if code >= HANGUL_SYLLABLES_FIRST && code <= HANGUL_SYLLABLES_LAST {
                    *e = MARKER_HANGUL;
                }
            });

            let data_index = tables.index[current_block_index as usize];

            // патчим существующий блок
            if data_index != empty_block_index {
                block.iter().enumerate().for_each(|(offset, &e)| {
                    if e == 0 {
                        return;
                    }

                    tables.data[(data_index * tables.block_size) as usize + offset] = e;
                });

                continue;
            };

            // индекс ссылается на пустой блок, но не все кодпоинты - хангыль
            if block.contains(&0) {
                tables.index[current_block_index as usize] =
                    tables.data.len() as u32 / tables.block_size;

                tables.data.extend(block);

                continue;
            }

            // создаём блок, состоящий только из слогов хангыль в data
            if all_hangul_block_index == 0 {
                all_hangul_block_index = tables.data.len() as u32 / tables.block_size;
                tables.data.extend(block);
            }

            // все кодпоинты блока - хангыль, ссылаемся на один и тот же блок в data
            tables.index[current_block_index as usize] = all_hangul_block_index;
        }
    }
}
