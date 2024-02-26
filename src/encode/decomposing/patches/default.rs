pub struct DefaultPatch;

macro_rules! default_patch {
    ($t:ty) => {
        impl crate::encode::PatchTables<$t, u32> for DefaultPatch
        {
            /// индексы для пустых блоков должны ссылаться на первый пустой блок в таблице данных
            fn patch(&self, tables: &mut crate::tables::NormalizationTables<$t, u32>)
            {
                let empty_block = tables.index.iter().filter(|&e| *e != u32::MAX).find(|&i| {
                    let start = i * tables.block_size;
                    let end = (i + 1) * tables.block_size;

                    tables.data[start as usize .. end as usize]
                        .iter()
                        .all(|e| *e == 0)
                });

                // пустого блока нет - например, блоки большой
                let empty_block = match empty_block {
                    Some(&value) => value,
                    None => {
                        let empty_block = tables.data.len() as u32 / tables.block_size;
                        tables.data.extend(vec![0; tables.block_size as usize]);
                        empty_block
                    }
                };

                tables
                    .index
                    .iter_mut()
                    .filter(|e| **e == u32::MAX)
                    .for_each(|e| *e = empty_block);
            }
        }
    };
}

default_patch!(u64);
default_patch!(u32);
