use std::collections::HashMap;

use unicode_data::{codepoint::Codepoint, NFC, NFD, NFKC, NFKD};

use crate::tables::LAST_DECOMPOSITION_CODE;

lazy_static! {
    /// различия NFC
    pub static ref EXPANSIONS_NFC: ComposedExpansions = ComposedExpansions::new(true);

    /// таблица декомпозиций NFKD
    pub static ref EXPANSIONS_NFKC: ComposedExpansions = ComposedExpansions::new(false);
}

/// различия между декомпозицией NF(K)D и прекомпозицией NF(K)C
pub struct ComposedExpansions
{
    pub map: HashMap<u32, Option<(usize, usize)>>,
    pub values: Vec<u32>,
}

impl ComposedExpansions
{
    pub fn new(is_canonical: bool) -> Self
    {
        let mut values = vec![];
        let mut map = HashMap::new();

        for code in 0 ..= LAST_DECOMPOSITION_CODE {
            let decomposition = match get_form_d(code, is_canonical) {
                Some(decomposition) => decomposition,
                None => continue,
            };

            // нам не нужны декомпозиции, которые не попадают в дополнительный блок
            // декомпозиция < 2 кодпоинтов, пары 15 бит стартер / 16 бит нестартер
            if decomposition.len() < 2
                || (decomposition.len() == 2
                    && decomposition[0].is_starter()
                    && decomposition[0].code <= 0x7FFF
                    && decomposition[1].is_nonstarter()
                    && decomposition[1].code <= 0xFFFF)
            {
                continue;
            }

            let precomposition = get_form_c(code, is_canonical).unwrap();

            // прекомпозиция - собирается в себя же
            if precomposition.len() == 0 {
                map.insert(code, None);
            }

            // сравним последовательности кодпоинтов
            let d: Vec<u32> = decomposition.iter().map(|c| c.code).collect();
            let p: Vec<u32> = precomposition.iter().map(|c| c.code).collect();

            if d == p {
                continue;
            }

            map.insert(code, Some((values.len(), precomposition.len())));

            precomposition.iter().for_each(|c| {
                values.push((c.code << 8) | c.ccc.compressed() as u32);
            })
        }

        ComposedExpansions { map, values }
    }

    pub fn size(&self) -> usize
    {
        self.values.len() * 4
    }

    pub fn get(&self, code: u32) -> Option<(usize, &[u32])>
    {
        match self.map.get(&code)? {
            Some((i, len)) => Some((*i, &self.values[*i .. *i + *len])),
            None => Some((0, &[])),
        }
    }
}

/// декомпозиция NF(K)D
fn get_form_d(code: u32, is_canonical: bool) -> Option<&'static Vec<Codepoint>>
{
    match is_canonical {
        true => NFD.get(&code),
        false => NFKD.get(&code),
    }
}

/// прекомпозиция NF(K)C
fn get_form_c(code: u32, is_canonical: bool) -> Option<&'static Vec<Codepoint>>
{
    match is_canonical {
        true => NFC.get(&code),
        false => NFKC.get(&code),
    }
}
