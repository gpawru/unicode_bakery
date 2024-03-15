use std::collections::HashMap;

use unicode_data::{COMBINES_BACKWARDS, COMPOSITION_PAIRS};

/// информация о хранимых композициях для стартера
#[derive(Default, Clone)]
pub struct CompositionInfo
{
    /// индекс первого элемента в таблице композиций
    pub index: u16,
    /// количество композиций для стартера
    pub count: u8,
}

impl CompositionInfo
{
    /// информация о хранимых композициях в сжатом виде (LE):
    /// [zzzz zzzz zzz] [z zzzz]
    ///      11 бит      5 бит      
    ///        \           \-------- количество пар
    ///         \------------------- индекс в таблице пар
    pub fn baked(&self) -> u16
    {
        assert!(self.index <= 0x7FF);
        assert!(self.count <= 0x1F);

        self.index | ((self.count as u16) << 11)
    }
}

/// "запеченные" композиции - массив значений и индексы для кодпоинтов
///
/// формат записи в таблице (LE):
/// xxxx xxxx  xxxx xxxx    xxyy yyyy  yyyy yyyy    yyyy ____  ____ ____    iiii iiii  iiii iiii
///
/// где:
///     xx.. - второй кодпоинт
///     yy.. - результат комбинирования
///     ii.. - сжатая информация о композициях результата (см. CompositionInfo)
#[derive(Clone)]
pub struct BakedCompositions
{
    pub table: Vec<u64>,
    pub index: HashMap<u32, CompositionInfo>,
    pub backwards_index: HashMap<u32, CompositionInfo>,
}

impl BakedCompositions
{
    pub fn new() -> Self
    {
        let mut table = Vec::new();
        let mut index = HashMap::new();

        let mut starters: Vec<&u32> = COMPOSITION_PAIRS.keys().collect();
        starters.sort();

        // таблица записей для каждого комбинируемого стартера - кодпоинт, с которым он комбинируется, результат
        for starter in starters {
            let pairs = &COMPOSITION_PAIRS[starter];

            let mut seconds: Vec<&u32> = pairs.keys().collect();
            seconds.sort();

            index.insert(
                *starter,
                CompositionInfo {
                    index: table.len() as u16,
                    count: seconds.len() as u8,
                },
            );

            for second in seconds {
                // в значении нужно хранить:
                // 1. второй кодпоинт
                // 2. результирующий кодпоинт
                // 3. если полученный кодпоинт может быть скомбинирован - оффсет и количество вариантов, добавим на следующем шаге
                let combined = pairs.get(second).unwrap();
                let value = (*second as u64) | ((combined.code as u64) << 18);

                table.push(value);
            }
        }

        // дополнительная часть - комбинирование с предыдущим кодпоинтом
        let mut backwards_index = HashMap::new();

        let mut combining_backwards: Vec<&u32> = COMBINES_BACKWARDS.keys().collect();
        combining_backwards.sort();

        for &starter in combining_backwards {
            let pairs = &COMBINES_BACKWARDS[&starter];

            let mut prevs: Vec<&u32> = pairs.keys().collect();
            prevs.sort();

            backwards_index.insert(
                starter,
                CompositionInfo {
                    index: table.len() as u16,
                    count: prevs.len() as u8,
                },
            );

            for prev in prevs {
                let combined = pairs.get(prev).unwrap();
                let value = (*prev as u64) | ((combined.code as u64) << 18);

                table.push(value);
            }
        }

        // для каждой записанной комбинируемой пары записываем дополнительную информацию - ссылку на варианты комбинирования
        // получаемого кодпоинта и количество вариантов
        for value in table.iter_mut() {
            let codepoint = (*value >> 18) as u32;

            if let Some(info) = index.get(&codepoint) {
                *value |= (info.baked() as u64) << 48;
            }
        }

        Self {
            table,
            index,
            backwards_index,
        }
    }

    pub fn size(&self) -> usize
    {
        self.table.len() * 8
    }
}
