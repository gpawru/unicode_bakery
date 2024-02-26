use std::collections::HashMap;

/// статистика, собираемая при кодировании кодпоинтов
#[derive(Debug, Clone)]
pub struct EncodeCodepointStats
{
    pub blocks: HashMap<String, EncodeCodepointStatsBlock>,
}

#[derive(Debug, Clone)]
pub struct EncodeCodepointStatsBlock
{
    pub count: usize,
    pub order: usize,
    pub entries: HashMap<u32, String>,
}

impl EncodeCodepointStats
{
    pub fn new() -> Self
    {
        Self {
            blocks: HashMap::new(),
        }
    }

    /// добавляем ключ
    pub fn touch(&mut self, key: impl AsRef<str>) -> &mut EncodeCodepointStatsBlock
    {
        let order = self.blocks.values().map(|e| e.order).max().unwrap_or(0) + 1;

        self.blocks
            .entry(key.as_ref().to_string())
            .or_insert(EncodeCodepointStatsBlock {
                count: 0,
                order,
                entries: HashMap::new(),
            })
    }
}

impl EncodeCodepointStatsBlock
{
    /// увеличить значение для какого-то параметра, если параметра нет - создать,
    /// добавить описание кодпоинта
    pub fn inc(&mut self, codepoint: u32, description: impl AsRef<str>)
    {
        self.count += 1;
        self.entries
            .insert(codepoint, description.as_ref().to_string());
    }
}
