use unicode_data::codepoint::Codepoint;

pub trait CodepointFilter
{
    /// фильтрация кодпоинтов для отсечения редкоиспользуемых/мёртвых письменностей в нормализации
    /// true, если кодпоинт допустим
    fn filter(&self, codepoint: &Codepoint) -> bool;
}
