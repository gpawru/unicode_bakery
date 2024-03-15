use unicode_data::{
    codepoint::Codepoint, hangul::is_composable_hangul_jamo, COMBINES_BACKWARDS,
    COMPOSITION_EXCLUSIONS, QC_NFC, QC_NFKC, UNICODE,
};

/// флаг комбинирования
/// - 0: может быть оставлен как есть, если за ним не следует кодпоинт с маркером 1.
/// - 1: обязательная декомпозиция предыдущего кодпоинта, возможно комбинирование с текущим.
pub fn get_qc(code: u32, is_canonical: bool) -> u8
{
    let qc_table: &Vec<char> = match is_canonical {
        true => &QC_NFC,
        false => &QC_NFKC,
    };

    let qc = qc_table[code as usize];

    match &UNICODE.get(&(code as u32)) {
        Some(codepoint) => get_flag(codepoint, qc),
        None => {
            assert_eq!('Y', qc);
            0
        }
    }
}

/// флаг комбинирования:
/// - 0: кодпоинт может быть оставлен как есть, если за ним не следует кодпоинт с маркером 1.
/// - 1:
///   - возможно комбинирование с предыдущим кодпоинтом:
///     - нестартеры,
///     - стартеры, комбинируемые с предыдущим кодпоинтом,
///     - хангыль чамо V, T.
///   - после декомпозиции кодпоинта он не будет собран в себя же:
///     - синглтоны,
///     - декомпозиция, содержащая несколько стартеров,
///     - стартеры с декомпозицией в нестартеры,
///     - исключения комопозиции.
fn get_flag(codepoint: &Codepoint, qc: char) -> u8
{
    macro_rules! checks {
    ($($expr: expr),+) => {
        if $($expr ||)+ false {
            return 1;
        }
    }}

    checks!(
        qc == 'M' || qc == 'N',
        codepoint.is_nonstarter(),
        is_composable_hangul_jamo(codepoint.code),
        codepoint.is_composition_exclusion(),
        COMBINES_BACKWARDS.contains_key(&codepoint.code),
        COMPOSITION_EXCLUSIONS.contains(&codepoint.code)
    );

    0
}
