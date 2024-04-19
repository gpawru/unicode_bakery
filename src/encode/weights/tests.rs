use std::marker::PhantomData;

use crate::encode::EncodeCodepoint;
use crate::{
    encode::weights::{bake_weights_vec, AdditionalInfo, EncodeWeights},
    stats::EncodeCodepointStats,
};
use unicode_data::{codepoint::CanonicalCombiningClass, UNICODE};

/// итератор по узлам дерева
pub struct TrieIter<'a>
{
    ptr: *const u32,
    start: *const u32,
    is_first: bool,
    _marker: PhantomData<&'a u32>,
}

pub struct TrieNode
{
    pub code: u32,
    pub ccc: u8,
    pub has_children: bool,
    pub weights_offset: usize,
    pub children_offset: usize,
}

impl TrieNode
{
    #[inline(always)]
    pub fn from(value: u32, weights_offset: usize) -> Self
    {
        Self {
            code: (value >> 8) & 0x3FFFF,
            ccc: (value >> 2) as u8 & 0x3F,
            weights_offset,
            children_offset: weights_offset + (value >> 26) as usize,
            has_children: (value & 1) != 0,
        }
    }
}

impl<'a> TrieIter<'a>
{
    #[inline(always)]
    pub fn new(source: &'a [u32], offset: usize) -> Self
    {
        Self {
            ptr: unsafe { source.as_ptr().add(offset) },
            start: source.as_ptr(),
            is_first: true,
            _marker: PhantomData,
        }
    }

    /// следующий элемент на уровне
    #[inline(always)]
    pub fn next(&mut self) -> Option<TrieNode>
    {
        if self.is_first {
            self.is_first = false;

            return Some(self.current_node());
        }

        // текущий элемент - последний?
        if (unsafe { *self.ptr } & 2) != 0 {
            return None;
        }

        // промотаем до следующего элемента
        self.skip_to_next();

        Some(self.current_node())
    }

    /// промотка до следующего элемента без проверок
    #[inline(always)]
    fn skip_to_next(&mut self)
    {
        let mut value = unsafe { *self.ptr };
        let mut level = 0;

        loop {
            // новый элемент располагается на следующем уровне
            if (value & 1) != 0 {
                level += 1;
            }

            // пропускаем запись и веса
            let weights_len = (value >> 26) as usize;
            self.ptr = unsafe { self.ptr.add(1 + weights_len) };

            value = unsafe { *self.ptr };

            // уровень = 0, т.е. полученный элемент - искомый
            if level == 0 {
                return;
            }

            // прочитанный элемент - последний на своем уровне
            if (value & 2) != 0 {
                level -= 1;
            }
        }
    }

    // текущий узел
    #[inline(always)]
    pub fn current_node(&self) -> TrieNode
    {
        unsafe { TrieNode::from(*self.ptr, self.ptr.offset_from(self.start) as usize + 1) }
    }
}

#[test]
fn test_trie()
{
    let encoder = EncodeWeights::new(true, &unicode_data::CLDR_UND_TRIE);
    let mut stats = EncodeCodepointStats::new();
    let mut data = AdditionalInfo {
        tries: &mut vec![],
        expansions: &mut vec![],
    };

    macro_rules! encode_codepoint {
        ($code: expr) => {{
            data.tries.clear();
            data.expansions.clear();
            let codepoint = &UNICODE[&$code];
            encoder.encode(codepoint, &mut data, &mut stats);
            codepoint
        }};
    }

    macro_rules! weights {
        ($code: expr) => {
            bake_weights_vec(encoder.trie.get(&$code).unwrap().weights)
        };
    }

    // MARKER_NONSTARTER_TRIE, обычные расширения

    // U+1ACC - COMBINING LATIN SMALL LETTER INSULAR G - [.215A.0020.0004][.0000.011D.0004]
    let codepoint = encode_codepoint!(0x1ACC);
    let mut iter = TrieIter::new(&data.tries, 0);

    let node = iter.next().unwrap();

    assert_eq!(node.code, codepoint.code);
    assert_eq!(node.ccc, codepoint.ccc.compressed());
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights!(0x1ACC)
    );
    assert!(!node.has_children);

    assert!(iter.next().is_none());

    // MARKER_NONSTARTER_TRIE, декомпозиция нестартера

    // U+0344 - COMBINING GREEK DIALYTIKA TONOS - [.0000.002B.0002][.0000.0024.0002]
    // -> U+0308(230) U+0301(230)
    // U+0308 - COMBINING DIAERESIS - [.0000.002B.0002]
    // U+0301 - COMBINING ACUTE ACCENT - [.0000.0024.0002]
    let _codepoint = encode_codepoint!(0x0344);
    let mut iter = TrieIter::new(&data.tries, 0);

    let node = iter.next().unwrap();

    assert_eq!(node.code, 0x0308);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(230).compressed());
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights!(0x308)
    );
    assert!(!node.has_children);

    let node = iter.next().unwrap();

    assert_eq!(node.code, 0x0301);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(230).compressed());
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights!(0x301)
    );
    assert!(!node.has_children);

    assert!(iter.next().is_none());

    // U+0F73 - TIBETAN VOWEL SIGN II - [.348A.0020.0002]
    // -> U+0F71(129) U+0F72(130)
    let _codepoint = encode_codepoint!(0x0F73);
    let mut iter = TrieIter::new(&data.tries, 0);

    let node = iter.next().unwrap();

    assert_eq!(node.code, 0x0F71);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(129).compressed());
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights!(0xF71)
    );
    assert!(node.has_children);

    // U+0F71
    // 0F71 0F72 ; [.348A.0020.0002] # TIBETAN VOWEL SIGN AA, TIBETAN VOWEL SIGN I
    // 0F71 0F80 ; [.348C.0020.0002] # TIBETAN VOWEL SIGN AA, TIBETAN VOWEL SIGN REVERSED I
    // 0F71 0F74 ; [.348E.0020.0002] # TIBETAN VOWEL SIGN AA, TIBETAN VOWEL SIGN U

    let mut ch_iter = TrieIter::new(&data.tries, node.children_offset);

    let node = ch_iter.next().unwrap();
    assert_eq!(node.code, 0x0F72);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(130).compressed());

    let weights = bake_weights_vec(
        encoder
            .trie
            .get(&0x0F71)
            .unwrap()
            .children
            .as_ref()
            .unwrap()
            .get(&0x0F72)
            .unwrap()
            .weights,
    );
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights
    );
    assert!(!node.has_children);

    let node = ch_iter.next().unwrap();
    assert_eq!(node.code, 0x0F80, "{:04X}", node.code);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(130).compressed());

    let weights = bake_weights_vec(
        encoder
            .trie
            .get(&0x0F71)
            .unwrap()
            .children
            .as_ref()
            .unwrap()
            .get(&0x0F80)
            .unwrap()
            .weights,
    );
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights
    );
    assert!(!node.has_children);

    let node = ch_iter.next().unwrap();
    assert_eq!(node.code, 0x0F74, "{:04X}", node.code);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(132).compressed());

    let weights = bake_weights_vec(
        encoder
            .trie
            .get(&0x0F71)
            .unwrap()
            .children
            .as_ref()
            .unwrap()
            .get(&0x0F74)
            .unwrap()
            .weights,
    );
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights
    );
    assert!(!node.has_children);

    assert!(ch_iter.next().is_none());

    //
    let node = iter.next().unwrap();

    assert_eq!(node.code, 0x0F72);
    assert_eq!(node.ccc, CanonicalCombiningClass::from(130).compressed());
    assert_eq!(
        data.tries[node.weights_offset .. node.children_offset],
        weights!(0x0F72)
    );
    assert!(!node.has_children);

    assert!(iter.next().is_none());

    //
}
