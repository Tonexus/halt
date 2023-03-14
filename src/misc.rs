// miscellaneous functions

use regex::Regex;
use lazy_static::lazy_static;

// implicit label names
pub const LABELS: [&str; 10] = ["_0", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9"];

// keywords
// kinds
pub const KW_TYPE: &str = "Type";

// types
pub const KW_ARR:   &str = "Arr";
pub const KW_ASCII: &str = "Ascii";
pub const KW_BOOL:  &str = "Bool";
pub const KW_F32:   &str = "F32";
pub const KW_OPT:   &str = "Opt";
pub const KW_RES:   &str = "Res";
pub const KW_S8:    &str = "S8";
pub const KW_S16:   &str = "S16";
pub const KW_S32:   &str = "S32";
pub const KW_SSIZE: &str = "SSize";
pub const KW_U8:    &str = "U8";
pub const KW_U16:   &str = "U16";
pub const KW_U32:   &str = "U32";
pub const KW_USIZE: &str = "USize";

// values
pub const KW_FAIL:  &str = "fail";
pub const KW_FALSE: &str = "false";
pub const KW_I:     &str = "i";
pub const KW_NONE:  &str = "none";
pub const KW_OKAY:  &str = "okay";
pub const KW_SOME:  &str = "some";
pub const KW_TRUE:  &str = "true";

// statements
pub const KW_BREAK:    &str = "break";
pub const KW_CONTINUE: &str = "continue";
pub const KW_ELSE:     &str = "else";
pub const KW_FOR:      &str = "for";
pub const KW_FROM:     &str = "from";
pub const KW_IF:       &str = "if";
pub const KW_IN:       &str = "in";
pub const KW_LET:      &str = "let";
pub const KW_LOOP:     &str = "loop";
pub const KW_MATCH:    &str = "match";
pub const KW_RETURN:   &str = "return";
pub const KW_TO:       &str = "to";

const KW_TYPES: [&str; 14] = [
    KW_ARR, KW_ASCII, KW_BOOL, KW_F32, KW_OPT, KW_RES, KW_S8, KW_S16, KW_S32,
    KW_SSIZE, KW_U8, KW_U16, KW_U32, KW_USIZE,
];
const KW_VALUES: [&str; 7] = [
    KW_FAIL, KW_FALSE, KW_I, KW_NONE, KW_OKAY, KW_SOME, KW_TRUE,
];
const KW_STATEMENTS: [&str; 12] = [
    KW_BREAK, KW_CONTINUE, KW_ELSE, KW_FOR, KW_FROM, KW_IF, KW_IN, KW_LET,
    KW_LOOP, KW_MATCH, KW_RETURN, KW_TO,
];

// returns number of variants in enum, if possible
pub fn enum_size(name: &str) -> Option<u32> {
    lazy_static! {
        // enum for up to 2^32 choices
        static ref ENUM: Regex = Regex::new("^N(\\d+)$").unwrap();
    }
    return ENUM.captures(name)
        .and_then(|c| c.get(1))
        .and_then(|m| m.as_str().parse::<u32>().ok());
}

// returns whether name is a keyword type
pub fn is_kw_type(name: &str) -> bool {
    return KW_TYPES.contains(&name) || enum_size(name).is_some();
}

// returns whether name is a keyword value
pub fn is_kw_value(name: &str) -> bool {
    return KW_VALUES.contains(&name);
}

// returns whether name is a keyword statement
pub fn is_kw_statement(name: &str) -> bool {
    return KW_STATEMENTS.contains(&name);
}

// returns whether name is any keyword
pub fn is_kw(name: &str) -> bool {
    return is_kw_type(name) || is_kw_value(name) || is_kw_statement(name);
}

