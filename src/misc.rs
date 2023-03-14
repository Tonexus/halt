// miscellaneous functions

use regex::Regex;
use lazy_static::lazy_static;

// keywords
// kinds
const KW_TYPE: &str = "Type";

// types
const KW_ARR:   &str = "Arr";
const KW_ASCII: &str = "Ascii";
const KW_BOOL:  &str = "Bool";
const KW_F32:   &str = "F32";
const KW_OPT:   &str = "Opt";
const KW_RES:   &str = "Res";
const KW_S8:    &str = "S8";
const KW_S16:   &str = "S16";
const KW_S32:   &str = "S32";
const KW_SSIZE: &str = "SSize";
const KW_U8:    &str = "U8";
const KW_U16:   &str = "U16";
const KW_U32:   &str = "U32";
const KW_USIZE: &str = "USize";

// values
const KW_FAIL:  &str = "fail";
const KW_FALSE: &str = "false";
const KW_I:     &str = "i";
const KW_NONE:  &str = "none";
const KW_OKAY:  &str = "okay";
const KW_SOME:  &str = "some";
const KW_TRUE:  &str = "true";

// statements
const KW_BREAK:    &str = "break";
const KW_CONTINUE: &str = "continue";
const KW_ELSE:     &str = "else";
const KW_FOR:      &str = "for";
const KW_FROM:     &str = "from";
const KW_IF:       &str = "if";
const KW_IN:       &str = "in";
const KW_LET:      &str = "let";
const KW_LOOP:     &str = "loop";
const KW_MATCH:    &str = "match";
const KW_RETURN:   &str = "return";
const KW_TO:       &str = "to";

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

