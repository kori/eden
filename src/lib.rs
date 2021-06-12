use std::collections::{BTreeMap, BTreeSet};

#[derive(PartialEq, Debug, Clone)]
pub enum Element {
    Nil,
    String(String),
    Boolean(bool),
    Character(char),
    Symbol(String),
    Keyword(String),
    Integer(i64),
    Float(f64),
    List(Vec<Element>),
    Vector(Vec<Element>),
    Map(BTreeMap<Element, Element>),
    Set(BTreeSet<Element>),
}

// Tagged Elements
// # followed immediately by a symbol starting with an alphabetic character indicates that that symbol is a tag.
// A tag indicates the semantic interpretation of the following element.
// It is envisioned that a reader implementation will allow clients to register handlers for specific tags.
// Upon encountering a tag, the reader will first read the next element
// (which may itself be or comprise other tagged elements),
// then pass the result to the corresponding handler for further interpretation,
// and the result of the handler will be the data value
// yielded by the tag + tagged element, i.e. reading a tag and tagged element yields one value.
// This value is the value to be returned to the program and is not further interpreted
// as EDN data by the reader.
#[derive(PartialEq, Debug, Clone)]
pub struct Tagged {
    name: String,
    element: Box<Element>,
}

pub mod parsers {
    use super::{Element, Tagged};

    use nom::IResult;

    // these references are directly from https://github.com/edn-format/edn

    //// basic types
    // nil represents nil, null or nothing.
    // It should be read as an object with similar meaning on the target platform.
    pub fn nil(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // true and false should be mapped to booleans.
    // If a platform has canonic values for true and false, it is a further semantic of booleans
    // that all instances of true yield that (identical) value, and similarly for false.
    pub fn boolean(input: &str) -> IResult<&str, Element> {
        use nom::bytes::streaming::tag;
        use nom::combinator::value;

        nom::branch::alt((
            value(Element::Boolean(false), tag("false")),
            value(Element::Boolean(true), tag("true")),
        ))(input)
    }

    // Unicode characters are represented with \uNNNN as in Java. Backslash cannot be followed by whitespace.
    // Characters are preceded by a backslash:
    // \c, \newline, \return, \space and \tab yield the corresponding characters.

    // the EDN spec is incomplete, so [1] was used as a reference.
    // The longform parser is a deviation from the(non-)spec and the "reference implementation."
    // [1]: https://github.com/clojure/clojure/blob/12e976ca3b07d7434ad4571a6bbeb05ef45d49b4/src/jvm/clojure/lang/EdnReader.java#L601

    pub fn character(input: &str) -> IResult<&str, Element> {
        use nom::bytes::streaming::{tag, take_while_m_n};
        use nom::character::streaming::char;
        use nom::combinator::{map_opt, map_res, value};
        use nom::error::{FromExternalError, ParseError};
        use nom::sequence::{delimited, preceded};

        // to parse the shortform \uXXXX
        fn parse_fourchar_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
        where
            E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
        {
            let parse_hex = take_while_m_n(1, 4, |c: char| c.is_ascii_hexdigit());
            let parse_unicode_prefix = preceded(char('u'), parse_hex);
            let parse_u32 = map_res(parse_unicode_prefix, move |hex| {
                u32::from_str_radix(hex, 16)
            });

            map_opt(parse_u32, std::char::from_u32)(input)
        }

        // to parse the longform \u{XXXXXX}, because emojis are cool
        fn parse_delimited_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
        where
            E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
        {
            let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

            let parse_delimited_hex =
                preceded(char('u'), delimited(char('{'), parse_hex, char('}')));
            let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

            map_opt(parse_u32, std::char::from_u32)(input)
        }

        match preceded(
            char('\\'),
            nom::branch::alt((
                parse_fourchar_unicode,
                parse_delimited_unicode,
                value(' ', tag("space")),
                value('\n', tag("newline")),
                value('\r', tag("return")),
                value('\t', tag("tab")),
            )),
        )(input)
        {
            Ok((r, c)) => Ok((r, Element::Character(c))),
            Err(e) => Err(e),
        }
    }

    // Strings are enclosed in "double quotes". May span multiple lines.
    // Standard C/Java escape characters \t, \r, \n, \\ and \" are supported.
    pub fn string<'a>(input: &'a str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // Symbols begin with a non-numeric character and can contain alphanumeric characters and
    // . * + ! - _ ? $ % & = < >.
    // If -, + or . are the first character, the second character (if any) must be non-numeric.
    // Additionally, : # are allowed as constituent characters in symbols other than as the first character.

    // / has special meaning in symbols. It can be used once only in the middle of a symbol to separate the
    // prefix (often a namespace) from the name,
    // e.g. my-namespace/foo. / by itself is a legal symbol, but otherwise neither the prefix nor the name
    // part can be empty when the symbol contains /

    // If a symbol has a prefix and /, the following name component should follow the first-character
    // restrictions for symbols as a whole.
    // This is to avoid ambiguity in reading contexts where prefixes might be presumed as implicitly
    // included namespaces and elided thereafter.
    fn symbol(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // Symbols are used to represent identifiers, and should map to something other than strings, if possible.

    // Keywords are identifiers that typically designate themselves. They are semantically akin to enumeration values.
    // Keywords follow the rules of symbols, except they can (and must) begin with :, e.g. :fred or :my/fred.
    // If the target platform does not have a keyword type distinct from a symbol type, the same type can be used without conflict,
    // since the mandatory leading : of keywords is disallowed for symbols.
    // Per the symbol rules above, :/ and :/anything are not legal keywords. A keyword cannot begin with ::

    // If the target platform supports some notion of interning, it is a further semantic of keywords that all instances of the same keyword yield the identical object.

    fn keyword(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }
    // Integers consist of the digits 0 - 9, optionally prefixed by - to indicate a negative number, or (redundantly) by +. No integer other than 0 may begin with 0. 64-bit (signed integer) precision is expected. An integer can have the suffix N to indicate that arbitrary precision is desired. -0 is a valid integer not distinct from 0.
    /*
    integer
      int
      int N
    digit
      0-9
    int
      digit
      1-9 digits
      + digit
      + 1-9 digits
      - digit
      - 1-9 digits
    */

    fn integer(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // 64-bit (double) precision is expected.
    // floating-point-number
    /*
      int M
      int frac
      int exp
      int frac exp
    digit
      0-9
    int
      digit
      1-9 digits
      + digit
      + 1-9 digits
      - digit
      - 1-9 digits
    frac
      . digits
    exp
      ex digits
    digits
      digit
      digit digits
    ex
      e
      e+
      e-
      E
      E+
      E-
    */
    fn floating(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    //// collections
    // A list is a sequence of values. Lists are represented by zero or more elements enclosed in parentheses (). Note that lists can be heterogeneous.
    fn list(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // A vector is a sequence of values that supports random access.
    // Vectors are represented by zero or more elements enclosed in square brackets [].
    // Note that vectors can be heterogeneous.
    fn vector(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // A map is a collection of associations between keys and values.
    // Maps are represented by zero or more key and value pairs enclosed in curly braces {}.
    // Each key should appear at most once. No semantics should be associated with the order in which the pairs appear.
    fn map(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // A set is a collection of unique values. Sets are represented by zero or more elements enclosed in curly braces preceded by # #{}.
    // No semantics should be associated with the order in which the elements appear. Note that sets can be heterogeneous.
    fn set(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    fn element(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    fn tag(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nil() {
        let goal = Ok(("", Element::Nil));
        assert_eq!(parsers::nil("nil"), goal);
    }

    #[test]
    fn test_boolean_true() {
        let goal = Ok(("", Element::Boolean(true)));
        assert_eq!(parsers::boolean("true"), goal);
    }

    #[test]
    fn test_boolean_false() {
        let goal = Ok(("", Element::Boolean(false)));
        assert_eq!(parsers::boolean("false"), goal);
    }

    #[test]
    fn test_character_space() {
        let goal = Ok(("", Element::Character(' ')));
        assert_eq!(parsers::character("\\space"), goal);
    }
    #[test]
    fn test_character_newline() {
        let goal = Ok(("", Element::Character('\n')));
        assert_eq!(parsers::character("\\newline"), goal);
    }
    #[test]
    fn test_character_return() {
        let goal = Ok(("", Element::Character('\r')));
        assert_eq!(parsers::character("\\return"), goal);
    }
    #[test]
    fn test_character_tab() {
        let goal1 = Ok(("", Element::Character('\t')));
        assert_eq!(parsers::character("\\tab"), goal1);
        let goal2 = Ok(("a", Element::Character('\t')));
        assert_eq!(parsers::character("\\taba"), goal2);
        let goal3 = Ok((" a", Element::Character('\t')));
        assert_eq!(parsers::character("\\tab a"), goal3);
    }

    #[test]
    fn test_character_shortform() {
        let goal1 = Ok(("", Element::Character('a')));
        assert_eq!(parsers::character("\\u0061"), goal1);
        let goal2 = Ok(("", Element::Character('0')));
        assert_eq!(parsers::character("\\u0030"), goal2);
        let goal3 = Ok(("", Element::Character('氷')));
        assert_eq!(parsers::character("\\u6c37"), goal3);
        // Careful! this will parse \u006c, then '37' will be left over.
        let goal4 = Ok(("37", Element::Character('l')));
        assert_eq!(parsers::character("\\u006c37"), goal4);
    }

    #[test]
    fn test_character_longform() {
        // with the longform syntax, test_character_shortform goal4 works as expected.
        let goal1 = Ok(("", Element::Character('氷')));
        assert_eq!(parsers::character("\\u{006c37}"), goal1);

        let goal2 = Ok(("", Element::Character('😀')));
        assert_eq!(parsers::character("\\u{1f600}"), goal2);
    }
}
