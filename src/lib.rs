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

pub mod parser_helpers {
    use super::{Element, Tagged};

    use nom;
    use nom::IResult;

    pub fn parse_char<'a, E>(i: &'a str) -> IResult<&'a str, char, E>
    where
        E: nom::error::ParseError<&'a str>
            + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
    {
        let parse_hexdigit =
            nom::bytes::streaming::take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

        // `preceeded` takes a prefix parser, and if it succeeds, returns the result
        // of the body parser. In this case, it parses uNNNN.
        let parse_hextag =
            nom::sequence::preceded(nom::character::streaming::char('u'), parse_hexdigit);

        let parse_u32 =
            nom::combinator::map_res(parse_hextag, move |hex| u32::from_str_radix(hex, 16));

        nom::combinator::map_opt(parse_u32, |value| std::char::from_u32(value))(i)
    }

    pub fn parse_escaped_char<'a, E>(i: &'a str) -> IResult<&'a str, char, E>
    where
        E: nom::error::ParseError<&'a str>
            + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
    {
        use nom::character::streaming::char;
        use nom::combinator::value;

        // parse chars that are preceded by \
        nom::sequence::preceded(
            nom::character::streaming::char('\\'),
            nom::branch::alt((
                parse_char,
                value('\n', char('n')),
                value('\r', char('r')),
                value('\t', char('t')),
                value('\u{08}', char('a')),
                value('\u{0C}', char('f')),
                value('\\', char('\\')),
                value('/', char('/')),
                value('"', char('"')),
            )),
        )(i)
    }

    pub fn parse_escaped_whitespace<'a, E: nom::error::ParseError<&'a str>>(
        i: &'a str,
    ) -> IResult<&'a str, &str, E> {
        use nom::character::streaming::{char, multispace1};

        nom::sequence::preceded(char('\\'), multispace1)(i)
    }

    pub fn parse_literal<'a, E: nom::error::ParseError<&'a str>>(
        i: &'a str,
    ) -> IResult<&'a str, &str, E> {
        use nom::bytes::streaming::is_not;
        use nom::combinator::verify;

        let not_quote_slash = is_not("\"\\");

        verify(not_quote_slash, |s: &str| !s.is_empty())(i)
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum StringFragment<'a> {
        Literal(&'a str),
        EscapedChar(char),
        EscapedWS,
    }

    pub fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
    where
        E: nom::error::ParseError<&'a str>
            + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
    {
        use nom::branch::alt;
        use nom::combinator::{map, value};

        alt((
            map(parse_literal, StringFragment::Literal),
            map(parse_escaped_char, StringFragment::EscapedChar),
            value(StringFragment::EscapedWS, parse_escaped_whitespace),
        ))(input)
    }

    pub fn parse_string<'a, E>(i: &'a str) -> IResult<&'a str, String, E>
    where
        E: nom::error::ParseError<&'a str>
            + nom::error::FromExternalError<&'a str, std::num::ParseIntError>,
    {
        use nom::character::streaming::char;
        use nom::multi::fold_many0;
        use nom::sequence::delimited;

        // fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
        // and for each output value, calls a folding function on each output value.
        let build_string = fold_many0(
            parse_fragment,
            // Our init value, an empty string
            String::new(),
            // Our folding function. For each fragment, append the fragment to the
            // string.
            |mut string, fragment| {
                match fragment {
                    StringFragment::Literal(s) => string.push_str(s),
                    StringFragment::EscapedChar(c) => string.push(c),
                    StringFragment::EscapedWS => {}
                }
                string
            },
        );

        // Finally, parse the string. Note that, if `build_string` could accept a raw
        // " character, the closing delimiter " would never match. When using
        // `delimited` with a looping parser (like fold_many0), be sure that the
        // loop won't accidentally match your closing delimiter!
        delimited(char('"'), build_string, char('"'))(i)
    }
}

pub mod parsers {
    use super::{Element, Tagged};

    use nom;
    use nom::IResult;

    // these references are directly from https://github.com/edn-format/edn

    //// basic types
    // nil represents nil, null or nothing.
    // It should be read as an object with similar meaning on the target platform.
    fn nil<'a>(input: &'a str) -> IResult<&'a str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::streaming::tag("nil"))(input)
    }

    // true and false should be mapped to booleans.
    // If a platform has canonic values for true and false, it is a further semantic of booleans
    // that all instances of true yield that (identical) value, and similarly for false.
    fn boolean<'a>(input: &'a str) -> IResult<&'a str, Element> {
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
    fn character<'a>(input: &'a str) -> IResult<&'a str, Element> {
        use nom::bytes::streaming::take_while_m_n;
        use nom::character::streaming::char;
        use nom::combinator;
        use nom::sequence::preceded;

        let parse_hexdigit = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

        // `preceeded` takes a prefix parser, and if it succeeds, returns the result
        // of the body parser. In this case, it parses uNNNN.
        let parse_hex = preceded(char('u'), parse_hexdigit);

        // transform str into u32
        let parse_unicode = combinator::map_res(parse_hex, move |hex| u32::from_str_radix(hex, 16));

        // get the actual unicode char
        let parse_char = preceded(
            char('\\'),
            combinator::map_opt(parse_unicode, |value| std::char::from_u32(value)),
        );

        combinator::map(parse_char, |c| Element::Character(c))(input)
    }

    // Strings are enclosed in "double quotes". May span multiple lines.
    // Standard C/Java escape characters \t, \r, \n, \\ and \" are supported.
    pub fn string<'a>(input: &'a str) -> IResult<&str, Element> {
        use super::parser_helpers;

        nom::combinator::map(parser_helpers::parse_string, |s| {
            Element::String(String::from(s))
        })(input)
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