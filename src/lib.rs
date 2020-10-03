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
// Upon encountering a tag, the reader will first read the next element (which may itself be or comprise other tagged elements),
// then pass the result to the corresponding handler for further interpretation, and the result of the handler will be the data value
// yielded by the tag + tagged element, i.e. reading a tag and tagged element yields one value.
// This value is the value to be returned to the program and is not further interpreted as edn data by the reader.
#[derive(PartialEq, Debug, Clone)]
pub struct Tagged {
    name: String,
    element: Box<Element>,
}

pub mod parsers {
    use super::{Element, Tagged};

    use nom;
    use nom::IResult;

    mod helpers {
        use nom;
        use nom::bytes::complete::take_while1;
        use nom::IResult;

        fn is_nonescaped_string_char(c: char) -> bool {
            let cv = c as u32;
            (cv >= 0x20) && (cv != 0x22) && (cv != 0x5C)
        }

        fn nonescaped_string(input: &str) -> IResult<&str, &str> {
            take_while1(is_nonescaped_string_char)(input)
        }
    }
    // we're going to use the full paths for the nom functions in eden, because I'd like to replace
    // complete with streaming whenever applicable. this is because EDN is suitable as a streaming
    // format (due to not having a structure that wraps everything) and I think it makes sense.

    // reference:
    // https://github.com/edn-format/edn

    //// basic types
    // nil represents nil, null or nothing. It should be read as an object with similar meaning on the target platform.
    fn nil<'a>(input: &'a str) -> IResult<&'a str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    // true and false should be mapped to booleans.
    // If a platform has canonic values for true and false, it is a further semantic of booleans that all instances of true yield that (identical) value, and similarly for false.
    fn boolean<'a>(input: &'a str) -> IResult<&'a str, Element> {
        nom::branch::alt((
            nom::combinator::value(Element::Boolean(false), nom::bytes::complete::tag("false")),
            nom::combinator::value(Element::Boolean(true), nom::bytes::complete::tag("true")),
        ))(input)
    }

    // Unicode characters are represented with \uNNNN as in Java. Backslash cannot be followed by whitespace.
    // Characters are preceded by a backslash: \c, \newline, \return, \space and \tab yield the corresponding characters.
    fn character<'a>(input: &'a str) -> IResult<&'a str, Element> {
        let parse_hexdigit =
            nom::bytes::complete::take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

        // `preceeded` takes a prefix parser, and if it succeeds, returns the result
        // of the body parser. In this case, it parses uNNNN.
        let parse_hextag =
            nom::sequence::preceded(nom::character::complete::char('u'), parse_hexdigit);

        let parse_u32 =
            nom::combinator::map_res(parse_hextag, move |hex| u32::from_str_radix(hex, 16));

        let parse_char = nom::combinator::map_opt(parse_u32, |value| std::char::from_u32(value));

        nom::combinator::map(parse_char, |c| Element::Character(c))(input)
    }

    // Strings are enclosed in "double quotes". May span multiple lines. Standard C/Java escape characters \t, \r, \n, \\ and \" are supported.
    fn string(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    // Symbols begin with a non-numeric character and can contain alphanumeric characters and . * + ! - _ ? $ % & = < >.
    // If -, + or . are the first character, the second character (if any) must be non-numeric.
    // Additionally, : # are allowed as constituent characters in symbols other than as the first character.

    // / has special meaning in symbols. It can be used once only in the middle of a symbol to separate the prefix (often a namespace) from the name,
    // e.g. my-namespace/foo. / by itself is a legal symbol, but otherwise neither the prefix nor the name part can be empty when the symbol contains /

    // If a symbol has a prefix and /, the following name component should follow the first-character restrictions for symbols as a whole.
    // This is to avoid ambiguity in reading contexts where prefixes might be presumed as implicitly included namespaces and elided thereafter.
    fn symbol(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    // Symbols are used to represent identifiers, and should map to something other than strings, if possible.

    // Keywords are identifiers that typically designate themselves. They are semantically akin to enumeration values.
    // Keywords follow the rules of symbols, except they can (and must) begin with :, e.g. :fred or :my/fred.
    // If the target platform does not have a keyword type distinct from a symbol type, the same type can be used without conflict,
    // since the mandatory leading : of keywords is disallowed for symbols.
    // Per the symbol rules above, :/ and :/anything are not legal keywords. A keyword cannot begin with ::

    // If the target platform supports some notion of interning, it is a further semantic of keywords that all instances of the same keyword yield the identical object.

    fn keyword(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
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
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
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
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    //// collections
    // A list is a sequence of values. Lists are represented by zero or more elements enclosed in parentheses (). Note that lists can be heterogeneous.
    fn list(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    // A vector is a sequence of values that supports random access.
    // Vectors are represented by zero or more elements enclosed in square brackets [].
    // Note that vectors can be heterogeneous.
    fn vector(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    // A map is a collection of associations between keys and values.
    // Maps are represented by zero or more key and value pairs enclosed in curly braces {}.
    // Each key should appear at most once. No semantics should be associated with the order in which the pairs appear.
    fn map(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    // A set is a collection of unique values. Sets are represented by zero or more elements enclosed in curly braces preceded by # #{}.
    // No semantics should be associated with the order in which the elements appear. Note that sets can be heterogeneous.
    fn set(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    fn element(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }

    fn tag(input: &str) -> IResult<&str, Element> {
        nom::combinator::value(Element::Nil, nom::bytes::complete::tag("nil"))(input)
    }
}
