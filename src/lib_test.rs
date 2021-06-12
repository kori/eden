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