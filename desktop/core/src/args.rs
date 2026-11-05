//! `$PRIGH_DESKTOP_ARGS`: extra arguments for `prigh desktop`, written as a
//! shell would (words split on blanks, '…' and "…" quote, \ escapes).

#[derive(Debug, PartialEq, Eq)]
pub struct Error(pub String);

pub fn split(s: &str) -> Result<Vec<String>, Error> {
    let mut words = Vec::new();
    let mut word = String::new();
    let mut in_word = false;
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        match c {
            c if c.is_whitespace() => {
                if in_word {
                    words.push(std::mem::take(&mut word));
                    in_word = false;
                }
            }
            '\'' => {
                in_word = true;
                loop {
                    match chars.next() {
                        Some('\'') => break,
                        Some(c) => word.push(c),
                        None => return Err(Error(format!("unterminated ' in {s:?}"))),
                    }
                }
            }
            '"' => {
                in_word = true;
                loop {
                    match chars.next() {
                        Some('"') => break,
                        Some('\\') => match chars.next() {
                            Some(c @ ('"' | '\\' | '$' | '`')) => word.push(c),
                            Some(c) => {
                                word.push('\\');
                                word.push(c)
                            }
                            None => return Err(Error(format!("unterminated \" in {s:?}"))),
                        },
                        Some(c) => word.push(c),
                        None => return Err(Error(format!("unterminated \" in {s:?}"))),
                    }
                }
            }
            '\\' => {
                in_word = true;
                match chars.next() {
                    Some(c) => word.push(c),
                    None => return Err(Error(format!("trailing \\ in {s:?}"))),
                }
            }
            c => {
                in_word = true;
                word.push(c)
            }
        }
    }
    if in_word {
        words.push(word);
    }
    Ok(words)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ok(s: &str) -> Vec<String> {
        split(s).unwrap()
    }

    #[test]
    fn words() {
        assert_eq!(ok(""), Vec::<String>::new());
        assert_eq!(ok("  \n "), Vec::<String>::new());
        assert_eq!(ok("-cwd ~/proj  -name laptop"), ["-cwd", "~/proj", "-name", "laptop"]);
    }

    #[test]
    fn quoting() {
        assert_eq!(ok("-cwd '/my proj'"), ["-cwd", "/my proj"]);
        assert_eq!(ok(r#"-name "a \"b\" \n""#), ["-name", r#"a "b" \n"#]);
        assert_eq!(ok(r"a\ b c"), ["a b", "c"]);
        assert_eq!(ok("''"), [""]);
        assert_eq!(ok("x'y'\"z\""), ["xyz"]);
        // what lib.escapeShellArgs writes
        assert_eq!(ok("'-cwd' '/it'\\''s here'"), ["-cwd", "/it's here"]);
    }

    #[test]
    fn errors() {
        assert_eq!(split("'a").unwrap_err().0, "unterminated ' in \"'a\"");
        assert!(split("\"a").is_err());
        assert!(split("a\\").is_err());
    }
}
