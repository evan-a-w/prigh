//! The sidecar's ready line: one JSON object on its stdout,
//! `{"url":"http://127.0.0.1:PORT/..."}`. Only flat objects of strings
//! (and other scalars, ignored) are accepted: enough for the contract, with
//! no JSON dependency.

#[derive(Debug, PartialEq, Eq)]
pub struct Error(pub String);

pub fn parse(line: &str) -> Result<String, Error> {
    let fields = parse_object(line.trim())
        .map_err(|e| Error(format!("prigh desktop printed {:?}, not its ready line ({e})", truncate(line))))?;
    let url = fields
        .into_iter()
        .find_map(|(k, v)| (k == "url").then_some(v).flatten())
        .ok_or_else(|| Error(format!("prigh desktop's ready line has no \"url\": {:?}", truncate(line))))?;
    if url.starts_with("http://") || url.starts_with("https://") {
        Ok(url)
    } else {
        Err(Error(format!("prigh desktop's ready line has a url that is not http(s): {url:?}")))
    }
}

fn truncate(s: &str) -> String {
    let s = s.trim_end();
    if s.chars().count() > 200 {
        s.chars().take(200).collect::<String>() + "…"
    } else {
        s.to_string()
    }
}

type Fields = Vec<(String, Option<String>)>;

fn parse_object(s: &str) -> Result<Fields, String> {
    let mut p = Parser { s: s.as_bytes(), i: 0 };
    p.ws();
    p.expect(b'{')?;
    let mut fields = Vec::new();
    p.ws();
    if p.peek() == Some(b'}') {
        p.i += 1;
    } else {
        loop {
            p.ws();
            let key = p.string()?;
            p.ws();
            p.expect(b':')?;
            p.ws();
            let value = if p.peek() == Some(b'"') {
                Some(p.string()?)
            } else {
                p.scalar()?;
                None
            };
            fields.push((key, value));
            p.ws();
            match p.next() {
                Some(b',') => continue,
                Some(b'}') => break,
                _ => return Err("expected , or }".into()),
            }
        }
    }
    p.ws();
    if p.i != p.s.len() {
        return Err("trailing characters".into());
    }
    Ok(fields)
}

struct Parser<'a> {
    s: &'a [u8],
    i: usize,
}

impl Parser<'_> {
    fn peek(&self) -> Option<u8> {
        self.s.get(self.i).copied()
    }
    fn next(&mut self) -> Option<u8> {
        let c = self.peek();
        self.i += 1;
        c
    }
    fn ws(&mut self) {
        while matches!(self.peek(), Some(b' ' | b'\t' | b'\r' | b'\n')) {
            self.i += 1;
        }
    }
    fn expect(&mut self, c: u8) -> Result<(), String> {
        if self.next() == Some(c) {
            Ok(())
        } else {
            Err(format!("expected {}", c as char))
        }
    }
    fn scalar(&mut self) -> Result<(), String> {
        let start = self.i;
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || b"+-.".contains(&c)) {
            self.i += 1;
        }
        if start == self.i {
            Err("expected a string, number, true, false or null".into())
        } else {
            Ok(())
        }
    }
    fn hex4(&mut self) -> Result<u32, String> {
        let h = self.s.get(self.i..self.i + 4).ok_or("short \\u escape")?;
        self.i += 4;
        u32::from_str_radix(std::str::from_utf8(h).map_err(|_| "bad \\u escape")?, 16)
            .map_err(|_| "bad \\u escape".into())
    }
    fn string(&mut self) -> Result<String, String> {
        self.expect(b'"')?;
        let mut out: Vec<u8> = Vec::new();
        loop {
            match self.next() {
                None => return Err("unterminated string".into()),
                Some(b'"') => break,
                Some(b'\\') => {
                    let c = match self.next() {
                        Some(b'"') => '"',
                        Some(b'\\') => '\\',
                        Some(b'/') => '/',
                        Some(b'b') => '\u{8}',
                        Some(b'f') => '\u{c}',
                        Some(b'n') => '\n',
                        Some(b'r') => '\r',
                        Some(b't') => '\t',
                        Some(b'u') => {
                            let hi = self.hex4()?;
                            let code = if (0xD800..0xDC00).contains(&hi) {
                                if self.next() != Some(b'\\') || self.next() != Some(b'u') {
                                    return Err("lone surrogate".into());
                                }
                                let lo = self.hex4()?;
                                0x10000 + ((hi - 0xD800) << 10) + (lo.wrapping_sub(0xDC00) & 0x3FF)
                            } else {
                                hi
                            };
                            char::from_u32(code).ok_or("bad \\u escape")?
                        }
                        _ => return Err("bad escape".into()),
                    };
                    let mut buf = [0; 4];
                    out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                }
                Some(c) => out.push(c),
            }
        }
        String::from_utf8(out).map_err(|_| "invalid UTF-8".into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ready_lines() {
        assert_eq!(
            parse(r#"{"url":"http://127.0.0.1:41234/#signin=abc"}"#),
            Ok("http://127.0.0.1:41234/#signin=abc".to_string())
        );
        // escapes, spacing, other fields, a trailing CR
        assert_eq!(
            parse("{ \"pid\": 12, \"ok\": true, \"url\" : \"http:\\/\\/127.0.0.1:1\\/x?a=\\u00e9\" }\r\n"),
            Ok("http://127.0.0.1:1/x?a=é".to_string())
        );
    }

    #[test]
    fn errors() {
        let err = |s| parse(s).unwrap_err().0;
        assert_eq!(
            err("listening on 7790"),
            "prigh desktop printed \"listening on 7790\", not its ready line (expected {)"
        );
        assert_eq!(err(r#"{"addr":"x"}"#), "prigh desktop's ready line has no \"url\": \"{\\\"addr\\\":\\\"x\\\"}\"");
        assert_eq!(
            err(r#"{"url":"file:///etc/passwd"}"#),
            "prigh desktop's ready line has a url that is not http(s): \"file:///etc/passwd\""
        );
        assert_eq!(
            err(r#"{"url":"http://x"} junk"#),
            "prigh desktop printed \"{\\\"url\\\":\\\"http://x\\\"} junk\", not its ready line (trailing characters)"
        );
        assert!(parse(r#"{"url":"http://x"#).is_err());
        assert!(parse(r#"{"url":12}"#).is_err());
        assert!(parse("").is_err());
    }
}
