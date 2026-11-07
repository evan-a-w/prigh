//! The command line.

use crate::detect::Kind;
use crate::driver::{Config, Format};
use std::time::Duration;

pub const USAGE: &str = "usage: prigh-computer [--backend auto|wlroots|x11|fake] [--max-long-side N] [--max-pixels N] \
[--format png|jpeg] [--settle-ms N] [--watch-seconds N] [--fake-size WxH] [--probe]
JSON lines on stdin and stdout: {\"id\":1,\"op\":\"info\"}, {\"id\":2,\"op\":\"act\",\"action\":{\"action\":\"screenshot\"}}";

#[derive(Debug, Clone)]
pub struct Args {
    pub backend: Option<Kind>,
    pub config: Config,
    pub fake_size: (u32, u32),
    pub probe: bool,
}

fn number<T: std::str::FromStr>(flag: &str, v: &str) -> Result<T, String> {
    v.parse().map_err(|_| format!("{flag} {v:?}: expected a number"))
}

/// `args` without the program name; `env_backend` is $PRIGH_COMPUTER_BACKEND.
pub fn parse(args: &[String], env_backend: Option<&str>) -> Result<Args, String> {
    let mut out = Args { backend: None, config: Config::default(), fake_size: (2560, 1600), probe: false };
    if let Some(b) = env_backend.filter(|b| !b.is_empty()) {
        out.backend = Kind::parse(b).map_err(|e| format!("$PRIGH_COMPUTER_BACKEND: {e}"))?;
    }
    let mut it = args.iter();
    while let Some(flag) = it.next() {
        let (flag, inline) = match flag.split_once('=') {
            Some((f, v)) => (f, Some(v.to_string())),
            None => (flag.as_str(), None),
        };
        let mut value = || inline.clone().or_else(|| it.next().cloned()).ok_or_else(|| format!("{flag} needs a value"));
        match flag {
            "--backend" => out.backend = Kind::parse(&value()?)?,
            "--max-long-side" => out.config.limits.max_long_side = number(flag, &value()?)?,
            "--max-pixels" => out.config.limits.max_pixels = number(flag, &value()?)?,
            "--format" => {
                out.config.format = match value()?.as_str() {
                    "png" => Format::Png,
                    "jpeg" | "jpg" => Format::Jpeg,
                    other => return Err(format!("--format {other:?}: use png or jpeg")),
                }
            }
            "--settle-ms" => out.config.settle = Duration::from_millis(number(flag, &value()?)?),
            "--watch-seconds" => out.config.watch = Duration::from_secs_f64(number(flag, &value()?)?),
            "--fake-size" => {
                let v = value()?;
                out.fake_size = v
                    .split_once('x')
                    .and_then(|(w, h)| Some((w.parse().ok()?, h.parse().ok()?)))
                    .filter(|&(w, h)| w > 0 && h > 0)
                    .ok_or_else(|| format!("--fake-size {v:?}: expected WIDTHxHEIGHT"))?;
            }
            "--probe" => out.probe = true,
            "--help" | "-h" | "-help" => return Err(USAGE.into()),
            other => return Err(format!("unknown argument {other:?}\n{USAGE}")),
        }
    }
    if out.config.limits.max_long_side < 16 || out.config.limits.max_pixels < 256 {
        return Err("--max-long-side must be at least 16 and --max-pixels at least 256".into());
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(a: &[&str], env: Option<&str>) -> Result<Args, String> {
        parse(&a.iter().map(|s| s.to_string()).collect::<Vec<_>>(), env)
    }

    #[test]
    fn parses() {
        let a = p(&[], None).unwrap();
        assert_eq!(a.backend, None);
        assert_eq!(a.config.limits.max_long_side, 1280);
        let a = p(
            &["--backend", "fake", "--max-long-side=800", "--format", "jpeg", "--fake-size", "640x400", "--probe"],
            None,
        )
        .unwrap();
        assert_eq!(a.backend, Some(Kind::Fake));
        assert_eq!(a.config.limits.max_long_side, 800);
        assert_eq!(a.config.format, Format::Jpeg);
        assert_eq!(a.fake_size, (640, 400));
        assert!(a.probe);
        assert_eq!(p(&[], Some("fake")).unwrap().backend, Some(Kind::Fake));
        assert_eq!(p(&["--backend", "x11"], Some("fake")).unwrap().backend, Some(Kind::X11));
        assert!(p(&["--fake-size", "640"], None).unwrap_err().contains("WIDTHxHEIGHT"));
        assert!(p(&["--bogus"], None).unwrap_err().starts_with("unknown argument"));
        assert!(p(&["--format"], None).unwrap_err().contains("needs a value"));
        assert!(p(&[], Some("mac")).unwrap_err().starts_with("$PRIGH_COMPUTER_BACKEND"));
    }
}
