//! What the dialog says when the sidecar cannot start or stops with an
//! error: what happened, its last words, and what to do next.

use crate::launch::Os;

#[derive(Debug)]
pub enum Failure {
    NotFound {
        name: String,
    },
    BadArgs(String),
    /// The program exists (or not, on Windows: wsl.exe) but did not start.
    Spawn {
        program: String,
        error: String,
        missing: bool,
    },
    /// It stopped with an error, `ready` saying whether it had printed its
    /// URL.
    Exited {
        code: Option<i32>,
        ready: bool,
        stderr: String,
    },
    BadReady {
        message: String,
        stderr: String,
    },
}

pub struct Message {
    pub title: String,
    pub body: String,
}

fn hint_from_stderr(os: Os, code: Option<i32>, stderr: &str) -> Option<String> {
    let lower = stderr.to_ascii_lowercase();
    if lower.contains("unknown subcommand") || lower.contains("unknown command") {
        return Some(
            "This prigh has no `desktop` command: update prigh (e.g. `nix profile upgrade prigh`, or rebuild your \
             Home Manager configuration)."
                .into(),
        );
    }
    match os {
        Os::Windows
            if lower.contains("no installed distributions") || lower.contains("wsl_e_default_distro_not_found") =>
        {
            Some(
                "WSL has no Linux distribution: run `wsl --install` in PowerShell, then install prigh inside it."
                    .into(),
            )
        }
        Os::Windows if code == Some(127) || lower.contains("command not found") || lower.contains("no such file") => {
            Some(
                "prigh is not installed inside WSL (or not on its login shell's PATH): install it there (`nix profile \
             install github:evan-a-w/prigh`), or set PRIGH_DESKTOP_BACKEND to its path inside WSL."
                    .into(),
            )
        }
        _ => None,
    }
}

pub fn message(os: Os, f: &Failure) -> Message {
    let with_stderr = |text: String, stderr: &str| {
        if stderr.is_empty() {
            text
        } else {
            format!("{text}\n\nIt said:\n{stderr}")
        }
    };
    match f {
        Failure::NotFound { name } => Message {
            title: "prigh not found".into(),
            body: format!(
                "Could not find `{name}` to run `{name} desktop`.\n\nInstall prigh (`nix profile install \
                 github:evan-a-w/prigh`, or programs.prigh in Home Manager), or set PRIGH_DESKTOP_BACKEND to its path."
            ),
        },
        Failure::BadArgs(e) => Message {
            title: "Bad PRIGH_DESKTOP_ARGS".into(),
            body: format!("{e}\n\nFix PRIGH_DESKTOP_ARGS (words as a shell splits them) and start prigh again."),
        },
        Failure::Spawn { program, error, missing } => Message {
            title: "prigh did not start".into(),
            body: match (os, missing) {
                (Os::Windows, true) => format!(
                    "Could not run {program}: {error}\n\nThe desktop app runs prigh inside WSL: install WSL (`wsl \
                     --install` in PowerShell), then prigh inside it."
                ),
                _ => format!("Could not run {program}: {error}\n\nCheck that it is an executable prigh."),
            },
        },
        Failure::Exited { code, ready, stderr } => {
            let what = match (ready, code) {
                (false, Some(c)) => format!("prigh desktop stopped before it was ready (exit code {c})."),
                (false, None) => "prigh desktop was killed before it was ready.".to_string(),
                (true, Some(c)) => format!("prigh desktop stopped with an error (exit code {c})."),
                (true, None) => "prigh desktop was killed.".to_string(),
            };
            let next = hint_from_stderr(os, *code, stderr)
                .unwrap_or_else(|| "Fix what it says, then start prigh again.".to_string());
            Message { title: "prigh stopped".into(), body: with_stderr(format!("{what}\n\n{next}"), stderr) }
        }
        Failure::BadReady { message, stderr } => Message {
            title: "prigh did not start".into(),
            body: with_stderr(format!("{message}\n\nThe desktop app and prigh may not match: update both."), stderr),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn show(os: Os, f: Failure) -> String {
        let m = message(os, &f);
        format!("{}\n---\n{}", m.title, m.body)
    }

    #[test]
    fn not_found() {
        assert_eq!(
            show(Os::Unix, Failure::NotFound { name: "prigh".into() }),
            "prigh not found\n---\nCould not find `prigh` to run `prigh desktop`.\n\nInstall prigh (`nix profile \
             install github:evan-a-w/prigh`, or programs.prigh in Home Manager), or set PRIGH_DESKTOP_BACKEND to its \
             path."
        );
    }

    #[test]
    fn exited() {
        assert_eq!(
            show(Os::Unix, Failure::Exited { code: Some(2), ready: false, stderr: "-connect: bad address".into() }),
            "prigh stopped\n---\nprigh desktop stopped before it was ready (exit code 2).\n\nFix what it says, then \
             start prigh again.\n\nIt said:\n-connect: bad address"
        );
        assert_eq!(
            show(Os::Unix, Failure::Exited { code: None, ready: true, stderr: String::new() }),
            "prigh stopped\n---\nprigh desktop was killed.\n\nFix what it says, then start prigh again."
        );
        assert!(show(
            Os::Unix,
            Failure::Exited {
                code: Some(1),
                ready: false,
                stderr: "Error parsing command line:\n\n  unknown subcommand desktop".into()
            }
        )
        .contains("This prigh has no `desktop` command"));
    }

    #[test]
    fn windows_hints() {
        let s = show(
            Os::Windows,
            Failure::Exited { code: Some(127), ready: false, stderr: "bash: line 1: exec: prigh: not found".into() },
        );
        assert!(s.contains("prigh is not installed inside WSL"), "{s}");
        let s = show(
            Os::Windows,
            Failure::Exited {
                code: Some(-1),
                ready: false,
                stderr: "Windows Subsystem for Linux has no installed distributions.".into(),
            },
        );
        assert!(s.contains("run `wsl --install`"), "{s}");
        let s = show(
            Os::Windows,
            Failure::Spawn { program: "wsl.exe".into(), error: "program not found".into(), missing: true },
        );
        assert!(s.contains("install WSL (`wsl --install` in PowerShell)"), "{s}");
        // on Linux, 127 is just an exit code
        let s = show(Os::Unix, Failure::Exited { code: Some(127), ready: false, stderr: String::new() });
        assert!(!s.contains("WSL"), "{s}");
    }

    #[test]
    fn bad_ready() {
        assert_eq!(
            show(
                Os::Unix,
                Failure::BadReady {
                    message: "prigh desktop printed \"hi\", not its ready line".into(),
                    stderr: String::new()
                }
            ),
            "prigh did not start\n---\nprigh desktop printed \"hi\", not its ready line\n\nThe desktop app and prigh \
             may not match: update both."
        );
    }
}
