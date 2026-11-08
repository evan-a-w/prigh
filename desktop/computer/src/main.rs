use prigh_computer::{args, detect, driver::Driver};
use std::io::{BufRead, Write};
use std::sync::mpsc;

enum Input {
    Line(String),
    Quit,
}

/// SIGTERM, SIGINT and SIGHUP end the helper as end of input does, after
/// the request in hand, so the driver lets go of any key it holds: blocked
/// here (before any thread starts, so every thread inherits it) and waited
/// for by one thread.
#[cfg(unix)]
fn quit_on_signals(tx: mpsc::Sender<Input>) {
    unsafe {
        let mut set: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&mut set);
        for s in [libc::SIGTERM, libc::SIGINT, libc::SIGHUP] {
            libc::sigaddset(&mut set, s);
        }
        libc::pthread_sigmask(libc::SIG_BLOCK, &set, std::ptr::null_mut());
        std::thread::spawn(move || {
            let mut sig = 0;
            libc::sigwait(&set, &mut sig);
            let _ = tx.send(Input::Quit);
        });
    }
}

#[cfg(not(unix))]
fn quit_on_signals(_: mpsc::Sender<Input>) {}

fn main() {
    let (tx, rx) = mpsc::channel();
    quit_on_signals(tx.clone());
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let args = match args::parse(&argv, std::env::var("PRIGH_COMPUTER_BACKEND").ok().as_deref()) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("prigh-computer: {e}");
            std::process::exit(2);
        }
    };
    let backend = match detect::open(args.backend, &detect::Options { fake_size: args.fake_size }) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("prigh-computer: {e}");
            std::process::exit(1);
        }
    };
    let mut driver = Driver::new(backend, args.config);
    let stdout = std::io::stdout();
    if args.probe {
        println!("{}", driver.info());
        return;
    }
    std::thread::spawn(move || {
        for line in std::io::stdin().lock().lines() {
            let Ok(line) = line else { break };
            if tx.send(Input::Line(line)).is_err() {
                return;
            }
        }
        let _ = tx.send(Input::Quit);
    });
    while let Ok(Input::Line(line)) = rx.recv() {
        if line.trim().is_empty() {
            continue;
        }
        let reply = driver.handle_line(&line);
        let mut out = stdout.lock();
        if writeln!(out, "{reply}").and_then(|()| out.flush()).is_err() {
            break;
        }
    }
}
