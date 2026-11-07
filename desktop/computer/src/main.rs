use prigh_computer::{args, detect, driver::Driver};
use std::io::{BufRead, Write};

fn main() {
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
    for line in std::io::stdin().lock().lines() {
        let Ok(line) = line else { break };
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
