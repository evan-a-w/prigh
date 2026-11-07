#![cfg(target_os = "linux")]

//! The portal backend's PipeWire side against a real PipeWire daemon (a
//! private one): GStreamer's videotestsrc provides a Video/Source node, as
//! a compositor's screen cast does, and the helper's stream reads frames
//! from it through a socket like the one OpenPipeWireRemote hands out.
//! Skipped without pipewire and gst-launch-1.0 on PATH (the nix build has
//! them, and sets PRIGH_REQUIRE_PIPEWIRE).

use prigh_computer::portal::{Capture, CursorMode, Stream};
use prigh_computer::screencast::ScreenCast;
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::time::{Duration, Instant};

struct Kill(Vec<Child>, PathBuf);

impl Drop for Kill {
    fn drop(&mut self) {
        for c in &mut self.0 {
            let _ = c.kill();
            let _ = c.wait();
        }
        let _ = std::fs::remove_dir_all(&self.1);
    }
}

fn spawn(dir: &Path, program: &str, args: &[&str]) -> Option<Child> {
    let log = std::fs::File::create(dir.join(format!("{program}.log"))).unwrap();
    let mut c = Command::new(program);
    c.args(args)
        .env("XDG_RUNTIME_DIR", dir)
        .env("PIPEWIRE_RUNTIME_DIR", dir)
        // nothing of the user's: wireplumber keeps state
        .env("HOME", dir)
        .env("XDG_STATE_HOME", dir)
        .env("XDG_CONFIG_HOME", dir)
        .env_remove("DBUS_SESSION_BUS_ADDRESS")
        .env_remove("PIPEWIRE_REMOTE")
        .stdout(log.try_clone().unwrap())
        .stderr(log);
    match c.spawn() {
        Ok(c) => Some(c),
        Err(e) if std::env::var_os("PRIGH_REQUIRE_PIPEWIRE").is_none() => {
            eprintln!("skipped: no {program} ({e})");
            None
        }
        Err(e) => panic!("{program}: {e}"),
    }
}

fn wait_for<T>(what: &str, dir: &Path, mut f: impl FnMut() -> Option<T>) -> T {
    let start = Instant::now();
    loop {
        if let Some(v) = f() {
            return v;
        }
        if start.elapsed() > Duration::from_secs(30) {
            let logs: Vec<String> = ["pipewire", "wireplumber", "gst-launch-1.0"]
                .iter()
                .map(|p| std::fs::read_to_string(dir.join(format!("{p}.log"))).unwrap_or_default())
                .collect();
            panic!("no {what} within 30s:\n{}", logs.join("\n"));
        }
        std::thread::yield_now();
    }
}

/// The id of the node named `name`, from pw-dump.
fn node_id(dir: &Path, name: &str) -> Option<u32> {
    let out = Command::new("pw-dump").env("XDG_RUNTIME_DIR", dir).env("PIPEWIRE_RUNTIME_DIR", dir).output().ok()?;
    let objects: serde_json::Value = serde_json::from_slice(&out.stdout).ok()?;
    objects.as_array()?.iter().find_map(|o| {
        (o["info"]["props"]["node.name"].as_str() == Some(name)).then(|| o["id"].as_u64())?.map(|id| id as u32)
    })
}

#[test]
fn reads_frames() {
    let dir = std::env::temp_dir().join(format!("prigh-computer-pw-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let mut kill = Kill(vec![], dir.clone());
    let Some(daemon) = spawn(&dir, "pipewire", &[]) else { return };
    kill.0.push(daemon);
    let socket = dir.join("pipewire-0");
    wait_for("PipeWire socket", &dir, || socket.exists().then_some(()));
    // the session manager links the stream to the screen, as on a desktop
    let Some(manager) = spawn(&dir, "wireplumber", &[]) else { return };
    kill.0.push(manager);
    // a 320x200 screen: SMPTE bars, in BGRx as GNOME sends
    let Some(source) = spawn(
        &dir,
        "gst-launch-1.0",
        &[
            "videotestsrc",
            "pattern=smpte",
            "is-live=true",
            "!",
            "video/x-raw,format=BGRx,width=320,height=200,framerate=10/1",
            "!",
            "pipewiresink",
            "mode=provide",
            "stream-properties=props,media.class=Video/Source,node.name=prigh-test-screen",
        ],
    ) else {
        return;
    };
    kill.0.push(source);
    let node = wait_for("screen node", &dir, || node_id(&dir, "prigh-test-screen"));

    let fd = UnixStream::connect(&socket).unwrap();
    let stream = Stream { node_id: node, position: Some((0, 0)), size: Some((320, 200)) };
    let mut cast = ScreenCast::open(fd.into(), &stream, CursorMode::Metadata).unwrap();
    assert_eq!(cast.size(), (320, 200));
    let frame = cast.frame().unwrap();
    assert_eq!(frame.dimensions(), (320, 200));
    // the top row's bars, left to right: white, yellow, cyan, green, magenta,
    // red, blue
    let bars: Vec<[u8; 3]> = (0..7)
        .map(|i| {
            let p = frame.get_pixel(i * 320 / 7 + 10, 20).0;
            [p[0], p[1], p[2]]
        })
        .collect();
    assert_eq!(
        bars,
        [[255, 255, 255], [255, 255, 0], [0, 255, 255], [0, 255, 0], [255, 0, 255], [255, 0, 0], [0, 0, 255]]
    );
    // a later frame, from the same stream
    assert_eq!(cast.frame().unwrap().dimensions(), (320, 200));
    // the test source has no pointer
    assert_eq!(cast.cursor(), None);
    drop(cast);
}
