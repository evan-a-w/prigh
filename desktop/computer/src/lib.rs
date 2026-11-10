//! prigh-computer: lets prigh's agent see and drive this machine's desktop.
//! The tool host runs it and talks JSON lines over stdio ([driver]); each
//! desktop is a [backend::Backend], chosen by [detect].

pub mod action;
pub mod args;
pub mod backend;
pub mod detect;
pub mod driver;
pub mod fake;
#[cfg(target_os = "linux")]
pub mod keymap;
pub mod keysym;
#[cfg(target_os = "macos")]
pub mod macos;
#[cfg(target_os = "linux")]
pub mod portal;
pub mod quartz;
pub mod scale;
#[cfg(target_os = "linux")]
pub mod screencast;
pub mod win_input;
#[cfg(windows)]
pub mod windows;
#[cfg(target_os = "linux")]
pub mod wlroots;
#[cfg(target_os = "linux")]
pub mod x11;
#[cfg(target_os = "linux")]
pub mod xkb;
