//! The desktop shell's decisions, kept apart from the window so they are
//! tested without one: how to start `prigh desktop`, what its page lines
//! say, which window to use, where it may navigate (and where its browser window's links go), how it zooms, and what to tell the user when the
//! sidecar fails.

pub mod args;
pub mod browser;
pub mod devtools;
pub mod failure;
pub mod launch;
pub mod navigation;
pub mod profile;
pub mod ready;
pub mod search_path;
pub mod tail;
pub mod zoom;
