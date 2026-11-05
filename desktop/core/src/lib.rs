//! The desktop shell's decisions, kept apart from the window so they are
//! tested without one: how to start `prigh desktop`, what its ready line
//! says, where the window may navigate, and what to tell the user when the
//! sidecar fails.

pub mod args;
pub mod failure;
pub mod launch;
pub mod navigation;
pub mod ready;
pub mod search_path;
pub mod tail;
