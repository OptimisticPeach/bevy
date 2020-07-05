//! [![](https://bevyengine.org/assets/bevy_logo_docs.svg)](https://bevyengine.org)
//!
//! Bevy is an open-source modular game engine built in Rust, with a focus on developer productivity and performance.
//!
//! Check out the [Bevy website](https://bevyengine.org) for more information, read the
//! [Bevy Book](https://bevyengine.org/learn/book/introduction) for a step-by-step guide, and [engage with our
//! community](https://bevyengine.org/community/) if you have any questions or ideas!
//!
//! ## Example
//!Here is a simple "Hello World" Bevy app:
//! ```no_run
//!use bevy::prelude::*;
//!
//!fn main() {
//!    App::build()
//!        .add_default_plugins()
//!        .add_system(hello_world_system.system())
//!        .run();
//!}
//!
//!fn hello_world_system() {
//!    println!("hello world");
//!}
//! ```

//! Don't let the simplicity of the example above fool you. Bevy is a [fully featured game engine](https://bevyengine.org/learn/book/introduction/features/)
//! and it gets more powerful every day!
//!
//! ### This Crate
//! The "bevy" crate is just a container crate that makes it easier to consume Bevy components.
//! The defaults provide a "full" engine experience, but you can easily enable / disable features
//! in your project's Cargo.toml to meet your specific needs. See Bevy's Cargo.toml for a full list of features available.
//!
//! If you prefer it, you can also consume the individual bevy crates directly.

#![doc(
    html_logo_url = "https://bevyengine.org/assets/icon.png",
    html_favicon_url = "https://bevyengine.org/assets/icon.png"
)]

mod add_default_plugins;
pub mod prelude;

pub use add_default_plugins::*;
pub use glam as math;
pub use legion;

pub use bevy_derive as derive;

pub mod app;
pub mod asset;
pub mod core;
pub mod diagnostic;
pub mod ecs;
pub mod gltf;
pub mod input;
pub mod pbr;
pub use bevy_property as property;
pub mod render;
pub mod scene;
pub mod sprite;
pub mod text;
pub mod transform;
pub mod type_registry;
pub mod ui;
#[cfg(feature = "wgpu")]
pub mod wgpu;
pub mod window;
#[cfg(feature = "winit")]
pub mod winit;
