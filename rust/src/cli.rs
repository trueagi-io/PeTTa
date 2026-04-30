//! Streamlined CLI configuration

use clap::{Parser, ValueEnum};
use petta::Backend;

#[derive(Parser, Debug, Clone)]
#[command(name = "petta", author = "Patrick Hammer", version = "0.5.0")]
#[command(about = "🦀 PeTTa v0.5.0 - Production MeTTa Runtime", long_about = None)]
pub struct Cli {
#[arg(required = false, value_name = "FILES")]
pub files: Vec<String>,

#[arg(short, long, default_value_t = false)]
pub verbose: bool,

#[arg(short, long, default_value_t = false)]
pub time: bool,

#[arg(short, long, default_value = "prolog", value_name = "BACKEND")]
pub backend: BackendArg,

#[arg(short = 'i', long, default_value_t = false)]
pub interactive: bool,

#[arg(short = 'O', long, default_value = "pretty", value_name = "FORMAT")]
pub output_format: OutputFormat,

#[arg(long, default_value_t = false)]
pub profile: bool,

#[arg(long, default_value_t = false)]
pub trace: bool,

#[arg(long, default_value_t = false)]
pub stats: bool,
}

#[derive(ValueEnum, Clone, Debug, Default, PartialEq)]
pub enum BackendArg {
Mork,
#[default]
Prolog,
}

#[derive(ValueEnum, Clone, Debug, Default, PartialEq)]
pub enum OutputFormat {
#[default]
Pretty,
Compact,
Json,
SExpr,
}

impl std::fmt::Display for BackendArg {
fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
match self {
BackendArg::Mork => write!(f, "Mork"),
BackendArg::Prolog => write!(f, "Swipl"),
}
}
}

impl BackendArg {
pub fn to_backend(&self) -> Backend {
match self {
BackendArg::Mork => Backend::Mork,
BackendArg::Prolog => Backend::Swipl,
}
}
}
