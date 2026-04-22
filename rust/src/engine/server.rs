use std::path::Path;

use tracing::{debug, trace};

use super::errors::PeTTaError;

pub fn build_server_source(src_dir: &Path, verbose: bool) -> Result<String, PeTTaError> {
    let silent = if verbose { "false" } else { "true" };
    let mut src = String::new();

    src.push_str(":- initialization(server_main).\n");
    src.push_str(":- set_prolog_flag(verbose, silent).\n");
    src.push_str(":- style_check(-singleton).\n");
    src.push_str(":- style_check(-discontiguous).\n");
    src.push_str(":- discontiguous throw_dcg_expansion_error/1.\n");
    src.push_str(&format!(":- assertz(silent({})).\n", silent));

    for file in &[
        "parser.pl",
        "spaces.pl",
        "specializer.pl",
        "translator.pl",
        "filereader.pl",
        "utils.pl",
        "metta.pl",
        "server_loop.pl",
    ] {
        let fpath = src_dir.join(file);
        if !fpath.exists() {
            return Err(PeTTaError::FileNotFound(fpath));
        }
        let fstr = fpath.to_string_lossy().replace('\\', "\\\\");
        src.push_str(&format!(":- consult('{}').\n", fstr));
        trace!("Consulting Prolog file: {}", fpath.display());
    }

    // filereader.pl asserts silent(false) at startup; re-assert our setting after all consults
    src.push_str(&format!(
        ":- retractall(silent(_)), assertz(silent({})).\n",
        silent
    ));

    // Initialize type lookup cache
    src.push_str(":- nb_setval(fun_types, []).\n");

    debug!("Built Prolog server source ({} bytes)", src.len());
    Ok(src)
}
