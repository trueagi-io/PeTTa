use std::path::Path;
use std::process::Command;

use tracing::debug;

use super::errors::PeTTaError;

/// Minimum supported SWI-Prolog version (major, minor).
pub const MIN_SWIPL_VERSION: (u32, u32) = (9, 3);

pub fn check_swipl_version(
    swipl_path: &Path,
    min_version: (u32, u32),
) -> Result<(), PeTTaError> {
    let output = Command::new(swipl_path)
        .arg("--version")
        .output()
        .map_err(|_| {
            PeTTaError::SwiplVersionError(format!(
                "swipl not found at {}. Install SWI-Prolog >= {}.{}.",
                swipl_path.display(),
                min_version.0,
                min_version.1
            ))
        })?;
    if !output.status.success() {
        return Err(PeTTaError::SwiplVersionError(
            "swipl --version failed".into(),
        ));
    }
    let vs = String::from_utf8_lossy(&output.stdout);
    for part in vs.split_whitespace() {
        let p: Vec<&str> = part.split('.').collect();
        if p.len() >= 2 {
            if let (Ok(ma), Ok(mi)) = (p[0].parse::<u32>(), p[1].parse::<u32>()) {
                if ma > min_version.0 || (ma == min_version.0 && mi >= min_version.1) {
                    debug!(
                        "SWI-Prolog version {}.{} meets minimum {}.{}",
                        ma, mi, min_version.0, min_version.1
                    );
                    return Ok(());
                }
                if ma < min_version.0 {
                    return Err(PeTTaError::SwiplVersionError(format!(
                        "SWI-Prolog {}.{} found, need >= {}.{}",
                        ma, mi, min_version.0, min_version.1
                    )));
                }
            }
        }
    }
    Ok(())
}

/// Check if SWI-Prolog is available at the given path.
pub fn swipl_available(swipl_path: &Path) -> bool {
    check_swipl_version(swipl_path, MIN_SWIPL_VERSION).is_ok()
}
