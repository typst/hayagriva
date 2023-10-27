use std::{fs, io, path::PathBuf, process::Command};

pub const CACHE_PATH: &str = "target/haya-cache";

pub fn iter_files<'a>(
    path: &'a PathBuf,
    extension: &'a str,
) -> impl Iterator<Item = PathBuf> + 'a {
    iter_files_with_name(path, extension, |_| true)
}

pub fn iter_files_with_name<'a, F>(
    path: &'a PathBuf,
    extension: &'a str,
    mut name_assertion: F,
) -> impl Iterator<Item = PathBuf> + 'a
where
    F: FnMut(&str) -> bool + 'static,
{
    fs::read_dir(path).unwrap().filter_map(move |thing| {
        let thing = thing.ok()?;
        if !thing.file_type().ok()?.is_file() {
            return None;
        }

        let path = thing.path();
        let thing_ext = path.extension();
        if thing_ext?.to_str() != Some(extension) {
            return None;
        }

        if !name_assertion(path.file_stem()?.to_str()?) {
            return None;
        }

        Some(path)
    })
}

/// Clone a repo if it does not exist, pull it otherwise.
///
/// This function requires a `git` installation to be available in PATH.
pub fn ensure_repo(
    repo_url: &str,
    repo_name: &str,
    branch_name: &str,
) -> Result<(), io::Error> {
    let cache_path = PathBuf::from(CACHE_PATH);
    fs::create_dir_all(CACHE_PATH)?;

    // Check if styles directory exists. If it does, try to git pull origin
    // main. If that fails or the directory does not exist, clone the repo.
    let style_path = cache_path.join(repo_name);
    let clone = if style_path.exists() {
        let status = Command::new("git")
            .args(["pull", "origin", branch_name])
            .current_dir(&style_path)
            .status()
            .expect("Please ensure git is installed");

        if !status.success() {
            fs::remove_dir_all(&style_path)?;
            true
        } else {
            false
        }
    } else {
        true
    };

    if clone {
        let status = Command::new("git")
            .args(["clone", repo_url, repo_name, "--depth", "1"])
            .current_dir(&cache_path)
            .status()
            .expect("Please ensure git is installed");

        if !status.success() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Failed to clone repo. Is git installed correnctly and is the internet working?",
            ));
        }
    }

    Ok(())
}
