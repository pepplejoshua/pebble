use zed_extension_api as zed;
use zed_extension_api::{Command, LanguageServerId, Result, Worktree};

struct PebbleExtension;

impl zed::Extension for PebbleExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Command> {
        let path = worktree.which("pebc").ok_or_else(|| {
            "pebc not found on $PATH — install the Pebble compiler and make sure `pebc` is on your PATH".to_string()
        })?;
        Ok(Command {
            command: path,
            args: vec!["lsp".to_string()],
            env: Default::default(),
        })
    }
}

zed::register_extension!(PebbleExtension);
