use zed_extension_api as zed;

struct PebbleExtension;

impl zed::Extension for PebbleExtension {
    fn new() -> Self {
        Self
    }
}

zed::register_extension!(PebbleExtension);