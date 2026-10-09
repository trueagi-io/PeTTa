!(import! &self imports/relative/root)

!(test (from-sibling) 42)
!(test (from-second) 7)
