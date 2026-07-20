; git-import! is prepared before import signature discovery and needs no helper library.
!(git-import! "git_import_core_fixture.git" "" "examples/imports")
!(import! &self (library test))

!(test (git-core-result) 42)
