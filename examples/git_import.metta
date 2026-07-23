;Simple import without build step:
!(git-import! "https://github.com/rTreutlein/test_metta_lib.git")

!(import! &self (library test_metta_lib test))

!(test (mytest 10) 10)
