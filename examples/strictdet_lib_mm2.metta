!(import! &self (library lib_mm2))

!(test (＋ (strictdet-lib-mm2 fact)) true)
