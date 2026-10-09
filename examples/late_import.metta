; Cross-file late binding: the declaration and a caller load first, the
; definition arrives from a second file, and the caller's clause is
; retranslated so the call binds to the implementation.
!(import! &self late_decl_user)
!(import! &self late_definition)
!(test (call-late-defined-probe 41) 42)
