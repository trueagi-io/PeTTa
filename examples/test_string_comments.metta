; Test that semicolons inside strings are NOT treated as comments
; This was broken before the string-aware comment stripper fix

; Test 0: comment separator and parentheses in strings:
!(test ")" ")")
!(test "(" "(")
!(test ";" ";")

; Test 1: Semicolon constant (from set.mm)
!(test (quote ";") ";")

; Test 2: String with semicolon in the middle
!(test "foo;bar" "foo;bar")

; Test 3: Multiple semicolons in string
!(test ";;;" ";;;")

; Test 4: Semicolon at start of string
!(test ";start" ";start")

; Test 5: Semicolon at end of string
!(test "end;" "end;")

; Test 6: Escaped characters still work
!(test "quote: \"" "quote: \"")

; Test 7: Backslash escape
!(test "path\\file" "path\\file")

; Test 8: Comments outside strings still work
(= (test-func) result) ; This comment should be stripped
!(test (test-func) result)

; Test 9: Escaped quote before ) and ;
!(test "\")" "\")")
!(test "\";" "\";")

; Test 10: Escaped backslash before the closing quote
!(test (quote "\\") "\\")
