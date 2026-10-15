!(add-atom &space "Hello")
!(add-atom &space 42)
!(add-atom &space (A B))

!(test (collapse (match &space $x $x)) ("Hello" 42 (A B)))
