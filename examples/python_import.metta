!(import! &self "python_import_file.py")

!(test (py-call (python_import_file.greet "PeTTa User")) "Hello, PeTTa User from Python!")
!(test (py-call (python_import_file.add 10 20)) 30)

; Regression tests for issue #288: Python strings must remain MeTTa strings.
!(test (py-call (python_import_file.echo "abc")) "abc")
!(test (py-call (python_import_file.echo "true")) "true")
!(test (py-call (python_import_file.echo "hello")) "hello")
!(test (py-call (python_import_file.boolean_true)) True)
