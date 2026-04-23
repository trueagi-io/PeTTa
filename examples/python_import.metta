!(import! &self examples/python_import_file.py)

!(test (repr (py-call (python_import_file.greet "PeTTa User"))) "Hello, PeTTa User from Python!")
!(test (py-call (python_import_file.add 10 20)) 30)
