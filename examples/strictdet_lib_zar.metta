!(import! &self (library lib_zar))

!(test (consult_file (library lib_datetime.pl)) true)
