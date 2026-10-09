!(import! &self (library lib_datetime))

!(test (day-of-week 1766188800) Saturday)
!(test (format-date 1735689600 "%B") January)
