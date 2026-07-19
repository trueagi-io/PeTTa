!(import! &self (library lib_import))
!(import_prolog_functions_from_file (library lib_datetime.pl) (now format_date day_of_week))

(= (format-date $timestamp $format)
   (format_date $timestamp $format))

(= (day-of-week $timestamp)
   (day_of_week $timestamp))
