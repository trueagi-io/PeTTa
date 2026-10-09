!(import! &self (library lib_import))
!(import_prolog_functions_from_file (library lib_datetime.pl) (now format_date day_of_week))

(: now (-[det]-> Number))
(: format_date (-[det]-> Number String Atom))
(: day_of_week (-[det]-> Number Atom))

(: format-date (-[det]-> Number String Atom))
(= (format-date $timestamp $format)
   (format_date $timestamp $format))

(: day-of-week (-[det]-> Number Atom))
(= (day-of-week $timestamp)
   (day_of_week $timestamp))
