now(TimeStamp) :-
    get_time(TimeStamp).

format_date(TimeStamp, Format, Formatted) :-
    stamp_date_time(TimeStamp, DateTime, 'UTC'),
    format_time(atom(Formatted), Format, DateTime).

day_of_week(TimeStamp, DayName) :-
    stamp_date_time(TimeStamp, DateTime, 'UTC'),
    format_time(atom(DayName), '%A', DateTime).
