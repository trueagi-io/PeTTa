!(import! &self ../lib/lib_datetime)

!(let $now-ts (now) ($now-ts))

!(let* (($now-ts (now))
        ($formatted (format-date $now-ts "%Y-%m-%d %H:%M:%S")))
    ($formatted))

!(let* (($ts (now))
        ($plus-3600 (+ $ts 3600))
        ($formatted (format-date $plus-3600 "%Y-%m-%d %H:%M:%S")))
    ($formatted))

!(let* (($ts 1766188800)
        ($dow (day-of-week $ts)))
    ($dow))
!(test (day-of-week 1766188800) Saturday)

!(let* (($ts1 1735689600)
        ($ts2 1736294400)
        ($diff (- $ts2 $ts1)))
    ($diff))
!(test (- 1736294400 1735689600) 604800)

!(let* (($ts 1735725045)
        ($time-only (format-date $ts "%H:%M:%S")))
    ($time-only))
!(test (format-date 1735689600 "%B") January)
