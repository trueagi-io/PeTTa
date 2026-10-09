;; Regression test for the global-eviction Head-reset bug in lib_memo.pl.
;;
;; Background:
;;   memo_store/5 binds the queue's Head via get_memo_queue_state/4,
;;   then calls evict_global_space/1 (which may evict entries and bump
;;   Head inside evict_entry/3), and finally writes the queue state
;;   back with the *original* Head value via set_memo_queue_state/4.
;;   That overwrites the increment evict_entry just performed, so
;;   Head stays pinned to whatever value it had at the start of the
;;   call.
;;
;;   The next call to find_global_oldest/3 reads that stale Head from
;;   the database and looks for an entry at position Head+1 in
;;   metta_memo_q. With Head pinned at 0 (its initial value), once
;;   the first entry at position 1 has been evicted every subsequent
;;   eviction attempt fails and lib_memo prints
;;       WARNING: No entries to evict, but global limit exceeded.
;;   on stderr. The cache then silently grows past the configured
;;   size-limit instead of being bounded by it.
;;
;; This test exercises the global-eviction path by combining a tiny
;; size-limit, a memoized recursive function, and a non-ground tail
;; so that each recursion level is a distinct variant-cache entry.
;; The function still returns the right answer even when the cache
;; is leaking, so the pass/fail signal for the unit test is the
;; function's return value; the difference between the broken and
;; fixed implementation is whether the eviction loop emits
;; "WARNING: No entries to evict" lines on stderr. Run with stderr
;; captured (./run.sh ... 2>&1 | tee out.log) and grep out.log for
;; "WARNING: No entries to evict" -- with the fix there should be no
;; such lines.
;;
;; Reproducing manually:
;;   cd PeTTa
;;   git stash                       ; remove the memo_store fix
;;   ./run.sh examples/test_memo_global_eviction_head_reset.metta 2>&1 \
;;     | tee /tmp/buggy.log
;;   grep -c "WARNING: No entries to evict" /tmp/buggy.log
;;   ; -> 23 (or similar non-zero count)
;;   git stash pop                   ; restore the fix
;;   ./run.sh examples/test_memo_global_eviction_head_reset.metta 2>&1 \
;;     | tee /tmp/fixed.log
;;   grep -c "WARNING: No entries to evict" /tmp/fixed.log
;;   ; -> 0

!(import! &self (library lib_memo))

;; Tight global memory cap. ~1 KiB is small enough that even a handful
;; of variant entries overflow it, forcing repeated global eviction
;; while the recursion runs.
!(config-memoize (size-limit 0.000001))

!(memoize vtail-len)

;; Recursive length-of-list. The tail is a fresh variable so each
;; recursion level is a distinct variant-cache entry -- exactly the
;; pattern that triggers the bug.
(= (vtail-len ())
   0)
(= (vtail-len (cons $x $xs))
   (+ 1 (vtail-len $xs)))

;; 30 nested cons cells is enough to produce many variant entries and
;; force several global-eviction cycles during the descent. With the
;; fix every entry added after the cache fills up evicts an old one
;; and the recursion completes without any
;; "WARNING: No entries to evict" line being printed on stderr.
!(test
   (vtail-len
     (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a (cons a ())))))))))))))))))))))))))))))))
  30)
