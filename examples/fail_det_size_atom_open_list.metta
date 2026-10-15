; size-atom is length/2 behind a non_list/1 guard, and non_list/1 is true only
; of a term that can never BECOME a list - so an unbound argument sails past
; it into length/2, which enumerates 0, 1, 2, ... forever. That is a real
; relational mode (examples/logicprogset.metta solves for a list from its
; length), so size-atom is nondet.
;
; It used to be det, and nothing else could see the problem either: size-atom
; is declared (: size-atom (-> $a Number)), a bare type variable, so
; check_call_arg/5 emits no check at all and even --strict reported a clean
; compile. This program was accepted, ran, and never came back.
;
; Rejecting it at compile time is what turns a hang into an error message.
(: sz (-[det]-> Number))
(= (sz) (size-atom $u))

!(sz)
