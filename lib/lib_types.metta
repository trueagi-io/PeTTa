;; Centralized type declarations for PeTTa/MeTTa libraries
;; This file consolidates type declarations that are shared across multiple modules

;; === Probability and Truth Value Types (for PLN/NARS) ===
;; These types are used by both lib_pln.metta and lib_nars.metta

;; Basic truth value type (strength, confidence)
(: stv (-> Number Number Expression))

;; Truth value conversion functions
(: Truth_c2w (-> Number Number))
(: Truth_w2c (-> Number Number))

;; Safe division to avoid division by zero
(: /safe (-> Number Number Number))

;; === PLN-specific Type Declarations ===

;; Probability intersection calculations
(: smallest-intersection-probability (-> Number Number Number))
(: largest-intersection-probability (-> Number Number Number))
(: conditional-probability-consistency (-> Number Number Number Bool))

;; === NARS-specific Type Declarations ===

;; NARS truth value operations
(: Truth_Deduction (-> Expression Expression Expression))
(: Truth_Abduction (-> Expression Expression Expression))
(: Truth_Induction (-> Expression Expression Expression))
(: Truth_Exemplification (-> Expression Expression Expression))
(: Truth_Revision (-> Expression Expression Expression))
(: Truth_Negation (-> Expression Expression))

;; === Higher-Order Function Types ===

;; List operations
(: foldl-atom (-> Expression Expression Expression Expression))
(: map-atom (-> Expression Expression Expression))
(: filter-atom (-> Expression Expression Expression))

;; === Utility Types ===

;; Clamp value to range
(: clamp (-> Number Number Number Number))

;; Tuple operations
(: TupleConcat (-> Expression Expression Expression))
(: TupleCount (-> Expression Number))

;; Logical operations with multiple arguments
(: and5 (-> Bool Bool Bool Bool Bool Bool))
(: min5 (-> Number Number Number Number Number Number))

;; Mathematical helpers
(: negate (-> Number Number))
(: invert (-> Number Number))

;; Sorting and uniqueness
(: InsertionSort (-> Expression Expression))
(: InsertSorted (-> Number Expression Expression))
(: Unique (-> Expression Expression Expression))
(: Without (-> Expression Expression Expression))
(: ElementOf (-> Expression Expression Bool))

;; Stamp operations for PLN/NARS
(: StampDisjoint (-> Expression Expression Bool))
(: StampConcat (-> Expression Expression Expression))

;; Priority queue operations
(: BestCandidate (-> Expression Expression Expression Expression))
(: PriorityRank (-> Expression Number))
(: PriorityRankNeg (-> Expression Number))
(: LimitSize (-> Expression Number Expression))
(: ConfidenceRank (-> Expression Number))

;; Derivation engine types
(: PLN.Derive (-> Expression Expression Expression))
(: PLN.Query (-> Expression Expression Expression))
(: NARS.Derive (-> Expression Expression Expression))
(: NARS.Query (-> Expression Expression Expression))

;; Configuration
(: PLN.Config.MaxSteps (-> Number))
(: PLN.Config.TaskQueueSize (-> Number))
(: PLN.Config.BeliefQueueSize (-> Number))
(: NARS.Config.MaxSteps (-> Number))
(: NARS.Config.TaskQueueSize (-> Number))
(: NARS.Config.BeliefQueueSize (-> Number))
