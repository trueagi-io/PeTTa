# lib_csp

Constraint Satisfaction / Optimization library for PeTTa, backed by SWI-Prolog CLP(FD).

## Overview
`lib_csp` provides practical CSP utilities for:
- minimum-cost assignment
- top-K assignment alternatives
- timetable scheduling with resource/conflict constraints

Backend:
- Prolog: `lib/lib_csp.pl`
- MeTTa API exposure: `lib/lib_csp.metta`

## Import

```metta
!(import! &self (library lib_csp))
```

## APIs

### 1) `csp-assignment-opt`
Type:
```metta
(: csp-assignment-opt (-> Expression Expression))
```

Usage:
```metta
(csp-assignment-opt ((9 2 7)
                     (6 4 3)
                     (5 8 1)))
```

Output shape:
```metta
(assignment-solution <total-cost> ((assign <worker-id> <task-id>) ...))
```

### 2) `csp-assignment-topk`
Type:
```metta
(: csp-assignment-topk (-> Number Expression Expression))
```

Usage:
```metta
(csp-assignment-topk 2 ((9 2 7)
                        (6 4 3)
                        (5 8 1)))
```

Output shape:
```metta
((assignment-solution <cost> ((assign ...) ...))
 (assignment-solution <cost> ((assign ...) ...))
 ...)
```

### 3) `csp-timetable-solve`
Type:
```metta
(: csp-timetable-solve (-> Expression Expression Expression Expression Expression))
```

Usage:
```metta
(csp-timetable-solve
  ((class c1 t1 g1 35)
   (class c2 t1 g2 25)
   (class c3 t2 g1 20))
  ((room r1 40)
   (room r2 30))
  (0 1 2)
  ((teacher t1 2)
   (group g1 0)))
```

Output shape:
```metta
(timetable-solution ((at <class-id> <slot> <room-id>) ...))
```

## Input Format Notes

### Assignment
- cost matrix must be a non-empty square matrix of integers
- worker and task ids in output are zero-based indices

### Timetable
- classes:
  - `(class <class-id> <teacher-id> <group-id> <size>)`
- rooms:
  - `(room <room-id> <capacity>)`
- slots:
  - integer list, e.g. `(0 1 2 3)`
- blocked constraints:
  - `(teacher <teacher-id> <slot>)`
  - `(group <group-id> <slot>)`

## Unsat Behavior
If no valid solution exists, APIs return:

```metta
Empty
```

## Test / Example
Example coverage is provided in:
- `examples/csp_library.metta`

Run:
```bash
sh run.sh ./examples/csp_library.metta
```

## Current Scope
This version focuses on correctness and clear API design for assignment and timetable CSP tasks using CLP(FD).
