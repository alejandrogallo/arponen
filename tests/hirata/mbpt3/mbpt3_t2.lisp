((:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P2 P1 H9 H10) (P1 P2 H9 H10)))
   ("+ 1.0" (:PERMUTE (P2 P1 H9 H10) (P2 P1 H10 H9)))
   ("- 1.0" (:PERMUTE (P2 P1 H9 H10) (P1 P2 H10 H9))))
  :TENSORS (("f" P2 H9) ("t3" P1 H10)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P1 P2 H9 H10) (P1 P2 H10 H9)))) :TENSORS
  (("Sum" H3) ("f" H3 H9) ("t3" P1 P2 H3 H10)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P2 P1 H9 H10) (P1 P2 H9 H10)))) :TENSORS
  (("Sum" P3) ("f" P2 P3) ("t3" P3 P1 H9 H10)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P1 P2 H9 H10) (P2 P1 H9 H10)))) :TENSORS
  (("Sum" H3) ("t2" P1 H3) ("v" H3 P2 H9 H10)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P1 P2 H10 H9) (P1 P2 H9 H10)))) :TENSORS
  (("Sum" P3) ("t2" P3 H10) ("v" P1 P2 H9 P3)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS (("Sum" H3 H4) ("t2" P1 P2 H3 H4) ("v" H3 H4 H9 H10)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P1 P2 H10 H9) (P2 P1 H10 H9)))
   ("- 1.0" (:PERMUTE (P1 P2 H10 H9) (P1 P2 H9 H10)))
   ("+ 1.0" (:PERMUTE (P1 P2 H10 H9) (P2 P1 H9 H10))))
  :TENSORS (("Sum" H4 P3) ("t2" P3 P1 H4 H10) ("v" H4 P2 H9 P3)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS (("Sum" P3 P4) ("t2" P3 P4 H9 H10) ("v" P1 P2 P3 P4)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P1 P2 H10 H9) (P1 P2 H9 H10)))) :TENSORS
  (("Sum" H4 H5 P3) ("t2" P3 P1 P2 H4 H5 H10) ("v" H4 H5 H9 P3)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P1 P2 H9 H10) (P2 P1 H9 H10)))) :TENSORS
  (("Sum" H5 P3 P4) ("t2" P3 P4 P1 H5 H9 H10) ("v" H5 P2 P3 P4)))
 (:PERMUTATIONS ("+ 0.25") :TENSORS
  (("Sum" H5 H6 P3 P4) ("t2" P3 P4 P1 P2 H5 H6 H9 H10) ("v" H5 H6 P3 P4)))
 (:PERMUTATIONS ("- 1.0") :TENSORS (("e2") ("t1" P1 P2 H9 H10))))