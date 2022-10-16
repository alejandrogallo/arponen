((:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P2 P1 H9 H10) (P1 P2 H9 H10)))
   ("+ 1.0" (:PERMUTE (P2 P1 H9 H10) (P2 P1 H10 H9)))
   ("- 1.0" (:PERMUTE (P2 P1 H9 H10) (P1 P2 H10 H9))))
  :TENSORS (("f" P2 H9) ("t1" P1 H10)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P1 P2 H9 H10) (P1 P2 H10 H9)))) :TENSORS
  (("Sum" H3) ("f" H3 H9) ("t1" P1 P2 H3 H10)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P2 P1 H9 H10) (P1 P2 H9 H10)))) :TENSORS
  (("Sum" P3) ("f" P2 P3) ("t1" P3 P1 H9 H10)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS (("v" P1 P2 H9 H10))))