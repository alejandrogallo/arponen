((:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H10 H11 H38) (P3 P1 P2 P30 H9 H10 H11 H38)))
   ("+ 1.0" (:PERMUTE (P30 P1 P3 P2 H9 H10 H11 H38) (P2 P1 P3 P30 H9 H10 H11 H38)))
   ("+ 1.0" (:PERMUTE (P30 P2 P3 P1 H9 H10 H11 H38) (P1 P2 P3 P30 H9 H10 H11 H38)))
   ("+ 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H10 H11 H38) (P30 P1 P2 P3 H10 H9 H11 H38)))
   ("- 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H10 H11 H38) (P3 P1 P2 P30 H10 H9 H11 H38)))
   ("- 1.0" (:PERMUTE (P30 P1 P3 P2 H9 H10 H11 H38) (P2 P1 P3 P30 H10 H9 H11 H38)))
   ("- 1.0" (:PERMUTE (P30 P2 P3 P1 H9 H10 H11 H38) (P1 P2 P3 P30 H10 H9 H11 H38)))
   ("+ 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H11 H10 H38) (P30 P1 P2 P3 H11 H9 H10 H38)))
   ("- 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H11 H10 H38) (P3 P1 P2 P30 H11 H9 H10 H38)))
   ("- 1.0" (:PERMUTE (P30 P1 P3 P2 H9 H11 H10 H38) (P2 P1 P3 P30 H11 H9 H10 H38)))
   ("- 1.0" (:PERMUTE (P30 P2 P3 P1 H9 H11 H10 H38) (P1 P2 P3 P30 H11 H9 H10 H38)))
   ("+ 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H38 H10 H11) (P30 P1 P2 P3 H38 H9 H10 H11)))
   ("- 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H38 H10 H11) (P3 P1 P2 P30 H38 H9 H10 H11)))
   ("- 1.0" (:PERMUTE (P30 P1 P3 P2 H9 H38 H10 H11) (P2 P1 P3 P30 H38 H9 H10 H11)))
   ("- 1.0" (:PERMUTE (P30 P2 P3 P1 H9 H38 H10 H11) (P1 P2 P3 P30 H38 H9 H10 H11))))
  :TENSORS (("f" P30 H9) ("t2" P1 P2 P3 H10 H11 H38)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H9 H10 H11 H38) (P1 P2 P3 P30 H10 H9 H11 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H9 H11 H10 H38) (P1 P2 P3 P30 H11 H9 H10 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H9 H38 H10 H11) (P1 P2 P3 P30 H38 H9 H10 H11))))
  :TENSORS (("Sum" H4) ("f" H4 H9) ("t2" P1 P2 P3 P30 H4 H10 H11 H38)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P30 P1 P2 P3 H9 H10 H11 H38) (P3 P1 P2 P30 H9 H10 H11 H38)))
   ("+ 1.0" (:PERMUTE (P30 P1 P3 P2 H9 H10 H11 H38) (P2 P1 P3 P30 H9 H10 H11 H38)))
   ("+ 1.0" (:PERMUTE (P30 P2 P3 P1 H9 H10 H11 H38) (P1 P2 P3 P30 H9 H10 H11 H38))))
  :TENSORS (("Sum" P4) ("f" P30 P4) ("t2" P4 P1 P2 P3 H9 H10 H11 H38)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P1 P3 P2 P30 H11 H38 H9 H10)))
   ("- 1.0" (:PERMUTE (P2 P1 P3 P30 H11 H38 H9 H10) (P2 P3 P1 P30 H11 H38 H9 H10)))
   ("- 1.0" (:PERMUTE (P1 P2 P30 P3 H11 H38 H9 H10) (P1 P30 P2 P3 H11 H38 H9 H10)))
   ("- 1.0" (:PERMUTE (P2 P1 P30 P3 H11 H38 H9 H10) (P2 P30 P1 P3 H11 H38 H9 H10)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P3 P30 P1 P2 H11 H38 H9 H10)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H38 H11 H9 H10) (P1 P2 P3 P30 H10 H38 H9 H11)))
   ("- 1.0" (:PERMUTE (P1 P2 P3 P30 H38 H11 H9 H10) (P1 P3 P2 P30 H10 H38 H9 H11)))
   ("- 1.0" (:PERMUTE (P2 P1 P3 P30 H38 H11 H9 H10) (P2 P3 P1 P30 H10 H38 H9 H11)))
   ("- 1.0" (:PERMUTE (P1 P2 P30 P3 H38 H11 H9 H10) (P1 P30 P2 P3 H10 H38 H9 H11)))
   ("- 1.0" (:PERMUTE (P2 P1 P30 P3 H38 H11 H9 H10) (P2 P30 P1 P3 H10 H38 H9 H11)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H38 H11 H9 H10) (P3 P30 P1 P2 H10 H38 H9 H11)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P1 P2 P3 P30 H10 H11 H9 H38)))
   ("- 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P1 P3 P2 P30 H10 H11 H9 H38)))
   ("- 1.0" (:PERMUTE (P2 P1 P3 P30 H11 H38 H9 H10) (P2 P3 P1 P30 H10 H11 H9 H38)))
   ("- 1.0" (:PERMUTE (P1 P2 P30 P3 H11 H38 H9 H10) (P1 P30 P2 P3 H10 H11 H9 H38)))
   ("- 1.0" (:PERMUTE (P2 P1 P30 P3 H11 H38 H9 H10) (P2 P30 P1 P3 H10 H11 H9 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P3 P30 P1 P2 H10 H11 H9 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H38 H11 H10 H9) (P1 P2 P3 P30 H9 H38 H10 H11)))
   ("- 1.0" (:PERMUTE (P1 P2 P3 P30 H38 H11 H10 H9) (P1 P3 P2 P30 H9 H38 H10 H11)))
   ("- 1.0" (:PERMUTE (P2 P1 P3 P30 H38 H11 H10 H9) (P2 P3 P1 P30 H9 H38 H10 H11)))
   ("- 1.0" (:PERMUTE (P1 P2 P30 P3 H38 H11 H10 H9) (P1 P30 P2 P3 H9 H38 H10 H11)))
   ("- 1.0" (:PERMUTE (P2 P1 P30 P3 H38 H11 H10 H9) (P2 P30 P1 P3 H9 H38 H10 H11)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H38 H11 H10 H9) (P3 P30 P1 P2 H9 H38 H10 H11)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H10 H9) (P1 P2 P3 P30 H9 H11 H10 H38)))
   ("- 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H10 H9) (P1 P3 P2 P30 H9 H11 H10 H38)))
   ("- 1.0" (:PERMUTE (P2 P1 P3 P30 H11 H38 H10 H9) (P2 P3 P1 P30 H9 H11 H10 H38)))
   ("- 1.0" (:PERMUTE (P1 P2 P30 P3 H11 H38 H10 H9) (P1 P30 P2 P3 H9 H11 H10 H38)))
   ("- 1.0" (:PERMUTE (P2 P1 P30 P3 H11 H38 H10 H9) (P2 P30 P1 P3 H9 H11 H10 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H10 H9) (P3 P30 P1 P2 H9 H11 H10 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P1 P2 P3 P30 H9 H10 H11 H38)))
   ("- 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P1 P3 P2 P30 H9 H10 H11 H38)))
   ("- 1.0" (:PERMUTE (P2 P1 P3 P30 H11 H38 H9 H10) (P2 P3 P1 P30 H9 H10 H11 H38)))
   ("- 1.0" (:PERMUTE (P1 P2 P30 P3 H11 H38 H9 H10) (P1 P30 P2 P3 H9 H10 H11 H38)))
   ("- 1.0" (:PERMUTE (P2 P1 P30 P3 H11 H38 H9 H10) (P2 P30 P1 P3 H9 H10 H11 H38)))
   ("+ 1.0" (:PERMUTE (P1 P2 P3 P30 H11 H38 H9 H10) (P3 P30 P1 P2 H9 H10 H11 H38))))
  :TENSORS (("t1" P1 P2 H11 H38) ("v" P3 P30 H9 H10))))