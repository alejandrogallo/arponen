((:PERMUTATIONS ("+ 1.0") :TENSORS (("v" P3 P4 H1 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("f" P4 H1) ("t" P3 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" H5) ("t" P3 H5) ("v" H5 P4 H1 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5) ("t" P5 H2) ("v" P3 P4 H1 P5)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5) ("f" H5 H1) ("t" P3 P4 H5 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5) ("f" P4 P5) ("t" P5 P3 H1 H2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS (("Sum" H5 H6) ("t" P3 P4 H5 H6) ("v" H5 H6 H1 H2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H1 H2))))
  :TENSORS (("Sum" H6 P5) ("t" P5 P3 H6 H2) ("v" H6 P4 H1 P5)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS (("Sum" P5 P6) ("t" P5 P6 H1 H2) ("v" P3 P4 P5 P6)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS (("Sum" H5 P6) ("f" H5 P6) ("t" P6 P3 P4 H5 H1 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H6 H7 P5) ("t" P5 P3 P4 H6 H7 H2) ("v" H6 H7 H1 P5)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" H7 P5 P6) ("t" P5 P6 P3 H7 H1 H2) ("v" H7 P4 P5 P6)))
 (:PERMUTATIONS ("- 1.0") :TENSORS (("e") ("t" P3 P4 H1 H2))))