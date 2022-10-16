((:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 H6 P7) ("t" P3 P4 H5 H2) ("v" H6 H5 H1 P7) ("x" P7 H6)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" P5 H6 H7) ("t" P5 P4 H6 H2) ("v" H7 H6 H1 P5) ("x" P3 H7)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 H6 P7) ("t" P3 P4 H5 H6) ("v" H5 H6 H1 P7) ("x" P7 H2)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" P5 H6 H7) ("t" P5 P4 H6 H7) ("v" H6 H7 H1 P5) ("x" P3 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" P5 H6 P7) ("t" P5 P3 H1 H2) ("v" H6 P4 P7 P5) ("x" P7 H6)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 P6 H7) ("t" P5 P6 H1 H2) ("v" H7 P4 P5 P6) ("x" P3 H7)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" P5 H6 P7) ("t" P5 P3 H6 H1) ("v" H6 P4 P7 P5) ("x" P7 H2)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" P5 P6 H7) ("t" P5 P6 H7 H1) ("v" H7 P4 P5 P6) ("x" P3 H2))))