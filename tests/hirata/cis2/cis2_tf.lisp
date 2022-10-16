((:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 H6) ("v" P5 P4 H1 H2) ("x" P3 H6) ("y+" H6 P5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6) ("v" P3 P4 H5 H1) ("x" P6 H2) ("y+" H5 P6)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" P5 H6) ("v" P5 P4 H6 H1) ("x" P3 H2) ("y+" H6 P5))))