((:PERMUTATIONS ("- 1.0") :TENSORS (("Sum" H3) ("f" H3 H1) ("x" P2* H3)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS (("Sum" H3 P4) ("f" H3 P4) ("x" P4 P2* H3 H1)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS (("Sum" H3 H4 P5) ("v" H3 H4 H1 P5) ("x" P5 P2* H3 H4)))
 (:PERMUTATIONS ("+ 0.25") :TENSORS
  (("Sum" H3 H4 P5 P6) ("v" H3 H4 P5 P6) ("x" P5 P6 P2* H3 H4 H1)))
 (:PERMUTATIONS ("- 1.0") :TENSORS (("Sum" H3 P4) ("f" H3 P4) ("t" P4 H1) ("x" P2* H3)))
 (:PERMUTATIONS ("- 1.0") :TENSORS (("Sum" P3 H4 H5) ("t" P3 H4) ("v" H5 H4 H1 P3) ("x" P2* H5)))
 (:PERMUTATIONS ("- 0.5") :TENSORS
  (("Sum" P3 H4 H5 P6) ("t" P3 H1) ("v" H4 H5 P6 P3) ("x" P6 P2* H4 H5)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P3 H4 H5 P6) ("t" P3 H4) ("v" H5 H4 P6 P3) ("x" P6 P2* H5 H1)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P3 P4 H5 H6) ("t" P3 P4 H5 H1) ("v" H6 H5 P3 P4) ("x" P2* H6)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P3 P4 H5 H6) ("t" P3 H1) ("t" P4 H5) ("v" H6 H5 P4 P3) ("x" P2* H6))))