((:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7) ("v" H7 P6 H1 H2) ("x" P4 P5 H7 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" P7) ("v" P5 P6 H1 P7) ("x" P7 P4 H2 H3*)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" H7) ("f" H7 H1) ("x" P4 P5 P6 H7 H2 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7) ("f" P6 P7) ("x" P7 P4 P5 H1 H2 H3*)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS (("Sum" H7 H8) ("v" H7 H8 H1 H2) ("x" P4 P5 P6 H7 H8 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" H7 P8) ("v" H7 P6 H1 P8) ("x" P8 P4 P5 H7 H2 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 P8) ("v" P5 P6 P7 P8) ("x" P7 P8 P4 H1 H2 H3*)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS (("Sum" H7 P8) ("f" H7 P8) ("x" P8 P4 P5 P6 H7 H1 H2 H3*)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" H7 H8 P9) ("v" H7 H8 H1 P9) ("x" P9 P4 P5 P6 H7 H8 H2 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 P8 P9) ("v" H7 P6 P8 P9) ("x" P8 P9 P4 P5 H7 H1 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 H8) ("t" P6 H7) ("v" H8 H7 H1 H2) ("x" P4 P5 H8 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P6 P4 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8) ("t" P7 H2) ("v" H8 P6 H1 P7) ("x" P4 P5 H8 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H2 H1))))
  :TENSORS (("Sum" H7 P8) ("t" P5 H7) ("v" H7 P6 H1 P8) ("x" P8 P4 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" P7 P8) ("t" P7 H1) ("v" P5 P6 P8 P7) ("x" P8 P4 H2 H3*)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" H7 P8) ("f" H7 P8) ("t" P8 H1) ("x" P4 P5 P6 H7 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 P8) ("f" H7 P8) ("t" P6 H7) ("x" P8 P4 P5 H1 H2 H3*)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P5 P6 H1 H2)))) :TENSORS
  (("Sum" P7 H8 H9) ("t" P7 H2) ("v" H8 H9 H1 P7) ("x" P4 P5 P6 H8 H9 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" H7 H8 P9) ("t" P6 H7) ("v" H8 H7 H1 P9) ("x" P9 P4 P5 H8 H2 H3*)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" P7 H8 H9) ("t" P7 H8) ("v" H9 H8 H1 P7) ("x" P4 P5 P6 H9 H2 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" P7 H8 P9) ("t" P7 H1) ("v" H8 P6 P9 P7) ("x" P9 P4 P5 H8 H2 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2))))
  :TENSORS (("Sum" H7 P8 P9) ("t" P5 H7) ("v" H7 P6 P8 P9) ("x" P8 P9 P4 H1 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8 P9) ("t" P7 H8) ("v" H8 P6 P9 P7) ("x" P9 P4 P5 H1 H2 H3*)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 H1) ("v" H8 H9 P10 P7) ("x" P10 P4 P5 P6 H8 H9 H2 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 H8 P9 P10) ("t" P6 H7) ("v" H8 H7 P9 P10) ("x" P9 P10 P4 P5 H8 H1 H2 H3*)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 H8) ("v" H9 H8 P10 P7) ("x" P10 P4 P5 P6 H9 H1 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P6 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P4 P6 H2 H1) (P5 P6 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P4 P6 H2 H1) (P5 P6 P4 H1 H2))))
  :TENSORS (("Sum" H7 P8) ("t" P4 P5 H7 H2) ("v" H7 P6 H1 P8) ("x" P8 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P6 P5 H1 H2) (P6 P4 P5 H1 H2))))
  :TENSORS (("Sum" P7 P8) ("t" P7 P4 H1 H2) ("v" P5 P6 P8 P7) ("x" P8 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 P8) ("f" H7 P8) ("t" P8 P6 H1 H2) ("x" P4 P5 H7 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" H7 P8) ("f" H7 P8) ("t" P5 P6 H7 H1) ("x" P8 P4 H2 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("+ 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P6 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H2 H1) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H2 H1) (P5 P6 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H2 H1) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 H8 P9) ("t" P5 P6 H7 H2) ("v" H8 H7 H1 P9) ("x" P9 P4 H8 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P6 P4 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8 H9) ("t" P7 P6 H8 H2) ("v" H9 H8 H1 P7) ("x" P4 P5 H9 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" H7 H8 P9) ("t" P5 P6 H7 H8) ("v" H7 H8 H1 P9) ("x" P9 P4 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8 P9) ("t" P7 P5 H1 H2) ("v" H8 P6 P9 P7) ("x" P9 P4 H8 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 P8 H9) ("t" P7 P8 H1 H2) ("v" H9 P6 P7 P8) ("x" P4 P5 H9 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P4 P6 H1 H2) (P5 P6 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))
   ("+ 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P5 P4 P6 H1 H2) (P5 P6 P4 H2 H1))))
  :TENSORS (("Sum" H7 P8 P9) ("t" P4 P5 H7 H1) ("v" H7 P6 P8 P9) ("x" P8 P9 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H2 H1))))
  :TENSORS (("Sum" P7 H8 P9) ("t" P7 P5 H8 H1) ("v" H8 P6 P9 P7) ("x" P9 P4 H2 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8 H9 P10) ("t" P7 P6 H1 H2) ("v" H8 H9 P10 P7) ("x" P10 P4 P5 H8 H9 H3*)))
 (:PERMUTATIONS ("+ 0.25") :TENSORS
  (("Sum" P7 P8 H9 H10) ("t" P7 P8 H1 H2) ("v" H9 H10 P7 P8) ("x" P4 P5 P6 H9 H10 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" H7 H8 P9 P10) ("t" P5 P6 H7 H1) ("v" H8 H7 P9 P10) ("x" P9 P10 P4 H8 H2 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" P7 H8 H9 P10) ("t" P7 P6 H8 H1) ("v" H9 H8 P10 P7) ("x" P10 P4 P5 H9 H2 H3*)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" P7 P8 H9 H10) ("t" P7 P8 H9 H1) ("v" H10 H9 P7 P8) ("x" P4 P5 P6 H10 H2 H3*)))
 (:PERMUTATIONS
  ("+ 0.25" ("+ 0.25" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.25" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 H8 P9 P10) ("t" P5 P6 H7 H8) ("v" H7 H8 P9 P10) ("x" P9 P10 P4 H1 H2 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8 H9 P10) ("t" P7 P6 H8 H9) ("v" H8 H9 P10 P7) ("x" P10 P4 P5 H1 H2 H3*)))
 (:PERMUTATIONS ("- 1.0") :TENSORS
  (("Sum" H7 P8) ("f" H7 P8) ("t" P4 P5 P6 H7 H1 H2) ("x" P8 H3*)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P5 P6 H1 H2)))) :TENSORS
  (("Sum" H7 H8 P9) ("t" P4 P5 P6 H7 H8 H2) ("v" H7 H8 H1 P9) ("x" P9 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P4 P6 H1 H2) (P5 P6 P4 H1 H2))))
  :TENSORS (("Sum" P7 H8 P9) ("t" P7 P4 P5 H8 H1 H2) ("v" H8 P6 P9 P7) ("x" P9 H3*)))
 (:PERMUTATIONS ("- 0.5") :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P4 P5 P6 H7 H1 H2) ("v" H8 H7 P9 P10) ("x" P9 P10 H8 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 H8 H9 P10) ("t" P7 P5 P6 H8 H1 H2) ("v" H9 H8 P10 P7) ("x" P10 P4 H9 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" P7 P8 H9 H10) ("t" P7 P8 P6 H9 H1 H2) ("v" H10 H9 P7 P8) ("x" P4 P5 H10 H3*)))
 (:PERMUTATIONS ("+ 0.25" ("- 0.25" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P4 P5 P6 H7 H8 H1) ("v" H7 H8 P9 P10) ("x" P9 P10 H2 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" P7 H8 H9 P10) ("t" P7 P5 P6 H8 H9 H1) ("v" H8 H9 P10 P7) ("x" P10 P4 H2 H3*)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 P4 P5 P6 H8 H9 H1 H2) ("v" H8 H9 P10 P7) ("x" P10 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P6 P4 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H1 H2))))
  :TENSORS (("Sum" H7 P8 H9) ("t" P6 H7) ("t" P8 H2) ("v" H9 H7 H1 P8) ("x" P4 P5 H9 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P6 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P5 P4 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P4 P5 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P4 P6 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" H7 H8 P9) ("t" P6 H7) ("t" P5 H8) ("v" H8 H7 H1 P9) ("x" P9 P4 H2 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS (("Sum" P7 P8 H9) ("t" P7 H1) ("t" P8 H2) ("v" H9 P6 P8 P7) ("x" P4 P5 H9 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H2 H1))))
  :TENSORS (("Sum" H7 P8 P9) ("t" P5 H7) ("t" P8 H1) ("v" H7 P6 P9 P8) ("x" P9 P4 H2 H3*)))
 (:PERMUTATIONS ("- 0.25" ("+ 0.25" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" P7 P8 H9 H10) ("t" P7 H1) ("t" P8 H2) ("v" H9 H10 P8 P7) ("x" P4 P5 P6 H9 H10 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P6 H7) ("t" P8 H1) ("v" H9 H7 P10 P8) ("x" P10 P4 P5 H9 H2 H3*)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))) :TENSORS
  (("Sum" P7 P8 H9 H10) ("t" P7 H1) ("t" P8 H9) ("v" H10 H9 P8 P7) ("x" P4 P5 P6 H10 H2 H3*)))
 (:PERMUTATIONS
  ("+ 0.25" ("- 0.25" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P6 P4 H1 H2)))
   ("- 0.25" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("+ 0.25" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.25" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.25" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P6 H7) ("t" P5 H8) ("v" H8 H7 P9 P10) ("x" P9 P10 P4 H1 H2 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P6 H7) ("t" P8 H9) ("v" H9 H7 P10 P8) ("x" P10 P4 P5 H1 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P6 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P4 P6 H2 H1) (P5 P6 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P4 P6 H2 H1) (P5 P6 P4 H1 H2))))
  :TENSORS (("Sum" H7 H8 P9) ("t" P4 P5 H7 H2) ("t" P6 H8) ("v" H8 H7 H1 P9) ("x" P9 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P4 P6 H2 H1) (P5 P6 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P4 P6 H2 H1) (P5 P6 P4 H1 H2))))
  :TENSORS (("Sum" H7 P8 P9) ("t" P4 P5 H7 H2) ("t" P8 H1) ("v" H7 P6 P9 P8) ("x" P9 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P5 P6 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P6 P4 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P6 P5 P4 H1 H2))))
  :TENSORS (("Sum" P7 H8 P9) ("t" P7 P4 H1 H2) ("t" P5 H8) ("v" H8 P6 P9 P7) ("x" P9 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("- 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H2 H1) (P4 P5 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H2 H1) (P5 P6 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H2 H1) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P5 P6 H7 H2) ("t" P8 H1) ("v" H9 H7 P10 P8) ("x" P10 P4 H9 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P6 P4 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H2 H1) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H2 H1) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" P7 H8 P9 H10) ("t" P7 P6 H8 H2) ("t" P9 H1) ("v" H10 H8 P9 P7) ("x" P4 P5 H10 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P5 P6 H7 H8) ("t" P9 H1) ("v" H7 H8 P10 P9) ("x" P10 P4 H2 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2))))
  :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 P5 H1 H2) ("t" P6 H8) ("v" H9 H8 P10 P7) ("x" P10 P4 H9 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" P7 P8 H9 H10) ("t" P7 P8 H1 H2) ("t" P6 H9) ("v" H10 H9 P7 P8) ("x" P4 P5 H10 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" P7 P8 H9 H10) ("t" P7 P6 H1 H2) ("t" P8 H9) ("v" H10 H9 P8 P7) ("x" P4 P5 H10 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P5 P4 P6 H1 H2) (P5 P6 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P5 P6 H2 H1)))
   ("+ 0.5" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P5 P4 P6 H1 H2) (P5 P6 P4 H2 H1))))
  :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P4 P5 H7 H1) ("t" P6 H8) ("v" H8 H7 P9 P10) ("x" P9 P10 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P5 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P4 P6 H2 H1))))
  :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 P5 H8 H1) ("t" P6 H9) ("v" H9 H8 P10 P7) ("x" P10 P4 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P5 P6 H7 H1) ("t" P8 H9) ("v" H9 H7 P10 P8) ("x" P10 P4 H2 H3*)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P5 P6 H2 H1) (P4 P5 P6 H1 H2)))) :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P4 P5 P6 H7 H8 H2) ("t" P9 H1) ("v" H7 H8 P10 P9) ("x" P10 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P5 P6 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 1.0" (:PERMUTE (P5 P4 P6 H1 H2) (P5 P6 P4 H1 H2))))
  :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 P4 P5 H8 H1 H2) ("t" P6 H9) ("v" H9 H8 P10 P7) ("x" P10 H3*)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P4 P5 P6 H7 H1 H2) ("t" P8 H9) ("v" H9 H7 P10 P8) ("x" P10 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" P7 H8 H9 P10) ("t" P7 P6 H1 H2) ("t" P4 P5 H8 H9) ("v" H8 H9 P10 P7) ("x" P10 H3*)))
 (:PERMUTATIONS
  ("+ 1.0" ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H2 H1)))
   ("+ 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P5 P6 P4 H1 H2) (P5 P6 P4 H2 H1))))
  :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P5 P6 H7 H1) ("t" P8 P4 H9 H2) ("v" H9 H7 P10 P8) ("x" P10 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS
  (("Sum" H7 P8 P9 H10) ("t" P6 H7) ("t" P8 H1) ("t" P9 H2) ("v" H10 H7 P9 P8)
   ("x" P4 P5 H10 H3*)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P6 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P5 P4 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P6 P4 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P4 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H2 H1)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P4 P6 H2 H1)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P6 H7) ("t" P5 H8) ("t" P9 H1) ("v" H8 H7 P10 P9)
   ("x" P10 P4 H2 H3*)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2)))
   ("+ 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P6 P4 P5 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P4 P5 H1 H2) (P5 P4 P6 H2 H1)))
   ("- 1.0" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H2 H1))))
  :TENSORS
  (("Sum" H7 P8 H9 P10) ("t" P6 H7) ("t" P8 H1) ("t" P4 P5 H9 H2) ("v" H9 H7 P10 P8)
   ("x" P10 H3*)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P6 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P6 P4 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P6 P5 H1 H2)))
   ("+ 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P5 P4 P6 H1 H2)))
   ("- 0.5" (:PERMUTE (P6 P5 P4 H1 H2) (P4 P5 P6 H1 H2))))
  :TENSORS
  (("Sum" H7 H8 P9 P10) ("t" P6 H7) ("t" P5 H8) ("t" P9 P4 H1 H2) ("v" H8 H7 P10 P9)
   ("x" P10 H3*))))