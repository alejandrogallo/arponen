((:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5) ("v" H5 P4 H1 H2) ("x" P3 H5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5) ("v" P3 P4 H1 P5) ("x" P5 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5) ("f" H5 H1) ("x" P3 P4 H5 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5) ("f" P4 P5) ("x" P5 P3 H1 H2)))
 (:PERMUTATIONS ("- 0.5") :TENSORS (("Sum" H5 H6) ("v" H5 H6 H1 H2) ("x" P3 P4 H6 H5)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6) ("v" H5 P4 H1 P6) ("x" P6 P3 H5 H2)))
 (:PERMUTATIONS ("- 0.5") :TENSORS (("Sum" P5 P6) ("v" P3 P4 P5 P6) ("x" P6 P5 H1 H2)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS (("Sum" H5 P6) ("f" H5 P6) ("x" P6 P3 P4 H5 H1 H2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 H6 P7) ("v" H5 H6 H1 P7) ("x" P7 P3 P4 H6 H5 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7) ("v" H5 P4 P6 P7) ("x" P7 P6 P3 H5 H1 H2)))
 (:PERMUTATIONS ("+ 0.25") :TENSORS
  (("Sum" H5 H6 P7 P8) ("v" H5 H6 P7 P8) ("x" P8 P7 P3 P4 H6 H5 H1 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 H6) ("t" P4 H5) ("v" H6 H5 H1 H2) ("x" P3 H6)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" P5 H6) ("t" P5 H2) ("v" H6 P4 H1 P5) ("x" P3 H6)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6) ("t" P3 H5) ("v" H5 P4 H1 P6) ("x" P6 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 P6) ("t" P5 H1) ("v" P3 P4 P6 P5) ("x" P6 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6) ("f" H5 P6) ("t" P6 H1) ("x" P3 P4 H5 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6) ("f" H5 P6) ("t" P4 H5) ("x" P6 P3 H1 H2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 H6 H7) ("t" P5 H2) ("v" H6 H7 H1 P5) ("x" P3 P4 H7 H6)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 H6 P7) ("t" P4 H5) ("v" H6 H5 H1 P7) ("x" P7 P3 H6 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 H6 H7) ("t" P5 H6) ("v" H7 H6 H1 P5) ("x" P3 P4 H7 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" P5 H6 P7) ("t" P5 H1) ("v" H6 P4 P7 P5) ("x" P7 P3 H6 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7) ("t" P3 H5) ("v" H5 P4 P6 P7) ("x" P7 P6 H1 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 H6 P7) ("t" P5 H6) ("v" H6 P4 P7 P5) ("x" P7 P3 H1 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 H1) ("v" H6 H7 P8 P5) ("x" P8 P3 P4 H7 H6 H2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 H6 P7 P8) ("t" P4 H5) ("v" H6 H5 P7 P8) ("x" P8 P7 P3 H6 H1 H2)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 H6) ("v" H7 H6 P8 P5) ("x" P8 P3 P4 H7 H1 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6) ("f" H5 P6) ("t" P6 P4 H1 H2) ("x" P3 H5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6) ("f" H5 P6) ("t" P3 P4 H5 H1) ("x" P6 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 H6 P7) ("t" P3 P4 H5 H2) ("v" H6 H5 H1 P7) ("x" P7 H6)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" P5 H6 H7) ("t" P5 P4 H6 H2) ("v" H7 H6 H1 P5) ("x" P3 H7)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 H6 P7) ("t" P3 P4 H5 H6) ("v" H5 H6 H1 P7) ("x" P7 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" P5 H6 P7) ("t" P5 P3 H1 H2) ("v" H6 P4 P7 P5) ("x" P7 H6)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 P6 H7) ("t" P5 P6 H1 H2) ("v" H7 P4 P5 P6) ("x" P3 H7)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" P5 H6 P7) ("t" P5 P3 H6 H1) ("v" H6 P4 P7 P5) ("x" P7 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 P4 H1 H2) ("v" H6 H7 P8 P5) ("x" P8 P3 H7 H6)))
 (:PERMUTATIONS ("- 0.25") :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H1 H2) ("v" H7 H8 P5 P6) ("x" P3 P4 H8 H7)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 H6 P7 P8) ("t" P3 P4 H5 H1) ("v" H6 H5 P7 P8) ("x" P8 P7 H6 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" P5 H6 H7 P8) ("t" P5 P4 H6 H1) ("v" H7 H6 P8 P5) ("x" P8 P3 H7 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H1) ("v" H8 H7 P5 P6) ("x" P3 P4 H8 H2)))
 (:PERMUTATIONS ("- 0.25") :TENSORS
  (("Sum" H5 H6 P7 P8) ("t" P3 P4 H5 H6) ("v" H5 H6 P7 P8) ("x" P8 P7 H1 H2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 P4 H6 H7) ("v" H6 H7 P8 P5) ("x" P8 P3 H1 H2)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 P3 P4 H6 H1 H2) ("v" H7 H6 P8 P5) ("x" P8 H7)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 P4 H7 H1 H2) ("v" H8 H7 P5 P6) ("x" P3 H8)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 P3 P4 H6 H7 H1) ("v" H6 H7 P8 P5) ("x" P8 H2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" H5 P6 H7) ("t" P4 H5) ("t" P6 H2) ("v" H7 H5 H1 P6) ("x" P3 H7)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 H6 P7) ("t" P4 H5) ("t" P3 H6) ("v" H6 H5 H1 P7) ("x" P7 H2)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" P5 P6 H7) ("t" P5 H1) ("t" P6 H2) ("v" H7 P4 P6 P5) ("x" P3 H7)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7) ("t" P3 H5) ("t" P6 H1) ("v" H5 P4 P7 P6) ("x" P7 H2)))
 (:PERMUTATIONS ("+ 0.25" ("- 0.25" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 H1) ("t" P6 H2) ("v" H7 H8 P6 P5) ("x" P3 P4 H8 H7)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 H7 P8) ("t" P4 H5) ("t" P6 H1) ("v" H7 H5 P8 P6) ("x" P8 P3 H7 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 H1) ("t" P6 H7) ("v" H8 H7 P6 P5) ("x" P3 P4 H8 H2)))
 (:PERMUTATIONS ("- 0.25" ("+ 0.25" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 H6 P7 P8) ("t" P4 H5) ("t" P3 H6) ("v" H6 H5 P7 P8) ("x" P8 P7 H1 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("t" P4 H5) ("t" P6 H7) ("v" H7 H5 P8 P6) ("x" P8 P3 H1 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("t" P3 P4 H5 H2) ("t" P6 H1) ("v" H7 H5 P8 P6) ("x" P8 H7)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" P5 H6 P7 H8) ("t" P5 P4 H6 H2) ("t" P7 H1) ("v" H8 H6 P7 P5) ("x" P3 H8)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 H6 P7 P8) ("t" P3 P4 H5 H6) ("t" P7 H1) ("v" H5 H6 P8 P7) ("x" P8 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" P5 H6 H7 P8) ("t" P5 P3 H1 H2) ("t" P4 H6) ("v" H7 H6 P8 P5) ("x" P8 H7)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H1 H2) ("t" P4 H7) ("v" H8 H7 P5 P6) ("x" P3 H8)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P4 H1 H2) ("t" P6 H7) ("v" H8 H7 P6 P5) ("x" P3 H8)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" P5 H6 H7 P8) ("t" P5 P3 H6 H1) ("t" P4 H7) ("v" H7 H6 P8 P5) ("x" P8 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("t" P3 P4 H5 H1) ("t" P6 H7) ("v" H7 H5 P8 P6) ("x" P8 H2)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("t" P4 H5) ("t" P6 H1) ("t" P7 H2) ("v" H8 H5 P7 P6) ("x" P3 H8)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 H6 P7 P8) ("t" P4 H5) ("t" P3 H6) ("t" P7 H1) ("v" H6 H5 P8 P7) ("x" P8 H2))))