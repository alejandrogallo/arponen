((:PERMUTATIONS ("+ 1.0") :TENSORS (("v" H3 H4 P1 P2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("f" H3 P2) ("y" H4 P1)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" H5) ("v" H3 H4 H5 P2) ("y" H5 P1)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5) ("v" H3 P5 P1 P2) ("y" H4 P5)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" H5) ("f" H3 H5) ("y" H5 H4 P1 P2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5) ("f" P5 P2) ("y" H3 H4 P5 P1)))
 (:PERMUTATIONS ("- 0.5") :TENSORS (("Sum" H5 H6) ("v" H3 H4 H5 H6) ("y" H6 H5 P1 P2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 H6) ("v" H3 P5 H6 P2) ("y" H6 H4 P5 P1)))
 (:PERMUTATIONS ("- 0.5") :TENSORS (("Sum" P5 P6) ("v" P5 P6 P1 P2) ("y" H3 H4 P6 P5)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS (("Sum" P5 H6) ("f" P5 H6) ("y" H6 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 H7) ("v" H3 P5 H6 H7) ("y" H7 H6 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7) ("v" P5 P6 H7 P2) ("y" H7 H3 H4 P6 P5 P1)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6) ("t" P5 H6) ("v" H3 H4 P5 P2) ("y" H6 P1)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6) ("t" P5 H6) ("v" H6 H3 P1 P2) ("y" H4 P5)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 H6) ("t" P5 H6) ("v" H6 H3 P5 P2) ("y" H4 P1)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6) ("f" H3 P5) ("t" P5 H6) ("y" H6 H4 P1 P2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" H5 P6) ("f" H5 P2) ("t" P6 H5) ("y" H3 H4 P6 P1)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 H6 H7) ("t" P5 H6) ("v" H3 H4 H7 P5) ("y" H7 H6 P1 P2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 H6 H7) ("t" P5 H6) ("v" H6 H3 H7 P2) ("y" H7 H4 P5 P1)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 H7) ("t" P5 H6) ("v" H6 H3 H7 P5) ("y" H7 H4 P1 P2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 H6 P7) ("t" P5 H6) ("v" H3 P7 P5 P2) ("y" H6 H4 P7 P1)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 H6 P7) ("t" P5 H6) ("v" H6 P7 P1 P2) ("y" H3 H4 P7 P5)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7) ("t" P5 H6) ("v" H6 P7 P5 P2) ("y" H3 H4 P7 P1)))
 (:PERMUTATIONS ("- 1.0") :TENSORS
  (("Sum" H5 H6 P7) ("f" H5 H6) ("t" P7 H5) ("y" H6 H3 H4 P7 P1 P2)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 P6 H7) ("f" P5 P6) ("t" P6 H7) ("y" H7 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 H7 H8) ("t" P5 H6) ("v" H6 H3 H7 H8) ("y" H8 H7 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("v" H3 P7 H8 P5) ("y" H8 H6 H4 P7 P1 P2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("v" H6 P7 H8 P2) ("y" H8 H3 H4 P7 P5 P1)))
 (:PERMUTATIONS ("- 1.0") :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("v" H6 P7 H8 P5) ("y" H8 H3 H4 P7 P1 P2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 P8) ("t" P5 H6) ("v" P7 P8 P5 P2) ("y" H6 H3 H4 P8 P7 P1)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H3 H4 P6 P2) ("y" H8 H7 P5 P1)))
 (:PERMUTATIONS ("- 0.25") :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H3 H4 P5 P6) ("y" H8 H7 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H8 H3 P1 P2) ("y" H7 H4 P6 P5)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H8 H3 P6 P2) ("y" H7 H4 P5 P1)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H8 H3 P5 P6) ("y" H7 H4 P1 P2)))
 (:PERMUTATIONS ("- 0.25") :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H7 H8 P1 P2) ("y" H3 H4 P6 P5)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("t" P5 P6 H7 H8) ("v" H7 H8 P6 P2) ("y" H3 H4 P5 P1)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8) ("f" H3 P5) ("t" P6 P5 H7 H8) ("y" H8 H7 H4 P6 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" H5 P6 P7 H8) ("f" H5 P2) ("t" P6 P7 H8 H5) ("y" H8 H3 H4 P7 P6 P1)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" H5 P6 P7 H8) ("f" H5 P6) ("t" P7 P6 H8 H5) ("y" H8 H3 H4 P7 P1 P2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 H9) ("t" P5 P6 H7 H8) ("v" H3 H4 H9 P6) ("y" H9 H8 H7 P5 P1 P2)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 P6 H7 H8 H9) ("t" P5 P6 H7 H8) ("v" H8 H3 H9 P2) ("y" H9 H7 H4 P6 P5 P1)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 H9) ("t" P5 P6 H7 H8) ("v" H8 H3 H9 P6) ("y" H9 H7 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.25" ("- 0.25" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 H9) ("t" P5 P6 H7 H8) ("v" H7 H8 H9 P2) ("y" H9 H3 H4 P6 P5 P1)))
 (:PERMUTATIONS ("- 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 H9) ("t" P5 P6 H7 H8) ("v" H7 H8 H9 P6) ("y" H9 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 P6 H7 H8 P9) ("t" P5 P6 H7 H8) ("v" H3 P9 P6 P2) ("y" H8 H7 H4 P9 P5 P1)))
 (:PERMUTATIONS ("- 0.25" ("+ 0.25" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9) ("t" P5 P6 H7 H8) ("v" H3 P9 P5 P6) ("y" H8 H7 H4 P9 P1 P2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 P9) ("t" P5 P6 H7 H8) ("v" H8 P9 P1 P2) ("y" H7 H3 H4 P9 P6 P5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9) ("t" P5 P6 H7 H8) ("v" H8 P9 P6 P2) ("y" H7 H3 H4 P9 P5 P1)))
 (:PERMUTATIONS ("- 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 P9) ("t" P5 P6 H7 H8) ("v" H8 P9 P5 P6) ("y" H7 H3 H4 P9 P1 P2)))
 (:PERMUTATIONS ("- 0.0833333333333" ("+ 0.0833333333333" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2))))
  :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H3 H4 P7 P2) ("y" H10 H9 H8 P6 P5 P1)))
 (:PERMUTATIONS ("- 0.0833333333333") :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H3 H4 P6 P7) ("y" H10 H9 H8 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.0833333333333" ("- 0.0833333333333" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2))))
  :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H10 H3 P1 P2) ("y" H9 H8 H4 P7 P6 P5)))
 (:PERMUTATIONS
  ("- 0.25" ("+ 0.25" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("+ 0.25" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("- 0.25" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H10 H3 P7 P2) ("y" H9 H8 H4 P6 P5 P1)))
 (:PERMUTATIONS ("- 0.25" ("+ 0.25" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H10 H3 P6 P7) ("y" H9 H8 H4 P5 P1 P2)))
 (:PERMUTATIONS ("- 0.0833333333333") :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H9 H10 P1 P2) ("y" H8 H3 H4 P7 P6 P5)))
 (:PERMUTATIONS ("+ 0.25" ("- 0.25" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H9 H10 P7 P2) ("y" H8 H3 H4 P6 P5 P1)))
 (:PERMUTATIONS ("+ 0.25") :TENSORS
  (("Sum" P5 P6 P7 H8 H9 H10) ("t" P5 P6 P7 H8 H9 H10) ("v" H9 H10 P6 P7) ("y" H8 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("v" H3 H4 P7 P5) ("y" H8 H6 P1 P2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("v" H8 H3 P5 P2) ("y" H6 H4 P7 P1)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("v" H8 H3 P7 P5) ("y" H6 H4 P1 P2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("v" H8 H6 P1 P2) ("y" H3 H4 P7 P5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("v" H8 H6 P7 P2) ("y" H3 H4 P5 P1)))
 (:PERMUTATIONS ("- 1.0") :TENSORS
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("f" H8 P5) ("y" H6 H3 H4 P7 P1 P2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8 H9) ("t" P5 H6) ("t" P7 H8) ("v" H8 H3 H9 P5) ("y" H9 H6 H4 P7 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8 H9) ("t" P5 H6) ("t" P7 H8) ("v" H8 H6 H9 P2) ("y" H9 H3 H4 P7 P5 P1)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 H6 P7 H8 H9) ("t" P5 H6) ("t" P7 H8) ("v" H8 H6 H9 P7) ("y" H9 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8 P9) ("t" P5 H6) ("t" P7 H8) ("v" H3 P9 P7 P5) ("y" H8 H6 H4 P9 P1 P2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8 P9) ("t" P5 H6) ("t" P7 H8) ("v" H8 P9 P5 P2) ("y" H6 H3 H4 P9 P7 P1)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 H6 P7 H8 P9) ("t" P5 H6) ("t" P7 H8) ("v" H8 P9 P7 P5) ("y" H6 H3 H4 P9 P1 P2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H3 H4 P9 P6)
   ("y" H10 H8 H7 P5 P1 P2)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H8 H3 P9 P2)
   ("y" H10 H7 H4 P6 P5 P1)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H8 H3 P9 P6)
   ("y" H10 H7 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.25" ("- 0.25" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H7 H8 P9 P2)
   ("y" H10 H3 H4 P6 P5 P1)))
 (:PERMUTATIONS ("- 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H7 H8 P9 P6)
   ("y" H10 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS
  ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))
   ("- 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P2 P1)))
   ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H4 H3 P1 P2))))
  :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H3 P6 P2)
   ("y" H8 H7 H4 P9 P5 P1)))
 (:PERMUTATIONS ("- 0.25" ("+ 0.25" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H3 P5 P6)
   ("y" H8 H7 H4 P9 P1 P2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H3 P9 P6)
   ("y" H8 H7 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H8 P1 P2)
   ("y" H7 H3 H4 P9 P6 P5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H8 P6 P2)
   ("y" H7 H3 H4 P9 P5 P1)))
 (:PERMUTATIONS ("- 0.5") :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H8 P5 P6)
   ("y" H7 H3 H4 P9 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H8 P9 P2)
   ("y" H7 H3 H4 P6 P5 P1)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" P5 P6 H7 H8 P9 H10) ("t" P5 P6 H7 H8) ("t" P9 H10) ("v" H10 H8 P9 P6)
   ("y" H7 H3 H4 P5 P1 P2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (H3 H4 P1 P2) (H4 H3 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8 P9 H10) ("t" P5 H6) ("t" P7 H8) ("t" P9 H10) ("v" H10 H3 P7 P5)
   ("y" H8 H6 H4 P9 P1 P2)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (H3 H4 P2 P1) (H3 H4 P1 P2)))) :TENSORS
  (("Sum" P5 H6 P7 H8 P9 H10) ("t" P5 H6) ("t" P7 H8) ("t" P9 H10) ("v" H10 H8 P5 P2)
   ("y" H6 H3 H4 P9 P7 P1)))
 (:PERMUTATIONS ("- 1.0") :TENSORS
  (("Sum" P5 H6 P7 H8 P9 H10) ("t" P5 H6) ("t" P7 H8) ("t" P9 H10) ("v" H10 H8 P9 P5)
   ("y" H6 H3 H4 P7 P1 P2))))