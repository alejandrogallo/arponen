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
  (("Sum" P5 H6 P7 H8) ("t" P5 H6) ("t" P7 H8) ("v" H8 H6 P7 P2) ("y" H3 H4 P5 P1))))