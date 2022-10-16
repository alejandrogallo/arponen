((:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P3 P4 H7 H5) ("v" H8 H7 H1 H2) ("x" P6 H8)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P6 P4 H7 H5) ("v" H8 H7 H1 H2) ("x" P3 H8)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P6 P4 H7 H8) ("v" H7 H8 H1 H2) ("x" P3 H5)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H1 H2))))
  :TENSORS (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P6 P3 H5 H2) ("v" H7 P4 H1 P8) ("x" P8 H7)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H2 H1)))
   ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H1 H2))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P3 H5 H2) ("v" H8 P4 H1 P7) ("x" P6 H8)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H2 H1)))
   ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P4 P3 H1 H2))))
  :TENSORS (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P6 P3 H7 H2) ("v" H7 P4 H1 P8) ("x" P8 H5)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P6 H5 H2) ("v" H8 P4 H1 P7) ("x" P3 H8)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P6 H8 H2) ("v" H8 P4 H1 P7) ("x" P3 H5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P3 P4 H7 H2) ("v" H8 H7 H5 H1) ("x" P6 H8)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P3 P4 H5 H2) ("v" H7 P6 H1 P8) ("x" P8 H7)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H2 H1) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P3 P4 H7 H2) ("v" H7 P6 H1 P8) ("x" P8 H5)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P6 P4 H7 H2) ("v" H8 H7 H5 H1) ("x" P3 H8)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P4 H5 H2) ("v" H8 P6 H1 P7) ("x" P3 H8)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H2 H1) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H2 H1) (P3 P4 H1 H2))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P4 H8 H2) ("v" H8 P6 H1 P7) ("x" P3 H5)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P6 P3 H7 H5) ("v" H7 P4 H1 P8) ("x" P8 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P3 H8 H5) ("v" H8 P4 H1 P7) ("x" P6 H2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P6 H8 H5) ("v" H8 P4 H1 P7) ("x" P3 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P3 P4 H7 H8) ("v" H7 H8 H5 H1) ("x" P6 H2)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P3 P4 H7 H5) ("v" H7 P6 H1 P8) ("x" P8 H2)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 H7 H8) ("y+" H5 P6) ("t" P6 P4 H7 H8) ("v" H7 H8 H5 H1) ("x" P3 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P4 H8 H5) ("v" H8 P6 H1 P7) ("x" P3 H2)))
 (:PERMUTATIONS ("+ 1.0") :TENSORS
  (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P6 H1 H2) ("v" P3 P4 P8 P7) ("x" P8 H5)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P6 P3 H1 H2) ("v" H7 P4 H5 P8) ("x" P8 H7)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P3 H1 H2) ("v" H8 P4 H5 P7) ("x" P6 H8)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P3 H1 H2) ("v" P6 P4 P8 P7) ("x" P8 H5)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P6 H1 H2) ("v" H8 P4 H5 P7) ("x" P3 H8)))
 (:PERMUTATIONS ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P8 H1 H2) ("v" P6 P4 P7 P8) ("x" P3 H5)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))) :TENSORS
  (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P4 H1 H2) ("v" H8 P6 H5 P7) ("x" P3 H8)))
 (:PERMUTATIONS ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P6 H5 H1) ("v" P3 P4 P8 P7) ("x" P8 H2)))
 (:PERMUTATIONS ("+ 0.5" ("- 0.5" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P8 H5 H1) ("v" P3 P4 P7 P8) ("x" P6 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P6 P3 H7 H1) ("v" H7 P4 H5 P8) ("x" P8 H2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P3 H8 H1) ("v" H8 P4 H5 P7) ("x" P6 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H1 H2)))
   ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))
   ("- 1.0" (:PERMUTE (P3 P4 H1 H2) (P4 P3 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P3 H5 H1) ("v" P6 P4 P8 P7) ("x" P8 H2)))
 (:PERMUTATIONS
  ("- 1.0" ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P6 H8 H1) ("v" H8 P4 H5 P7) ("x" P3 H2)))
 (:PERMUTATIONS
  ("- 0.5" ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("+ 0.5" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("- 0.5" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 P8) ("y+" H5 P6) ("t" P7 P8 H5 H1) ("v" P6 P4 P7 P8) ("x" P3 H2)))
 (:PERMUTATIONS ("- 1.0" ("+ 1.0" (:PERMUTE (P3 P4 H1 H2) (P3 P4 H2 H1)))) :TENSORS
  (("Sum" H5 P6 H7 P8) ("y+" H5 P6) ("t" P3 P4 H7 H1) ("v" H7 P6 H5 P8) ("x" P8 H2)))
 (:PERMUTATIONS
  ("+ 1.0" ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H1 H2)))
   ("- 1.0" (:PERMUTE (P4 P3 H1 H2) (P4 P3 H2 H1)))
   ("+ 1.0" (:PERMUTE (P4 P3 H1 H2) (P3 P4 H2 H1))))
  :TENSORS (("Sum" H5 P6 P7 H8) ("y+" H5 P6) ("t" P7 P4 H8 H1) ("v" H8 P6 H5 P7) ("x" P3 H2))))