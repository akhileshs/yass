(ns yass.solve
  (:use [clojure.set])
  (:require [clojure.core.typed :as t]))

(t/ann constraints [(t/Seq Number) Number -> (t/Set Number)])
(defn constraints
  "Returns a set of numbers which can't be filled at the given position"
  [s i]
  (let [gridList (t/ann-form (partition 9 s) (t/Seq (t/Seq Number))),       ;; create a list of lists
        row (t/ann-form (/ i 9) Number),                    ;; row no
        column (t/ann-form (mod i 9) Number),               ;; column no
        columnGroup (t/ann-form (/ column 3) Number),            ;; column group no
        rowGroup (t/ann-form (/ row 3) Number),               ;; row group no
        every-nth-item (fn [gridList i] (map #(nth % i) gridList))   ;; return every nth item in the list of lists.
        group-column (t/ann-form (every-nth-item (map #(partition 3 %) gridList) columnGroup) (t/Seq Number)) ;; returns all nos. in the column group
        group (t/ann-form (take 3 (drop (* 3 (int rowGroup)) group-column)) (t/Seq (t/Seq Number)))]                  ;; returns all nos. in the given number's group.
     (into #{} (flatten [(nth gridList row) (every-nth-item gridList column) group])))) ;; finally return the constaint numbers

(defn solve
  [sudokuGrid]
  (if (.contains sudokuGrid 0)        ;; check if the grid is solved
    (let [i (.indexOf sudokuGrid 0)   ;; index of the first blank no in the grid (0)
          inject #(concat (take %2 %1) [%3] (drop (inc %2) %1))]    ;; inject values on a per-value basis (each value belongs to the unconstrained set
      (flatten (map #(solve (inject sudokuGrid i %))
                    (difference #{1 2 3 4 5 6 7 8 9} (constraints sudokuGrid i)))))     ;; check each injected value and see if the board is solved.
    sudokuGrid))
