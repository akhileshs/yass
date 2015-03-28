(ns yass.solve
  (:use [clojure.set]
        [clojure.core.typed]))

(defn constraints
  "Returns a set of numbers which can't be filled at the given position"
  [s :- (Vec Number), i :- AnyInteger]
  (let [gridList :- (Seq (Seq Number)) (partition 9 s),       ;; create a list of lists
        row :- Number (/ i 9),                    ;; row no
        column :- Number (mod i 9),               ;; column no
        columnGroup :- Number (/ column 3),            ;; column group no
        rowGroup :- Number (/ row 3),               ;; row group no
        every-nth-item (fn [gridList :- (Seq (Seq Number)), i :- Number] (map #(nth % i) gridList))            ;; return every nth item in the list of lists.

        group-column :- (Seq Number) (every-nth-item (map #(partition 3 %) gridList) columnGroup) ;; returns all nos. in the column group
        group :- (Seq Number) (take 3 (drop (* 3 (int rowGroup)) group-column))]                  ;; returns all nos. in the given number's group.
    (into #{} (flatten [(nth gridList row) (every-nth-item gridList column) group])))) ;; finally return the constaint numbers

(defn solve
  [sudokuGrid :- (Vec Number)]
  (if (.contains sudokuGrid 0)        ;; check if the grid is solved
    (let [i (.indexOf sudokuGrid 0)   ;; index of the first blank no in the grid (0)
          inject #(concat (take %2 %1) [%3] (drop (inc %2) %1))]    ;; inject values on a per-value basis (each value belongs to the unconstrained set
      (flatten (map #(solve (inject sudokuGrid i %))
                    (difference #{1 2 3 4 5 6 7 8 9} (constraints sudokuGrid i)))))     ;; check each injected value and see if the board is solved.
    sudokuGrid))
