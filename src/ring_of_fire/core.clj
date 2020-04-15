(ns ring-of-fire.core
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]))

#_(read-in-csv "data/Arrowhead/Fire1/FinalScarGrid.csv")



(defn error
  "Compares each cell in the two grids and finds the sum of differences between the two"
  [evolved-scar final-scar]
  (reduce + (map #(Math/abs (reduce - %))
                 (partition 2 (interleave (flatten evolved-scar) (flatten final-scar))))))
;; this should have an error of 1 as only one value is different
#_(error [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])
;; this should have an error of 0 since it's the same fire scar twice
#_(error arrowhead1-final-scar arrowhead1-final-scar)