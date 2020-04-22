  `(ns ring-of-fire.core
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


;;;;;;;;;;;;;;;;
;; Test pairs ;;
;;;;;;;;;;;;;;;;

;; each fire has input (forest, ignition cell, elevation, slope, weather)
;; matched with its output (final fire scar)

(def test-pairs [[[arrowhead1-forest arrowhead1-ignition-cell arrowhead1-elevation arrowhead1-slope arrowhead1-weather] arrowhead1-final-scar]
                 [[arrowhead2-forest arrowhead2-ignition-cell arrowhead2-elevation arrowhead2-slope arrowhead2-weather] arrowhead2-final-scar]
                 [[kootenay1-forest kootenay1-ignition-cell kootenay1-elevation kootenay1-slope kootenay1-weather] kootenay1-final-scar]
                 [[kootenay2-forest kootenay2-ignition-cell kootenay2-elevation kootenay2-slope kootenay2-weather] kootenay2-final-scar]
                 [[glacier1-forest glacier1-ignition-cell glacier1-elevation glacier1-slope glacier1-weather] glacier1-final-scar]
                 [[glacier2-forest glacier2-ignition-cell glacier2-elevation glacier2-slope glacier2-weather] glacier2-final-scar]
                 [[mica1-forest mica1-ignition-cell mica1-elevation mica1-slope mica1-weather] mica1-final-scar]
                 [[mica2-forest mica2-ignition-cell mica2-elevation mica2-slope mica2-weather] mica2-final-scar]
                 [[revelstoke1-forest revelstoke1-ignition-cell revelstoke1-elevation revelstoke1-slope revelstoke1-weather] revelstoke1-final-scar]
                 [[revelstoke2-forest revelstoke2-ignition-cell revelstoke2-elevation revelstoke2-slope revelstoke2-weather] revelstoke2-final-scar]])