(ns ring-of-fire.burn-demo
  (:use [clojure.repl])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
    ;[ring-of-fire.propel-helper :refer :all]
            [ring-of-fire.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read in data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-in-csv
  "Reads in a csv given a string holding the path to a file
  as a lazy sequence of lazy sequences"
  [path-to-file]
  (with-open [reader (io/reader path-to-file)]
    (doall
      (csv/read-csv reader))))

(defn clean-row
  "Helper function to clean data. Converts one vector row of strings to longs"
  [row]
  (vec (map #(read-string %) row)))
#_(clean-row '("1 1 1"))

(defn clean
  "Cleans data in string format into a vector of vector of longs"
  [data]
  (vec (map #(clean-row %) data)))

(defn read-in-data
  "Returns a fully cleaned fire scar (a vector of vectors)"
  [path]
  (clean (read-in-csv path)))

(def fire-scar-a1-us (read-in-data "data/Outputs/a1-program1.csv"))
(def fire-scar-m1-us (read-in-data "data/Outputs/m1-program1.csv"))
(def fire-scar-g1-us (read-in-data "data/Outputs/g1-program1.csv"))

(def fuel-a1 (read-in-data "data/Outputs/a1-fuel.csv"))

(def m1-test (read-in-data "data/Outputs/m1-test.csv"))
(def k2-test (read-in-data "data/Outputs/k2-test.csv"))
(def a1-test (read-in-data "data/Outputs/a1-test.csv"))
(def g1-test (read-in-data "data/Outputs/g1-test.csv"))
(def r2-test (read-in-data "data/Outputs/r2-test.csv"))



(def fire-scar-a1 (read-in-data "data/Outputs/final-fire-scars/a1.csv"))
(def fire-scar-a2 (read-in-data "data/Outputs/final-fire-scars/a2.csv"))
(def fire-scar-g1 (read-in-data "data/Outputs/final-fire-scars/g1.csv"))
(def fire-scar-g2 (read-in-data "data/Outputs/final-fire-scars/g2.csv"))
(def fire-scar-k1 (read-in-data "data/Outputs/final-fire-scars/k1.csv"))
(def fire-scar-k2 (read-in-data "data/Outputs/final-fire-scars/k2.csv"))
(def fire-scar-m1 (read-in-data "data/Outputs/final-fire-scars/m1.csv"))
(def fire-scar-m2 (read-in-data "data/Outputs/final-fire-scars/m2.csv"))
(def fire-scar-r1 (read-in-data "data/Outputs/final-fire-scars/r1.csv"))
(def fire-scar-r2 (read-in-data "data/Outputs/final-fire-scars/r2.csv"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions to calculate percent error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn num-rows-quil
  "Returns the number of rows of a vector of vectors
  given a grid"
  [data]
  (count data))
#_(num-rows-quil [[1 0 0] [0 0 1]])

(defn num-columns-quil
  "Returns the number of rows of a vector of vectors
  given a grid"
  [data]
  (count (nth data 0)))
#_(num-columns-quil [[1 0 0] [0 0 1]])

(defn tot-cells-quil
  "Returns the total number of cells in one grid"
  [grid]
  ( * (num-rows-quil grid) (num-columns-quil grid)))
#_(tot-cells-quil [[1 0 0] [0 0 1]])

(defn abs-val
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn compare-grids-quil
  "Compares each cell in the two grids and
   returns a vector of differences where 1 means
   there was a difference, 0 refers to no difference"
  [evolved-scar final-scar]
  (vec (map #(abs-val (reduce - %))
                         (partition 2 (interleave (flatten evolved-scar) (flatten final-scar))))))
#_(compare-grids-quil [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])

(defn calculate-pct-difference
  "Calculates percent difference of two grids"
  [evolved-scar final-scar]
  (float (/ (reduce + (compare-grids-quil evolved-scar final-scar)) (tot-cells-quil final-scar))))
#_(calculate-pct-difference [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])
#_(calculate-pct-difference fire-scar-m1 fire-scar-m1-us)
#_(calculate-pct-difference fire-scar-a1 fire-scar-a1-us)
#_(calculate-pct-difference fire-scar-g1 fire-scar-g1-us)
#_(calculate-pct-difference fuel-a1 fire-scar-a1-us)

#_(calculate-pct-difference fire-scar-m1 m1-test)
#_(calculate-pct-difference fire-scar-k2 k2-test)

#_(calculate-pct-difference fire-scar-a1 a1-test)
#_(calculate-pct-difference fire-scar-g1 g1-test)
#_(calculate-pct-difference fire-scar-r2 r2-test)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Run quil program on read in data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def selected-fire r2-test)

(def rows (num-rows-quil selected-fire))
(def columns (num-columns-quil selected-fire))


(defn grid-to-print []
  selected-fire)

(defn setup []
  (q/no-stroke)
  (q/frame-rate 10)
  (grid-to-print))

(defn step-forward [grid]
  ;; go through entire grid
  (for [x (range rows)]
    (for [y (range columns)]
      (let [state (nth (nth grid x) y)]
        ;(let [state (int (rand 3))]
        state))))

(def scale 5)

(defn draw-world [grid]
  (dotimes [r rows]
    (dotimes [c columns]
      (let [state (nth (nth grid r) c)]
        (q/fill
          (cond
            ;; if burning then red
            (= state 1)
            (q/color 255 0 0)
            ;; if burned then yellow
            (= state 2)
            (q/color 255 255 0)
            ;; if available then white
            (= state 0)
            (q/color 255 255 255))))

      (q/rect (* c scale) (* r scale) scale scale))))


(q/defsketch run
             :host "host"
             :size [(* columns scale) (* rows scale)]
             :setup setup
             :update step-forward
             :draw draw-world
             :middleware [m/fun-mode])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An attempt to use run-fire to live update a fire in quil
; DOES NOT WORK YET

#_(def test-argmap {:instructions            fire-instructions
                    :error-function          fire-error-function
                    :max-generations         1000
                    :population-size         5
                    :max-initial-plushy-size 20
                    :step-limit              100
                    :parent-selection        :lexicase
                    :tournament-size         5
                    :time-step               100
                    :fire-selection          1})


#_(update-grid (:m1 initial-fire-grids-flattened) "m1" 100 (make-split-program '(NBD)) test-argmap (:m1 initial-time-grids-flattened))
#_(:cell-grid (update-grid (:m1 initial-fire-grids-flattened) "m1" 100 (make-split-program '(NBD)) test-argmap (:m1 initial-time-grids-flattened)))

;(def rows (num-rows "m2"))
;(def columns (num-columns "m2"))

;(def state {:flattened-grid (:m1 initial-fire-grids-flattened)
;            :fire-name "m1"
;            :time-step 100
;            :program (make-split-program '(NBD))
;            :argmap-quil test-argmap
;            :flattened-time-grid (:m1 initial-time-grids-flattened)
;            :final-grid (:m1 initial-fire-grids)})
;
;(defn setup []
;  (q/no-stroke)
;  (q/frame-rate 10)
;  state)
;
;(defn next-step [state]
;  (let [final-grid (:cell-grid (update-grid (:flattened-grid state) (:fire-name state) (:time-step state) (:program state) (:argmap-quil state) (:flattened-time-grid state)))]
;    (assoc state :final-grid (partition (num-columns (:fire-name state)) final-grid))))
;#_(next-step (:m1 initial-fire-grids-flattened) "m1" 100 (make-split-program '(NBD)) test-argmap (:m1 initial-time-grids-flattened))
;#_(next-step state)
;
;(defn draw-grid [state]
;  (dotimes [r rows]
;    (dotimes [c columns]
;      (let [value (nth (nth (:final-grid state) r) c)]
;        ;; if burning then red
;        (q/fill
;          (cond
;            ;; if burning then red
;            (= value 1)
;            (q/color 255 0 0)
;            ;; if burned then yellow
;            (= value 2)
;            (q/color 255 255 0)
;            ;; if available then white
;            (= value 0)
;            (q/color 255 255 255))))
;
;      ;(q/rect (* r 20) (* c 20) 20 20))))
;      (q/rect (* r 1) (* c 1) 1 1))))
;
;(q/defsketch run-grid
;             :host "host"
;             :size [1000 1000]
;             :setup setup
;             :update next-step
;             :draw draw-grid
;             :middleware [m/fun-mode])

