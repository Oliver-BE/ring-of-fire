(ns ring-of-fire.burn-demo
  (:use [clojure.repl])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            ;[ring-of-fire.propel-helper :refer :all]
            [ring-of-fire.core :refer :all]))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read in data

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

(def fire-scar (read-in-data "data/Outputs/output.csv"))

(defn num-rows-quil
  "Returns the number of rows of a vector of vectors
  given a grid"
  [data]
  (count data))

(defn num-columns-quil
  "Returns the number of rows of a vector of vectors
  given a grid"
  [data]
  (count (nth data 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def rows (num-rows-quil fire-scar))
(def columns (num-columns-quil fire-scar))


(defn grid-to-print []
  fire-scar)

(defn setup-simple []
  (q/no-stroke)
  (q/frame-rate 10)
  (grid-to-print))

;(defn m1-grid []
;  (:m1 final-scar-grid-master))
;
;(defn setup-final-scar []
;  (q/no-stroke)
;  (q/frame-rate 10)
;  (m1-grid))

(defn step-forward [grid]
  ;; go through entire grid
  (for [x (range rows)]
    (for [y (range columns)]
      (let [state (nth (nth grid x) y)]
        ;(let [state (int (rand 3))]
        state))))

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
            ;; if available then green
            (= state 0)
            (q/color 255 255 255))))
            ;(q/color 34 139 34))))

      ;(q/rect (* r 20) (* c 20) 20 20))))
      (q/rect (* c 10) (* r 10) 10 10))))

(q/defsketch run
             :host "host"
             :size [(* columns 10) (* rows 10)]
             :setup setup-simple
             :update step-forward
             :draw draw-world
             :middleware [m/fun-mode])

