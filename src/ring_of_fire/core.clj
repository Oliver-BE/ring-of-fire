(ns ring-of-fire.core
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]
            [ring-of-fire.propel-helper :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRE PROPEL FUNCTIONS     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master Data Dictionaries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def forest-master {:a1 arrowhead1-forest
                    :a2 arrowhead2-forest
                    :k1 kootenay1-forest
                    :k2 kootenay2-forest
                    :g1 glacier1-forest
                    :g2 glacier2-forest
                    :m1 mica1-forest
                    :m2 mica2-forest
                    :r1 revelstoke1-forest
                    :r2 revelstoke2-forest
                    })

(def weather-master {:a1 arrowhead1-weather
                     :a2 arrowhead2-weather
                     :k1 kootenay1-weather
                     :k2 kootenay2-weather
                     :g1 glacier1-weather
                     :g2 glacier2-weather
                     :m1 mica1-weather
                     :m2 mica2-weather
                     :r1 revelstoke1-weather
                     :r2 revelstoke2-weather
                     })

(def elevation-master {:a1 arrowhead1-elevation
                       :a2 arrowhead2-elevation
                       :k1 kootenay1-elevation
                       :k2 kootenay2-elevation
                       :g1 glacier1-elevation
                       :g2 glacier2-elevation
                       :m1 mica1-elevation
                       :m2 mica2-elevation
                       :r1 revelstoke1-elevation
                       :r2 revelstoke2-elevation
                       })

;; note that arrowhead 2 slope is messed up
(def slope-master {:a1 arrowhead1-slope
                   :a2 arrowhead1-slope
                   :k1 kootenay1-slope
                   :k2 kootenay2-slope
                   :g1 glacier1-slope
                   :g2 glacier2-slope
                   :m1 mica1-slope
                   :m2 mica2-slope
                   :r1 revelstoke1-slope
                   :r2 revelstoke2-slope
                   })

(def ignition-cell-master {:a1 arrowhead1-ignition-cell
                           :a2 arrowhead2-ignition-cell
                           :k1 kootenay1-ignition-cell
                           :k2 kootenay2-ignition-cell
                           :g1 glacier1-ignition-cell
                           :g2 glacier2-ignition-cell
                           :m1 mica1-ignition-cell
                           :m2 mica2-ignition-cell
                           :r1 revelstoke1-ignition-cell
                           :r2 revelstoke2-ignition-cell
                           })

;; a grid where 0 refers to non-fuel, 1 refers to burnable / fuel
(def fuel-master {:a1 (create-fuel-grid "a1")
                  :a2 (create-fuel-grid "a2")
                  :k1 (create-fuel-grid "k1")
                  :k2 (create-fuel-grid "k2")
                  :g1 (create-fuel-grid "g1")
                  :g2 (create-fuel-grid "g2")
                  :m1 (create-fuel-grid "m1")
                  :m2 (create-fuel-grid "m2")
                  :r1 (create-fuel-grid "r1")
                  :r2 (create-fuel-grid "r2")
                  })

(def final-scar-grid-master {:a1 arrowhead1-final-scar
                             :a2 arrowhead2-final-scar
                             :k1 kootenay1-final-scar
                             :k2 kootenay2-final-scar
                             :g1 glacier1-final-scar
                             :g2 glacier2-final-scar
                             :m1 mica1-final-scar
                             :m2 mica2-final-scar
                             :r1 revelstoke1-final-scar
                             :r2 revelstoke2-final-scar
                             })

; make our initial worlds for each of the 10 fires
(def fire-names ["a1", "a2", "k1", "k2", "g1", "g2", "m1", "m2", "r1", "r2"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grid initialization functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn num-rows
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count ((keyword (name fire-name)) final-scar-grid-master)))
#_(num-rows "m1")
#_(num-rows mica1-forest)
#_(num-rows mica1-elevation)

(defn num-columns
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count (nth ((keyword (name fire-name)) final-scar-grid-master) 0)))
#_(num-columns "m1")
#_(num-columns mica1-forest)
#_(num-columns mica1-elevation)


(defn create-fuel-grid
  "Takes in appropriate fire name and uses its forest grid to return new grid
  where 0 refers to non-fuel and 1 refers to burnable / fuel"
  [fire-name]
  (let [grid ((keyword (name fire-name)) forest-master)]
    (partition (num-columns fire-name)
               (map #(if (and (>= % 100) (<= % 105))
                       (* 0 %)
                       (+ 1 (* 0 %))
                       )
                    (flatten grid)))))
#_(create-fuel-grid "m1")
#_(:m1 forest-master)
#_(:m1 final-scar-grid-master)


(defn construct-empty-grid
  "Returns a vector of vectors filled with 0s with the same dimensions as the specified fire"
  [fire-name]
  (vec (repeat (count ((keyword (name fire-name)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire-name)) elevation-master))) 0)))))

;; note that we're not certain if ignition cell is 0 indexed or not
;; (do we need to subtract 1?)
(defn construct-initial-grid
  "Returns initial grid with a 1 where the ignition cell is"
  [fire-name]
  (let [width (num-columns fire-name)
        ignition-cell ((keyword (name fire-name)) ignition-cell-master)
        grid (construct-empty-grid fire-name)]
    (partition width (assoc (vec (flatten grid)) ignition-cell 1))))
#_(construct-initial-grid "m1")
#_(:m1 ignition-cell-master)
#_(reduce + (flatten (construct-initial-grid "m1")))
#_(reduce + (flatten (construct-initial-grid "m2")))


;; make our initial worlds for each of the 10 fires
(def initial-fire-grids {:a1 (construct-initial-grid "a1")
                         :a2 (construct-initial-grid "a2")
                         :k1 (construct-initial-grid "k1")
                         :k2 (construct-initial-grid "k2")
                         :g1 (construct-initial-grid "g1")
                         :g2 (construct-initial-grid "g2")
                         :m1 (construct-initial-grid "m1")
                         :m2 (construct-initial-grid "m2")
                         :r1 (construct-initial-grid "r1")
                         :r2 (construct-initial-grid "r2")
                         })
#_(:m1 initial-fire-grids)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Burning neighbors   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-get-cell
  "Returns the value of a cell if inbounds, returns 0 if out of bounds
  i is row/height j is column/width"
  [fire-grid i j]
  (let [height (count fire-grid)
        width (count (nth fire-grid 1))]
    (if (and (>= j 0) (< j width) (>= i 0) (< i height))
      (nth (nth fire-grid i) j)                             ;; in bounds so return the value
      0                                                     ;; out of bounds so return zero
      )))
;; should return 1 (bottom right value in test-grid)
#_(safe-get-cell test-grid 2 2)
#_(safe-get-cell test-burning-grid 57 53)


(defn get-burning-neighbors-map
  "Returns a map of neighbors and their associated values. Value in map 0 if out of bounds."
  [cell-id fire-grid]
  (let [height (count fire-grid)
        width (count (nth fire-grid 1))
        x (mod cell-id width)
        y (Math/floor (/ cell-id width))
        return-map (for
                     [input [["NW" -1 -1] ["N" -1 0] ["NE" -1 1] ["E" 0 1] ["W" 0 -1] ["SW" 1 -1] ["S" 1 0] ["SE" 1 1]]]
                     (let [direction (nth input 0)
                           j (nth input 1)
                           i (nth input 2)]
                       (assoc {} (keyword direction) (safe-get-cell fire-grid (+ y j) (+ x i)))))]
    (apply conj return-map)))
;; tests the very middle cell of test-grid
#_(get-burning-neighbors-map 4 test-grid)
;; this broke ass code
#_(get-burning-neighbors-map 9 test-grid)

(defn num-burning-neighbors
  "Gets total num of burning neighbors (only cells with value of 1 are burning, ignores 2s)"
  [cell-id fire-grid]
  (let [num (get (frequencies (vals (conj (get-burning-neighbors-map cell-id fire-grid)))) 1)]
    (if (nil? num) 0 num)))
;; should return 2 for middle cell of test-grid
#_(num-burning-neighbors 4 test-grid)
(def test-grid '((0 0 0) (0 0 1) (2 0 1)))





(defn get-burning-neighbor
  "Calls burning neighbors map and retrieves specific neighbor"
  [cell-id fire-grid neighbor-direction]
  ((keyword neighbor-direction) (get-burning-neighbors-map cell-id fire-grid)))
;; returns southwest neighbor of middle cell, should return 2
#_(get-burning-neighbor 4 test-grid "SW")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data lookups        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-current-weather-var
  "Returns the value of a specified weather variable for a specified fire at a specified time."
  [desired-var fire time]
  (read-string ((keyword (name desired-var)) (nth ((keyword (name fire)) weather-master) (Math/floor (/ time 60))))))
#_(get-current-weather-var "FFMC" "a1" 54)

(defn get-elevation-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (nth (flatten ((keyword (name fire)) elevation-master)) cell-id))
#_(get-elevation-at-cell 3131 "m1")

(defn get-slope-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (nth (flatten ((keyword (name fire)) slope-master)) cell-id))
#_(get-slope-at-cell 150 "r1")


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE FIRE FUNCTIONS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------
; RUNNER

#_(propel-gp {:instructions            fire-instructions
              :error-function          fire-error-function
              :max-generations         500
              :population-size         1
              :max-initial-plushy-size 50
              :step-limit              100
              :parent-selection        :lexicase
              :tournament-size         5})

;-------------------------



#_(fire-error-function test-argmap test-instructions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fire error function (calls run fire)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how do argmap and individual get passed in???
(defn fire-error-function
  "Error is 0 if the value and the program's selected behavior match,
   or 1 if they differ, or 1000000 if no behavior is produced.
   The behavior is here defined as the final top item on the :boolean stack."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))

        ;; a vector containing each fire name as a string is our inputs
        inputs fire-names


        ;; correct output is each
        correct-outputs (map #((keyword (name %)) final-scar-grid-master) inputs)

        ;; run each fire through our run-fire function with the given program
        outputs (map #(run-fire % program argmap) inputs)


        ;; returns a vector where 1 indicates different outputs
        ;; 0 indicates the outputs were the same
        errors (compare-grids outputs correct-outputs)]
    ;errors (map (fn [correct-output output]
    ;              (if (= output :no-stack-item)
    ;                1000000
    ;                (if (= correct-output output)
    ;                  0
    ;                  1))
    ;            correct-outputs
    ;            outputs)]
    (assoc individual
      :behaviors outputs
      :errors errors
      :total-error (apply +' errors))))
#_(fire-error-function test-argmap test-program)



(defn compare-grids
  "Compares each cell in the two grids and
   returns a vector of differences"
  [evolved-scars final-scars]
  (vec (map #(Math/abs (reduce - %))
            (partition 2 (interleave (flatten evolved-scars) (flatten final-scars))))))
;; this should have an error of 1 as only one value is different
#_(compare-grids [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run fire function (calls update grid) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note why ware we updating one extra time step beyond?
(defn run-fire
  "Runs a specified fire all the way through and returns
  a final fire scar to be compared with actual fire scar"
  [fire-name program argmap]
  (loop [grid ((keyword (name fire-name)) initial-fire-grids)
         time-step 0]
    (prn "time-step:" time-step) (prn "Fire name:" fire-name)
    ;(prn grid)
    (if (> time-step 5)
      ;; if time is up convert all 2s to 1s and return fire-scar
      (convert-grid grid)

      ;; otherwise update our grid and increment time

      (recur (time (update-grid grid fire-name time-step program argmap))
             (inc time-step)))))
;; this is dummy slow
#_(run-fire "m1" test-program test-argmap)


(defn convert-grid
  "Converts grid to all 1s and 0s (2s become 1s everything else stays the same"
  [grid]
  ;; partition by columns
  (vec (partition (count (nth grid 0))
                  (map #(if (= % 2)
                          ;; 2s become 1s
                          (dec %)
                          ;; everything else stays the same
                          %)
                       (flatten grid)))))
#_(convert-grid [[2 1 1 0 1 0] [0 0 1 2 2 2]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update grid function (calls update cell) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add functionality so we only update cells that are burning
;; or having at least one burning neighbor and that they are not
;; non-fuel cells (use fuel-master to check this)
;; in general:
;; 0 means unburned, 1 means burning, 2 means burned

;; before testing needs num-burning-neighbors
;; as well as update-cell

(defn update-grid
  "Updates a fire grid from one time step to the next"
  [fire-grid fire-name time-step program argmap]
  (let [flattened-grid (flatten fire-grid)
        flattened-fuel (flatten ((keyword (name fire-name)) fuel-master))]
    ;; turns flattened vector back into a grid
    (partition (num-columns fire-name)
               ;; returns a flattened vector of all cell values
               (for [i (range (count flattened-grid))]
                 ;; only update cell if it's burning
                 (if (or (= (nth flattened-grid i) 1)
                         ;; or it has at least one burning neighbor (burning = 1)
                         ;; and is a burnable cell (according to fuel-master where 1 = burnable)
                         (and (>= (num-burning-neighbors i fire-grid) 1) (= 1 (nth flattened-fuel i))))
                   ;; if true then return 0

                   ;(do
                   ;Use the following to time the update cell method
                   (time (update-cell i fire-name time-step fire-grid program argmap))
                   ;(update-cell i fire-name time-step fire-grid program argmap))
                   ;; else just return current value
                   (nth flattened-grid i))))))
#_(update-grid (:m1 initial-fire-grids) "m1" 0 test-program test-argmap)
#_(update-grid test-burning-grid "m1" 0 test-program test-argmap)




;;;;;;;;;;;;;
;; testing ;;
;;;;;;;;;;;;;
;;  test plushy



#_(def test-program (list 'w))
#_(def test-argmap {:step-limit 5})
#_(def test-program (push-from-plushy (:plushy test-instructions)))

#_(run-fire "m1" test-program test-argmap)


(defn test-update-grid
  "Updates a fire grid from one time step to the next"
  [fire-grid fire-name time program argmap]
  (let [flattened-grid (flatten fire-grid)
        flattened-fuel (flatten ((keyword (name fire-name)) fuel-master))]
    ;; turns flattened vector back into a grid
    (partition (num-columns fire-name)
               ;; returns a flattened vector of all cell values
               (for [i (range (count flattened-grid))]
                 ;; only update cell if it's burning
                 (if (or (= (nth flattened-grid i) 1)
                         ;; or it has at least one burning neighbor (burning = 1)
                         ;; and is a burnable cell (according to fuel-master where 1 = burnable)
                         (and (>= (num-burning-neighbors i fire-grid) 1) (= 1 (nth flattened-fuel i))))
                   ;; if true then return 0
                   0
                   ;; else just return current value
                   (nth flattened-grid i))))))
;; should return all 2s
#_(test-update-grid test-burned-grid "m1" 0 test-program test-argmap)

(defn construct-burned-grid
  "Returns a vector of vectors filled with 2s with the same dimensions as the specified fire"
  [fire-name]
  (vec (repeat (count ((keyword (name fire-name)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire-name)) elevation-master))) 2)))))

(defn construct-burning-grid
  "Returns a vector of vectors filled with 2s with the same dimensions as the specified fire"
  [fire-name]
  (vec (repeat (count ((keyword (name fire-name)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire-name)) elevation-master))) 1)))))

(def test-burned-grid (construct-burned-grid "m1"))
(def test-burning-grid (construct-burning-grid "m1"))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update cell function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-cell
  "Update cell to next state by interpreting push program"
  [cell-id fire-name time current-grid program argmap]
  (prn "cell-id:" cell-id)
  ;(prn "Update-Cell: program" program)
  (let [b-neighbors-map (get-burning-neighbors-map cell-id current-grid)
        current-value (nth (flatten current-grid) cell-id)
        answer (peek-stack
                 (interpret-program
                   program
                   (assoc empty-push-state :input {:elevation             (get-elevation-at-cell cell-id fire-name)
                                                   :slope                 (get-slope-at-cell cell-id fire-name)
                                                   :FWI                   (get-current-weather-var "FWI" fire-name time)
                                                   :WS                    (get-current-weather-var "WS" fire-name time)
                                                   :FFMC                  (get-current-weather-var "FFMC" fire-name time)
                                                   :TMP                   (get-current-weather-var "TMP" fire-name time)
                                                   :APCP                  (get-current-weather-var "APCP" fire-name time)
                                                   :DC                    (get-current-weather-var "DC" fire-name time)
                                                   :BUI                   (get-current-weather-var "BUI" fire-name time)
                                                   :RH                    (get-current-weather-var "RH" fire-name time)
                                                   :ISI                   (get-current-weather-var "ISI" fire-name time)
                                                   :DMC                   (get-current-weather-var "DMC" fire-name time)
                                                   :WD                    (get-current-weather-var "WD" fire-name time)
                                                   :current-value         current-value
                                                   :time-step             time
                                                   :nw                    (:NW b-neighbors-map)
                                                   :n                     (:N b-neighbors-map)
                                                   :ne                    (:NE b-neighbors-map)
                                                   :e                     (:E b-neighbors-map)
                                                   :w                     (:W b-neighbors-map)
                                                   :sw                    (:SW b-neighbors-map)
                                                   :s                     (:S b-neighbors-map)
                                                   :se                    (:SE b-neighbors-map)
                                                   :num-burning-neighbors (num-burning-neighbors cell-id current-grid)
                                                   })

                   (:step-limit argmap))

                 :integer)]

    ;(prn "answer" answer)
    ;(prn "No stack item?" (= answer :no-stack-item))

    ;; get first thing off integer stack
    ;; check for other types, we only went integers
    (cond
      ;; this is simply the cells original value that was initially passed in
      ;; (see :current-value above)
      (= answer :no-stack-item)
      current-value

      ;; If currently 0, can go to 0, 1, 2
      ;; note we might want to only allow 0 to go to 1 (and not straight to 2)
      (= current-value 0)
      (mod answer 3)

      ;; if burning and we get an answer of 0, just stay burning (1)
      (and (= current-value 1) (= (mod answer 3) 0))
      1

      ;; otherwise you can be a 1 or a 2
      (= current-value 1)
      (mod answer 3)

      ;If 2, stays 2
      :else
      2
      )))
#_(update-cell 5 "m1" 0 test-burning-grid test-program test-argmap)

;;;;;;;;;;;;;;;;;;
;; MAIN METHOD  ;;
;;;;;;;;;;;;;;;;;;

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'ring-of-fire.core)]
    (propel-gp (update-in (merge {:instructions            fire-instructions
                                  :error-function          fire-error-function
                                  :max-generations         500
                                  :population-size         200
                                  :max-initial-plushy-size 50
                                  :step-limit              100
                                  :parent-selection        :lexicase
                                  :tournament-size         5}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))

