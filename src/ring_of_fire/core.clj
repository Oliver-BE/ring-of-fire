(ns ring-of-fire.core
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]
            [ring-of-fire.propel-helper :refer :all]
            [clojure.walk :refer [postwalk]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRE PROPEL FUNCTIONS     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grid initialization functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; and Master Data Dictionaries ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
#_(:a1 final-scar-grid-master)

(def final-scar-grid-master-flattened {:a1 (vec (flatten arrowhead1-final-scar))
                                       :a2 (vec (flatten arrowhead2-final-scar))
                                       :k1 (vec (flatten kootenay1-final-scar))
                                       :k2 (vec (flatten kootenay2-final-scar))
                                       :g1 (vec (flatten glacier1-final-scar))
                                       :g2 (vec (flatten glacier2-final-scar))
                                       :m1 (vec (flatten mica1-final-scar))
                                       :m2 (vec (flatten mica2-final-scar))
                                       :r1 (vec (flatten revelstoke1-final-scar))
                                       :r2 (vec (flatten revelstoke2-final-scar))
                                       })
#_(:a1 final-scar-grid-master-flattened)

(defn num-rows
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count ((keyword (name fire-name)) final-scar-grid-master)))
#_(num-rows "r1")
#_(num-rows mica1-forest)
#_(num-rows mica1-elevation)


(defn num-columns
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count (nth ((keyword (name fire-name)) final-scar-grid-master) 0)))
#_(num-columns "r1")
#_(num-columns mica1-forest)
#_(num-columns mica1-elevation)

(defn tot-num-cells
  "Returns the total number of cells in a vector of fires"
  [fire-vec]
  (reduce + (map #(* (num-rows %) (num-columns %)) fire-vec)))
#_(tot-num-cells ["a2" "m1" "m2"])

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


(defn create-fuel-grid
  "Takes in appropriate fire name and uses its forest grid to return new grid
  where 0 refers to non-fuel and 1 refers to burnable / fuel"
  [fire-name]
  (let [grid ((keyword (name fire-name)) forest-master)
        ;; messy-grid is what we want, but with each row as a lazy seq
        messy-grid (vec (partition (num-columns fire-name)
                                   (map #(if (and (>= % 100) (<= % 105))
                                           (* 0 %)
                                           (+ 1 (* 0 %))
                                           )
                                        (vec (flatten grid)))))]
    ;; this for loop transforms each lazy seq row into a vector
    (vec (for [i (range (count messy-grid))]
           (vec (nth messy-grid i))))))
#_(create-fuel-grid "m1")
#_(:m1 forest-master)
#_(:m1 final-scar-grid-master)


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

;; elevation glacier1 wont compile
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

;; elevation glacier1 wont compile
(def elevation-master-flattened {:a1 (vec (flatten arrowhead1-elevation))
                                 :a2 (vec (flatten arrowhead2-elevation))
                                 :k1 (vec (flatten kootenay1-elevation))
                                 :k2 (vec (flatten kootenay2-elevation))
                                 :g1 (vec (flatten glacier1-elevation))
                                 :g2 (vec (flatten glacier2-elevation))
                                 :m1 (vec (flatten mica1-elevation))
                                 :m2 (vec (flatten mica2-elevation))
                                 :r1 (vec (flatten revelstoke1-elevation))
                                 :r2 (vec (flatten revelstoke2-elevation))
                                 })

;; note that arrowhead 2 slope is messed up
(def slope-master {:a1 arrowhead1-slope
                   :a2 arrowhead2-slope
                   :k1 kootenay1-slope
                   :k2 kootenay2-slope
                   :g1 glacier1-slope
                   :g2 glacier2-slope
                   :m1 mica1-slope
                   :m2 mica2-slope
                   :r1 revelstoke1-slope
                   :r2 revelstoke2-slope
                   })

;; note that arrowhead 2 slope is messed up
(def slope-master-flattened {:a1 (vec (flatten arrowhead1-slope))
                             :a2 (vec (flatten arrowhead2-slope))
                             :k1 (vec (flatten kootenay1-slope))
                             :k2 (vec (flatten kootenay2-slope))
                             :g1 (vec (flatten glacier1-slope))
                             :g2 (vec (flatten glacier2-slope))
                             :m1 (vec (flatten mica1-slope))
                             :m2 (vec (flatten mica2-slope))
                             :r1 (vec (flatten revelstoke1-slope))
                             :r2 (vec (flatten revelstoke2-slope))
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

(defn construct-empty-grid
  "Returns a vector of vectors filled with 0s with the same dimensions as the specified fire"
  [fire-name]
  (vec (repeat (count ((keyword (name fire-name)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire-name)) elevation-master))) 0)))))
#_(construct-empty-grid "m1")


;; ignition cell is 1 indexed, so we need to subtract 1 from its value
(defn construct-initial-grid
  "Returns initial grid (vector of vectors) with a 1 where the ignition cell is"
  [fire-name]
  (let [width (num-columns fire-name)
        ignition-cell (- ((keyword (name fire-name)) ignition-cell-master) 1)
        grid (construct-empty-grid fire-name)
        ;; messy-grid is what we want, but with each row as a lazy seq
        messy-grid (partition width (assoc (vec (flatten grid)) ignition-cell 1))]
    ;; this for loop transforms each lazy seq row into a vector
    (vec (for [i (range (count messy-grid))]
           (vec (nth messy-grid i))))))
#_(construct-initial-grid "m1")
#_(:m1 ignition-cell-master)
#_(reduce + (flatten (construct-initial-grid "m1")))
#_(reduce + (flatten (construct-initial-grid "m2")))

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

;; a flattened grid where 0 refers to non-fuel, 1 refers to burnable / fuel
(def fuel-master-flattened {:a1 (vec (flatten (create-fuel-grid "a1")))
                            :a2 (vec (flatten (create-fuel-grid "a2")))
                            :k1 (vec (flatten (create-fuel-grid "k1")))
                            :k2 (vec (flatten (create-fuel-grid "k2")))
                            :g1 (vec (flatten (create-fuel-grid "g1")))
                            :g2 (vec (flatten (create-fuel-grid "g2")))
                            :m1 (vec (flatten (create-fuel-grid "m1")))
                            :m2 (vec (flatten (create-fuel-grid "m2")))
                            :r1 (vec (flatten (create-fuel-grid "r1")))
                            :r2 (vec (flatten (create-fuel-grid "r2")))
                            })

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

;; make our initial worlds for each of the 10 fires
(def initial-fire-grids-flattened {:a1 (vec (flatten (construct-initial-grid "a1")))
                                   :a2 (vec (flatten (construct-initial-grid "a2")))
                                   :k1 (vec (flatten (construct-initial-grid "k1")))
                                   :k2 (vec (flatten (construct-initial-grid "k2")))
                                   :g1 (vec (flatten (construct-initial-grid "g1")))
                                   :g2 (vec (flatten (construct-initial-grid "g2")))
                                   :m1 (vec (flatten (construct-initial-grid "m1")))
                                   :m2 (vec (flatten (construct-initial-grid "m2")))
                                   :r1 (vec (flatten (construct-initial-grid "r1")))
                                   :r2 (vec (flatten (construct-initial-grid "r2")))
                                   })

(def initial-time-grids-flattened {:a1 (vec (flatten (construct-empty-grid "a1")))
                                   :a2 (vec (flatten (construct-empty-grid "a2")))
                                   :k1 (vec (flatten (construct-empty-grid "k1")))
                                   :k2 (vec (flatten (construct-empty-grid "k2")))
                                   :g1 (vec (flatten (construct-empty-grid "g1")))
                                   :g2 (vec (flatten (construct-empty-grid "g2")))
                                   :m1 (vec (flatten (construct-empty-grid "m1")))
                                   :m2 (vec (flatten (construct-empty-grid "m2")))
                                   :r1 (vec (flatten (construct-empty-grid "r1")))
                                   :r2 (vec (flatten (construct-empty-grid "r2")))
                                   })

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Burning neighbors   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn safe-get-cell
  "Returns the value of a cell if inbounds, returns 0 if out of bounds
  i is row/height j is column/width"
  [flattened-grid num-cell]
  (if (and (>= num-cell 0) (< num-cell (count flattened-grid)))
    (nth flattened-grid num-cell)
    0
    ))


;; should return 1 (bottom right value in test-grid)
#_(safe-get-cell test-grid 2 2)
#_(safe-get-cell test-burning-grid 57 53)

#_(def burning-neighbor 0)
#_(def direction 45)
#_(if (not= 0 burning-neighbor) (assoc {} direction burning-neighbor))

#_(def cell-id 4)
#_(def width 3)
#_(def test-grid [4 1 2 0 0 5 6 7 8])
#_(into {} (for
             [input [[0 (+ cell-id 1)]
                     [45 (- cell-id (- width 1))]
                     [90 (- cell-id width)]
                     [135 (- cell-id (+ width 1))]
                     [180 (- cell-id 1)]
                     [225 (+ cell-id (- width 1))]
                     [270 (+ cell-id width)]
                     [315 (+ cell-id (+ width 1))]]]
             (let [direction (nth input 0)
                   num-cell-to-get (nth input 1)
                   burning-neighbor (safe-get-cell test-grid num-cell-to-get)]
               (if (not= 0 burning-neighbor) (assoc {} direction burning-neighbor)))))

;;Direction string keys are now angles (east is 0, west is 180, etc)
(defn get-burning-neighbors-map-using-time
  "Returns a map of time of neighbors and their associated values. Value in map 0 if out of bounds."
  [cell-id flattened-time-grid fire-name]
  (let [width (count (nth ((keyword fire-name) initial-fire-grids) 1))
        return-map (into {} (for
                              [input [[0 (+ cell-id 1)]
                                      [45 (- cell-id (- width 1))]
                                      [90 (- cell-id width)]
                                      [135 (- cell-id (+ width 1))]
                                      [180 (- cell-id 1)]
                                      [225 (+ cell-id (- width 1))]
                                      [270 (+ cell-id width)]
                                      [315 (+ cell-id (+ width 1))]]]
                              (let [direction (nth input 0)
                                    num-cell-to-get (nth input 1)
                                    burning-neighbor (safe-get-cell flattened-time-grid num-cell-to-get)]
                                (if (not= 0 burning-neighbor)
                                  (assoc {} direction burning-neighbor)))))]
    return-map))

(defn get-burning-neighbors-map
  "Returns a map of neighbors and their associated values. Value in map 0 if out of bounds."
  [cell-id flattened-grid fire-name]
  (let [width (count (nth ((keyword fire-name) initial-fire-grids) 1))
        return-map (for
                     [input [["NW" (- cell-id (+ width 1))] ["N" (- cell-id width)] ["NE" (- cell-id (- width 1))] ["W" (- cell-id 1)] ["E" (+ cell-id 1)] ["SW" (+ cell-id (- width 1))] ["S" (+ cell-id width)] ["SE" (+ cell-id (+ width 1))]]]
                     (let [direction (nth input 0)
                           num-cell-to-get (nth input 1)]
                       (assoc {} (keyword direction) (safe-get-cell flattened-grid num-cell-to-get))))]
    (apply conj return-map)))
;; tests the very middle cell of test-grid
#_(get-burning-neighbors-map 4 test-grid)
;; this broke ass code
#_(get-burning-neighbors-map 9 test-grid)

(defn num-burning-neighbors
  "Gets total num of burning neighbors (only cells with value of 1 are burning, ignores 2s)"
  [cell-id flattened-grid fire-name]
  (let [num (get (frequencies (vals (conj (get-burning-neighbors-map cell-id flattened-grid fire-name)))) 1)]
    (if (nil? num) 0 num)))
;; should return 2 for middle cell of test-grid
#_(num-burning-neighbors 4 test-grid)
(def test-grid '(0 0 0 0 0 1 2 0 1))



(defn get-burning-neighbor
  "Calls burning neighbors map and retrieves specific neighbor"
  [cell-id fire-grid neighbor-direction fire-name]
  ((keyword neighbor-direction) (get-burning-neighbors-map cell-id fire-grid fire-name)))
;; returns southwest neighbor of middle cell, should return 2
#_(get-burning-neighbor 4 test-grid "SW")



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data lookups        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-current-weather-var
  "Returns the value of a specified weather variable for a specified fire at a specified time."
  [desired-var fire time]
  (int (read-string ((keyword (name desired-var)) (nth ((keyword (name fire)) weather-master) (Math/floor (/ time 60)))))))
#_(get-current-weather-var "FFMC" "a1" 54)
#_(time (nth ((keyword (name "m1")) weather-master) (Math/floor (/ 80 60))))


(defn get-elevation-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (int (nth ((keyword (name fire)) elevation-master-flattened) cell-id)))
#_(get-elevation-at-cell 3131 "m1")

(defn get-slope-at-cell
  "Returns the slope of a specified cell in a specified fire"
  [cell-id fire]
  (int (nth ((keyword (name fire)) slope-master-flattened) cell-id)))
#_(get-slope-at-cell 150 "r1")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test grids with proper dimensions;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(def test-burned-grid-flat (vec (flatten (construct-burned-grid "m1"))))
(def test-burning-grid (construct-burning-grid "m1"))
(def test-burning-grid-flat (vec (flatten (construct-burning-grid "m1"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE FIRE FUNCTIONS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update cell function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-net-direction
  "Returns a net fire direction from currently burning neighbors"
  [b-neighbors-time-map]
  (let [directions-burning (keys b-neighbors-time-map)]
    (if (not (empty? directions-burning))
      (/ (reduce + directions-burning) (count directions-burning))
      0)))
#_(get-net-direction {0 5, 90 5})

#_(def sample-program '(sw n integer_% false false e exec_dup (e integer_* w sw WD DMC false exec_if)))
(defn update-cell
  "Update cell to next state by interpreting push program"
  [cell-id fire-name time flattened-grid program argmap time-burning current-time-grid]
  (let [b-neighbors-time-map (get-burning-neighbors-map-using-time cell-id current-time-grid fire-name)
        current-value (nth flattened-grid cell-id)
        answer (peek-stack
                 (interpret-program
                   (if (= current-value 0)
                     (nth program 0)
                     (nth program 1))
                   (assoc empty-push-state :input {:slope (get-slope-at-cell cell-id fire-name)
                                                   :ISI   (get-current-weather-var "ISI" fire-name time)
                                                   :BUI   (get-current-weather-var "BUI" fire-name time)
                                                   :FWI   (get-current-weather-var "FWI" fire-name time)
                                                   :NT    (int (/ (reduce + (vals b-neighbors-time-map)) 8))
                                                   :NBD   (int (get-net-direction b-neighbors-time-map))
                                                   :WS    (get-current-weather-var "WS" fire-name time)
                                                   :WD    (get-current-weather-var "WD" fire-name time)
                                                   :TB    time-burning

                                                   ;:split     false
                                                   ;:elevation         (get-elevation-at-cell cell-id fire-name)
                                                   ;;:FFMC          (get-current-weather-var "FFMC" fire-name time)
                                                   ;;:TMP           (get-current-weather-var "TMP" fire-name time)
                                                   ;;:APCP          (get-current-weather-var "APCP" fire-name time)
                                                   ;;:DC            (get-current-weather-var "DC" fire-name time)
                                                   ;;:RH            (get-current-weather-var "RH" fire-name time)
                                                   ;;:DMC           (get-current-weather-var "DMC" fire-name time)
                                                   ;;The stack can't take something that isn't an int right?
                                                   })
                   (:step-limit argmap))
                 :boolean)]
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
      (if answer 1 0)
      ;; if burning and we get an answer of 0, just stay burning (1)
      (= current-value 1)
      (if answer 2 1))))

#_(update-cell 5 "m1" 0 test-burning-grid-flat test-burning-grid test-program test-argmap)
#_(time (update-cell 5 "m1" 10 test-burned-grid-flat test-burned-grid test-program test-argmap))
#_(time (update-cell 5 "m1" 10 test-burning-grid-flat test-burning-grid test-program test-argmap))
#_(time (get-burning-neighbors-map 500 test-burning-grid))
#_(time (nth (flatten test-burning-grid) 500))
#_(time (nth (vec (flatten test-burning-grid)) 500))

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
#_(count [10 5 4 3])

#_(do 1 (count [10 5 4]))
#_(time (old-update-grid test-burning-grid "m1" 100 test-program test-argmap))
#_(time (old-update-grid ((keyword (name "m1")) initial-fire-grids) "m1" 0 test-program test-argmap))
#_(time (old-update-grid test-burned-grid "m1" 100 test-program test-argmap))

(defn update-grid
  "Updates a fire grid from one time step to the next, RETURNS A MAP OF :time-grid and :cell-grid"
  [flattened-grid fire-name time-step program argmap flattened-time-grid]
  ;prn "---------------------")
  (let [flattened-fuel ((keyword (name fire-name)) fuel-master-flattened)
        grid-size (count flattened-grid)]
    ;; go through every cell in our grid
    (loop [i 0
           current-time-grid flattened-time-grid
           current-cell-grid flattened-grid]
      ;; if you're at the end of the grind
      (if (= i grid-size)
        {:time-grid current-time-grid :cell-grid current-cell-grid}

        ;; otherwise, you want to check the values for each grid
        (let [cell-value (nth flattened-grid i)
              time-burning (nth current-time-grid i)]

          ;; if it's burning
          (if (= cell-value 1)
            ;; then we increase time-burning and update the cell
            (recur (inc i) (assoc current-time-grid i (+ time-burning 1)) (assoc current-cell-grid i (update-cell i fire-name time-step flattened-grid program argmap time-burning current-time-grid)))

            ;; otherwise check to see if it's burned
            (if (= cell-value 2)
              ;; if it is, then don't change anything and leave the cell as burned
              (recur (inc i) (assoc current-time-grid i 0) current-cell-grid)

              ;; if it's not burned, then it must be a 0 (unburned)
              ;; so now we check if it has at least one burning neighbor and it is burnable and it will be burning next
              (if (and
                    (>= (num-burning-neighbors i flattened-grid fire-name) 1)
                    (= 1 (nth flattened-fuel i))
                    (= 1 (update-cell i fire-name time-step flattened-grid program argmap time-burning current-time-grid)))
                ;; if it is burning then set to burning and increase time burning
                (recur (inc i) (assoc current-time-grid i (+ time-burning 1)) (assoc current-cell-grid i 1))
                ;; otherwise then don't change anything and leave the cell as unburned
                (recur (inc i) current-time-grid current-cell-grid)))))))))
#_(:time-grid (update-grid (:m1 initial-fire-grids-flattened) "m1" 100 test-argmap (:m1 initial-time-grids-flattened)))
#_(update-grid (:m1 initial-fire-grids-flattened) "m1" 100 '(NBD) test-argmap (:m1 initial-time-grids-flattened))
#_(time (update-grid ((keyword (name "m1")) initial-fire-grids) "m1" 0 test-program test-argmap))
#_(time (update-grid test-grid "m1" 100 test-program test-argmap))
#_(:m1 initial-fire-grids-flattened)

(defn convert-vector
  "Converts vector to all 1s and 0s (2s become 1s everything else stays the same)"
  [grid]
  (map #(if (= % 2)
          ;; 2s become 1s
          (dec %)
          ;; everything else stays the same
          %) grid))
#_(convert-vector '(2 1 1 0 1 0 0 0 1 2 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run fire function (calls update grid) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(old-run-fire "m1" test-program test-argmap)
#_(time (old-run-fire "m1" test-program test-argmap))
#_(time (old-run-fire "m1" best-program test-argmap))
#_(def test-argmap {:instructions            fire-instructions
                    :error-function          fire-error-function
                    :max-generations         1000
                    :population-size         5
                    :max-initial-plushy-size 20
                    :step-limit              100
                    :parent-selection        :lexicase
                    :tournament-size         5
                    :time-step               10
                    :fire-selection          1})

(defn run-fire
  "Runs a specified fire all the way through and returns
  a final fire scar to be compared with actual fire scar.
  Returns a vector of burning and non-burning cells (can be partitioned
  back into a grid later)"
  [fire-name program argmap]
  (loop [flat-grid ((keyword (name fire-name)) initial-fire-grids-flattened)
         time-step 0
         time-grid ((keyword (name fire-name)) initial-time-grids-flattened)]
    (if (>= time-step 1440)
      ;; if time is up convert all 2s to 1s and return fire-scar

      (convert-vector flat-grid)                            ;might need to change the convert grid format

      ;; otherwise update our grid and increment time
      (let [updated-grids (update-grid flat-grid fire-name time-step program argmap time-grid)]
        (recur (:cell-grid updated-grids)
               (+ time-step (:time-step argmap))
               (:time-grid updated-grids))))))
#_(run-fire "m1" test-program test-argmap)
#_(time (run-fire "m1" test-program test-argmap))
#_(time (run-fire "m1" best-program test-argmap))
#_(def test-a-program '(WS se BUI DMC 1 integer_+ integer_- exec_dup (APCP w DC integer_- exec_if (num-burning-neigh TMP boolean_= integer_-))))
#_(def runfire-test-argmap {:instructions            fire-instructions
                            :error-function          fire-error-function
                            :max-generations         1000
                            :population-size         5
                            :max-initial-plushy-size 30
                            :step-limit              100
                            :parent-selection        :lexicase
                            :tournament-size         5
                            :time-step               10
                            :fire-selection          1})
(def best-program '(sw n integer_% false false e exec_dup (e integer_* w sw WD DMC false exec_if)))
(def test-program (list 'w))
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

;----------------------------------------------
;run-fire inputs: [fire-name program argmap]
#_(def m1_run_24time (run-fire "m1" test-program test-argmap))

; update-grid inputs: [fire-grid fire-name time-step program argmap]
#_(time (update-grid m1-tester-flat-grid "m1" 100 test-program test-argmap))


#_(partition (run-fire "m1" test-program test-argmap) (num-columns "m1"))
#_(def m1-tester-flat-grid (vec (flatten (:m1 initial-fire-grids))))


(defn convert-grid
  "Converts grid to all 1s and 0s (2s become 1s everything else stays the same)"
  [grid]
  ;; partition by columns
  (let [messy-grid (vec (partition (count (nth grid 0))
                                   (map #(if (= % 2)
                                           ;; 2s become 1s
                                           (dec %)
                                           ;; everything else stays the same
                                           %)
                                        (flatten grid))))]
    ;; this for loop transforms each lazy seq row into a vector
    (vec (for [i (range (count messy-grid))]
           (vec (nth messy-grid i))))))
#_(convert-grid '((2 1 1 0 1 0) (0 0 1 2 2 2)))
#_(convert-grid test-burned-grid)
#_(convert-grid (update-grid (:m1 initial-fire-grids) "m1" 0 test-program test-argmap))


;; DOES THIS NEED TO RETURN A VECTOR?
(defn compare-grids
  "Compares each cell in the two grids and
   returns a vector of differences"
  [evolved-scars final-scars]
  (vec (map #(abs (reduce - %))
            (partition 2 (interleave (flatten evolved-scars) (flatten final-scars))))))
;; this should have an error of 1 as only one value is different
#_(compare-grids [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])
#_(compare-grids [[1 0 0] [0 0 1] [0 1 1]] [[1 0 0] [0 1 1] [1 0 0]])
#_(def test-time-program (list 'time-step))
#_(reduce + (compare-grids (old-run-fire "m1" test-time-program test-argmap) (run-fire "m1" test-time-program test-argmap)))
#_(reduce + (compare-grids (old-run-fire "m1" test-program test-argmap) (run-fire "m1" test-program test-argmap)))

(defn remove-split
  "removes every instance of :split inside a nested structure"
  [program]
  (postwalk
    (fn [v]
      (if (coll? v)
        (reverse (into (empty v) (remove #{:split}) v))
        v))
    program))

(defn make-split-program
  "Returns a program partitioned at the first instance of :split with all splits removed"
  [program]
  (let [split (split-with (complement #{:split}) program)
        force-split (if (empty? (nth split 1))
                      (split-at (/ (count program) 2) program)
                      split)
        before-split (nth force-split 0)
        after-split (remove #{:split} (nth force-split 1))]
    [(remove-split before-split) (remove-split after-split)])
  )

#_(nth (make-split-program sample-program) 0)
#_(make-split-program sample-program)
#_(make-split-program no-split-program)
#_(def sample-program '(sw n integer_% false false :split exec_dup :split (e integer_* w sw :split WD DMC :split false exec_if)))
#_(def no-split-program '(sw n integer_% false false exec_dup (e integer_* w sw WD DMC false exec_if)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fire error function (calls run fire)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_(sort (take 2 (shuffle fire-names)))
;; how do argmap and individual get passed in???

#_(def test-individual '(FWI NBD integer_- true integer_* WD :split exec_dup exec_dup integer_* boolean_not NT NT 0 0 NT NT 1))
(defn fire-error-function
  "Error is 0 if the value and the program's selected behavior match,
   or 1 if they differ, or 1000000 if no behavior is produced.
   The behavior is here defined as the final top item on the :boolean stack."
  [argmap fire-subset individual]
  (let [program (push-from-plushy (:plushy individual))
        ;; make-split-program splits the program [[program][program]]
        split-program (make-split-program program)
        ;; a vector containing each fire name as a string is our inputs
        inputs fire-subset
        ;; correct output is each
        correct-outputs (map #((keyword (name %)) final-scar-grid-master-flattened) inputs)
        ;; run each fire through our run-fire function with the given program
        outputs (pmap #(run-fire % split-program argmap) inputs)
        ;; returns a vector where 1 indicates different outputs
        ;; 0 indicates the outputs were the same
        errors (compare-grids outputs correct-outputs)]

    (assoc individual
      ;;:behaviors outputs
      ;;:errors errors
      :total-error (reduce + errors))))
#_(time (fire-error-function test-argmap ["m1" "m2" "g1"] sample-program))
#_(def sample-program '(sw n integer_% false false :split exec_dup :split (e integer_* w sw :split WD DMC :split false exec_if)))
#_(def test-argmap {:instructions            fire-instructions
                  :error-function          fire-error-function
                  :max-generations         1000
                  :population-size         5
                  :max-initial-plushy-size 20
                  :step-limit              100
                  :parent-selection        :lexicase
                  :tournament-size         5
                  :time-step               500
                  :fire-selection          1})
(def lookuptest {:a [0 0 0]
                 :b [1 1 1]
                 :c [2 2 2]})
#_(map #((keyword (name %)) lookuptest) ["a" "b" "c"])



;-------------------------
; RUNNER

#_(propel-gp {:instructions            fire-instructions
              :error-function          fire-error-function
              :max-generations         1000
              :population-size         5
              :max-initial-plushy-size 30
              :step-limit              100
              :parent-selection        :lexicase
              :tournament-size         5
              :time-step               50
              :fire-selection          3})

;-------------------------


;;;;;;;;;;;;;;;;;;
;; MAIN METHOD  ;;
;;;;;;;;;;;;;;;;;;
#_(defn -main
    "Runs propel-gp, giving it a map of arguments."
    [& args]
    (binding [*ns* (the-ns 'ring-of-fire.core)]
      (propel-gp {:instructions            fire-instructions
                  :error-function          fire-error-function
                  :max-generations         5000
                  :population-size         5
                  :max-initial-plushy-size 30
                  :parent-selection        :lexicase
                  :tournament-size         5
                  :fire-selection          3
                  :time-step               5
                  :step-limit              100})))
;;fires-tested can be an int x from 1-10 where any value less than 10 will evaluate a random selection of x fires

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'ring-of-fire.core)]
    (propel-gp (update-in (merge {:instructions            fire-instructions
                                  :error-function          fire-error-function
                                  :max-generations         5000
                                  :population-size         10
                                  :max-initial-plushy-size 40
                                  :parent-selection        :lexicase
                                  :tournament-size         5
                                  :fire-selection          2
                                  :time-step               3
                                  :step-limit              100}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to test a chosen program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make sure we are running this on a fire from the test set
(def test-fire-vec ["a1", "g1", "r2"])
#_(run-fire "m1" test-a-program argmap-for-test-program)
(def test-program (list 'w))
(def test-a-program '(WS se BUI DMC 1 integer_+ integer_- exec_dup (APCP w DC integer_- exec_if (num-burning-neigh TMP boolean_= integer_-))))
; make sure this is the exact same as the parameters for when you ran the above program
#_(def argmap-for-test-program {:instructions            fire-instructions
                                :error-function          fire-error-function
                                :max-generations         1000
                                :population-size         5
                                :max-initial-plushy-size 20
                                :step-limit              25
                                :parent-selection        :lexicase
                                :tournament-size         5
                                :time-step               1000
                                :fire-selection          1})

;;; write program to CSV
#_(write-csv [["abc" "def"] ["ghi" "jkl"]] "data/Outputs/output.csv")
#_(write-csv (run-fire "m1" test-a-program argmap-for-test-program) "data/Outputs/output.csv")
