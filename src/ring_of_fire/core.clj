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

;; make our initial worlds for each of the 10 fires
(def fire-names ["a1", "a2", "k1", "k2", "g1", "g2", "m1", "m2", "r1", "r2"])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fire neighbors functions
(def test-neighbors '((0 0 0) (1 1 1) (1 0 1)))

(defn get-burning-neighbors-map
  "Returns a map of neighbors and their associated values. Value in map 0 if out of bounds."
  [cell-id fire-grid]
  (let [height (count fire-grid)
        width (count (nth fire-grid 1))
        x (mod cell-id width)
        y (Math/floor (/ cell-id width))
        return-map
        (for [input [["NW" -1 -1] ["N" -1 0] ["NE" -1 1] ["E" 0 -1] ["W" 0 1] ["SW" 1 -1] ["S" 1 0] ["SE" 1 1]]]
          (let [direction (nth input 0)
                j (nth input 1)
                i (nth input 2)]
            (assoc return-map (keyword direction) (safe-get-cell fire-grid (+ x i) (+ y j)))))]
    (apply conj return-map)))

(defn num-burning-neighbors
  "Gets total num of burning neighbors"
  [cell-id fire-grid]
  (reduce + (vals (conj (get-burning-neighbors-map cell-id fire-grid)))))

(defn get-burning-neighbor
  "Calls burning neighbors map and retrieves specific neighbor"
  [cell-id fire-grid neighbor-direction]
  ((keyword neighbor-direction) (get-burning-neighbors-map cell-id fire-grid)))

(defn safe-get-cell
  "Returns the value of a cell if inbounds, returns 0 if out of bounds"
  [fire-grid i j]
  (let [height (count fire-grid)
        width (count (nth fire-grid 1))]
    (if (and (>= j 0) (< j width) (>= i 0) (< i height))
      (nth (nth fire-grid j) i)                             ;; in bounds so return the value
      0                                                     ;; out of bounds so return zero
      )))

;; need to fix
;(defn get-neighbors
;  "Returns a sequence of sequences containing each neighbor's pertinent information"
;  [cell-id fire current-grid]
;  (vec (burning-neighbors cell-id current-grid))
;  )

;; need to fix
;(defn burning-neighbors
;  "Returns the burning neighbors of a given cell"
;  [cell-id current-grid]
;  (let [flat-grid (flatten current-grid)]
;    (if (= 1 (nth flat-grid (- cell-id 1))) (- cell-id 1))
;    (if (= 1 (nth flat-grid (+ cell-id 1))) (+ cell-id 1))
;    ))


(defn get-current-weather-var
  "Returns the value of a specified weather variable for a specified fire at a specified time."
  [desired-var fire time]
  (read-string ((keyword (name desired-var)) (nth ((keyword (name fire)) weather-master) (Math/floor (/ time 60))))))
#_(get-current-weather-var "FFMC" "a1" 54)

(defn get-elevation-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (read-string (nth (flatten ((keyword (name fire)) elevation-master)) cell-id)))
#_(get-elevation-at-cell 150 "r1")

(defn get-slope-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (read-string (nth (flatten ((keyword (name fire)) slope-master)) cell-id)))
#_(get-slope-at-cell 150 "r1")


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE FIRE FUNCTIONS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        correct-outputs (map #((keyword (name %) final-scar-grid-master)) inputs)
        ;; run each fire through our run-fire function with the given program
        outputs (map #(run-fire % argmap program) inputs)
        errors (map (fn [correct-output output]             ;;implement lexicase selection here I think??? Might already be implemented
                      (if (= output :no-stack-item)
                        1000000
                        (if (= correct-output output)
                          0
                          1)))
                    correct-outputs
                    outputs)]
    (assoc individual
      :behaviors outputs
      :errors errors
      :total-error (apply +' errors))))

(defn testpl [fire-names]
  (#((keyword (name %) final-scar-grid-master)) (take 1 fire-names))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; run fire function (calls update grid) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defn run-fire
    "Runs a specified fire all the way through and returns
    a final fire scar to be compared with actual fire scar"
    [fire-name program argmap]
    (loop [grid ((keyword (name fire-name) initial-fire-grids))
           time 0]
      (if (> time 1440)
        ;; if time is up return fire-scar
        grid
        ;; otherwise update our grid and increment time
        (recur (update-grid grid fire-name time program argmap)
               (inc time)))))

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
    [fire-grid fire-name time program argmap]
    ;; turns flattened vector back into a grid
    (partition (num-columns fire-name)
               ;; returns a flattened vector of all cell values
               (for [i (range (count (flatten fire-grid)))]
                 ;; only update cell if it's burning
                 ;; or it has at least one burning neighbor (burning = 1)
                 ;; and is a burnable cell (according to fuel-master)
                 (if (or (= (nth (flatten fire-grid) i) 1)
                         (and (>= (num-burning-neighbors fire-grid i) 1) (= 0 (nth (flatten ((keyword (name fire-name)) fuel-master)) i))))
                   ;; if true then update cell
                   (update-cell i fire-name time fire-grid program argmap)
                   ;; else just return current value
                   (nth (flatten fire-grid) i)))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; update cell function ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defn update-cell
    "Update cell to next state by interpreting push program"
    [cell-id fire-name time current-grid program argmap]
    (let [answer (peek-stack
                   (interpret-program
                     program
                     (assoc empty-push-state :input {:elevation         (get-elevation-at-cell cell-id fire-name)
                                                     :slope             (get-slope-at-cell cell-id fire-name)
                                                     :FWI               (get-current-weather-var "FWI" fire-name time)
                                                     :WS                (get-current-weather-var "WS" fire-name time)
                                                     :FFMC              (get-current-weather-var "FFMC" fire-name time)
                                                     :TMP               (get-current-weather-var "TMP" fire-name time)
                                                     :APCP              (get-current-weather-var "APCP" fire-name time)
                                                     :DC                (get-current-weather-var "DC" fire-name time)
                                                     :BUI               (get-current-weather-var "BUI" fire-name time)
                                                     :RH                (get-current-weather-var "RH" fire-name time)
                                                     :ISI               (get-current-weather-var "ISI" fire-name time)
                                                     :DMC               (get-current-weather-var "DMC" fire-name time)
                                                     :WD                (get-current-weather-var "WD" fire-name time)
                                                     :current-value     (nth (flatten current-grid) cell-id)
                                                     :time              time
                                                     :nw                0
                                                     :n                 0
                                                     :ne                0
                                                     :e                 0
                                                     :w                 0
                                                     :sw                0
                                                     :s                 0
                                                     :se                0
                                                     :num-neigh-burning (num-burning-neighbors current-grid cell-id)
                                                     })
                     ;; what is this????
                     (:step-limit argmap))
                   :integer)]
      (if (= answer :no-stack-item)
        ;; this is simply the cells original value that was initially passed in
        ;; (see :current-value above)
        (nth (flatten current-grid) cell-id)
        (mod answer 3))))

  ;;;;;;;;;;;;;;;;;;
  ;; MAIN METHOD  ;;
  ;;;;;;;;;;;;;;;;;;

  (defn -main
    "Runs propel-gp, giving it a map of arguments."
    [& args]
    (binding [*ns* (the-ns 'ring-of-fire.core)]
      (propel-gp (update-in (merge {:instructions            default-instructions
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

