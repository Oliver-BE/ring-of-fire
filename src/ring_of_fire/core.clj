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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grid initialization functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn num-rows
  "Returns the number of rows of a vector of vectors"
  [grid]
  (count grid))
#_(num-rows (:m1 empty-cell-grids))
#_(num-rows mica1-forest)

(defn num-columns
  "Returns the number of rows of a vector of vectors"
  [grid]
  (count (nth grid 0)))
#_(num-columns (:m1 empty-cell-grids))
#_(num-columns mica1-forest)

;; function to place correct initial burning point into a grid

;; function to turn forest file into a grid of 0s and 1s
;; where 0 = non-fuel and 1 = fuel/burnable
;; (need to use fbp look-up table to determine what is non-fuel)

;;(defn get-elevation-table [fire-string] ())
;;(defn get-slope-table [fire-string] ())

(defn construct-empty-grid
  "Returns a vector of vectors filled with 0s with the same dimensions as the specified fire"
  [fire-name]
  (vec (repeat (count ((keyword (name fire-name)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire-name)) elevation-master))) 0)))))
#_(construct-empty-grid "a1")

;; make our initial worlds for each of the 10 fires
(def empty-cell-grids {:a1 (construct-empty-grid "a1")
                       :a2 (construct-empty-grid "a2")
                       :k1 (construct-empty-grid "k1")
                       :k2 (construct-empty-grid "k2")
                       :g1 (construct-empty-grid "g1")
                       :g2 (construct-empty-grid "g2")
                       :m1 (construct-empty-grid "m1")
                       :m2 (construct-empty-grid "m2")
                       :r1 (construct-empty-grid "r1")
                       :r2 (construct-empty-grid "r2")
                       })
#_(:m1 empty-cell-grids)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-neighbors
  "Returns a sequence of sequences containing each neighbor's pertinent information"
  [cell-id fire current-grid]
  (vec (burning-neighbors cell-id current-grid))
  )

(defn burning-neighbors
  "Returns the burning neighbors of a given cell"
  [cell-id current-grid]
  (let [flat-grid (flatten current-grid)]
    (if (= 1 (nth flat-grid (- cell-id 1))) (- cell-id 1))
    (if (= 1 (nth flat-grid (+ cell-id 1))) (+ cell-id 1))
    ))

;;(defn get-cell [cell-id])

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

;;function to get-is-fuel or not (used when updating grid)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE FIRE FUNCTIONS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fire error function (calls run fire)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fire-error-function
  "Error is 0 if the value and the program's selected behavior match, or 1 if they differ, or 1000000 if no behavior is produced. The behavior is here defined as the final top item on the :boolean stack."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs conway-inputs
        correct-outputs (map get-correct-fire-scar inputs)  ;;g
        outputs (map (run-fire %) inputs)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run fire function (calls update grid) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-fire [fire-string]
  (let [elevation-table (get-elevation-table fire-string)
        more-variables variables])
  (loop
    (if (;some condition)
         ; break the loop so we know we are )
         ('update-grid time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update grid function (calls update cell) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-grid [all our variables]
  "Updates a fire grid from one time step to the next"
  (fn [input program]
    (peek-stack
      (interpret-program
        program
        (assoc empty-push-state :input {:in1 input})
        (:step-limit argmap))
      :integer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update cell function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-cell
  "Update cell to next state by interpreting push program"
  [cell-id fire time current-state program]
  (let [answer (peek-stack
                 (interpret-program
                   program
                   (assoc empty-push-state :input {:elevation     (get-elevation-at-cell cell-id fire)
                                                   :slope         (get-slope-at-cell cell-id fire)
                                                   :FWI           (get-current-weather-var "FWI" fire time)
                                                   :WS            (get-current-weather-var "WS" fire time)
                                                   :FFMC          (get-current-weather-var "FFMC" fire time)
                                                   :TMP           (get-current-weather-var "TMP" fire time)
                                                   :APCP          (get-current-weather-var "APCP" fire time)
                                                   :DC            (get-current-weather-var "DC" fire time)
                                                   :BUI           (get-current-weather-var "BUI" fire time)
                                                   :RH            (get-current-weather-var "RH" fire time)
                                                   :ISI           (get-current-weather-var "ISI" fire time)
                                                   :DMC           (get-current-weather-var "DMC" fire time)
                                                   :WD            (get-current-weather-var "WD" fire time)
                                                   :current-state current-state
                                                   :time          time
                                                   :nw            0
                                                   :n             0
                                                   :ne            0
                                                   :e             0
                                                   :w             0
                                                   :sw            0
                                                   :s             0
                                                   :se            0
                                                   })
                   (:step-limit argmap))
                 :integer)]
    (if (= answer :no-stack-item)
      current-state
      (mod answer 3))))

;;;;;;;;;;;;;;;;;;
;; MAIN METHOD  ;;
;;;;;;;;;;;;;;;;;;

(defn -main
  "Runs propel-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'propel.core)]
    (propel-gp (update-in (merge {:instructions default-instructions
                                  :error-function fire-error-function
                                  :max-generations 500
                                  :population-size 200
                                  :max-initial-plushy-size 50
                                  :step-limit 100
                                  :parent-selection :lexicase
                                  :tournament-size 5}
                                 (apply hash-map
                                        (map read-string args)))
                          [:error-function]
                          #(if (fn? %) % (eval %))))))

