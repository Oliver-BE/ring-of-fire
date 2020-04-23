(ns ring-of-fire.core
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]
            [ring-of-fire.propel-helper :refer :all]))



;;;;;; OLD ERROR FUNCTION
(defn error
  "Compares each cell in the two grids and finds the sum of differences between the two"
  [evolved-scar final-scar]
  (reduce + (map #(Math/abs (reduce - %))
                 (partition 2 (interleave (flatten evolved-scar) (flatten final-scar))))))
;; this should have an error of 1 as only one value is different
#_(error [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])
;; this should have an error of 0 since it's the same fire scar twice
#_(error arrowhead1-final-scar arrowhead1-final-scar)




;;;;;;;;;;;;;;;;;;;;;;;
;; Fire methods      ;;
;;;;;;;;;;;;;;;;;;;;;;;


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

;update each fire gird from one time step to the next
(defn update-grid [all our variables]
  "Updates a fire grid from one time step to the next"
  (fn [input program]
    (peek-stack
      (interpret-program
        program
        (assoc empty-push-state :input {:in1 input})
        (:step-limit argmap))
      :integer)))



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

(defn get-cell [cell-id]
  )

(defn current-weather-var
  "Returns the value of a specified weather variable for a specified fire at a specified time."
  [desired-var fire time]
  (read-string ((keyword (name desired-var)) (nth ((keyword (name fire)) weather-master) (Math/floor (/ time 60))))))
#_(current-weather-var "FFMC" "a1" 54)

(defn elevation-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (read-string (nth (flatten ((keyword (name fire)) elevation-master)) cell-id)))
#_(elevation-at-cell 150 "r1")

(defn slope-at-cell
  "Returns the elevation of a specified cell in a specified fire"
  [cell-id fire]
  (read-string (nth (flatten ((keyword (name fire)) slope-master)) cell-id)))
#_(slope-at-cell 150 "r1")

(defn update-cell
  "Update cell to next state by interpreting push program"
  [cell-id fire time current-state program]
  (let [answer (peek-stack
                 (interpret-program
                   program
                   (assoc empty-push-state :input {:elevation     (elevation-at-cell cell-id fire)
                                                   :slope         (slope-at-cell cell-id fire)
                                                   :FWI           (current-weather-var "FWI" fire time)
                                                   :WS            (current-weather-var "WS" fire time)
                                                   :FFMC          (current-weather-var "FFMC" fire time)
                                                   :TMP           (current-weather-var "TMP" fire time)
                                                   :APCP          (current-weather-var "APCP" fire time)
                                                   :DC            (current-weather-var "DC" fire time)
                                                   :BUI           (current-weather-var "BUI" fire time)
                                                   :RH            (current-weather-var "RH" fire time)
                                                   :ISI           (current-weather-var "ISI" fire time)
                                                   :DMC           (current-weather-var "DMC" fire time)
                                                   :WD            (current-weather-var "WD" fire time)
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


(defn run-fire [fire-string]
  (let [elevation-table (get-elevation-table fire-string)
        more-variables variables])
  (loop
    (if (;some condition)
         ; break the loop so we know we are )
         ('update-grid time)))))


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









