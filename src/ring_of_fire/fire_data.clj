(ns ring-of-fire.fire-data
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]
            [clojure.walk :refer [postwalk]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Data and grid initializations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; final fire scars for each fire, as a vector of vectors
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
#_(:m1 final-scar-grid-master)

;; final fire scars for each fire, as a flattened vector
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
#_(:m1 final-scar-grid-master-flattened)

(defn num-rows
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count ((keyword (name fire-name)) final-scar-grid-master)))
#_(num-rows "m1")

(defn num-columns
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count (nth ((keyword (name fire-name)) final-scar-grid-master) 0)))
#_(num-columns "m1")

(defn tot-num-cells
  "Returns the total number of cells in a vector of fires"
  [fire-vec]
  (reduce + (map #(* (num-rows %) (num-columns %)) fire-vec)))
#_(tot-num-cells ["a2" "m1" "m2"])

;; forest composition for each fire, as a vector of vectors
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
#_(:m1 forest-master)

(defn create-fuel-grid
  "Takes in appropriate fire name and uses its forest grid to return new grid
  where 0 refers to non-fuel cell and 1 refers to burnable/fuel cell.
  The IDs 100 through 105 in a forest grid correspond to non-fuel"
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

;; fuel-grids for each fire where 0 refers to non-fuel, 1 refers to burnable/fuel
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
#_(:m1 fuel-master)

;; fuel-grids for each fire, as a flattened vector
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
#_(:m1 fuel-master-flattened)

;; weather data for each fire as a vector of dictionaries
;; each weather data object has 24 dictionaries, one for each hour of the day
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
#_(:m1 weather-master)
#_(:ISI (nth (:m1 weather-master) 23)) ;;returns ISI for last row of weather data

;; elevation data for each fire, as a vector of vectors
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
#_(:m1 elevation-master)

;; elevation data for each fire, as a flattened vector
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
#_(:m1 elevation-master-flattened)

;; slope data for each fire, as a vector of vectors
;; note that arrowhead 2 slope is missing data, so currently is unusable
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
#_(:m1 slope-master)

;; slope data for each fire, as a flattened vector
;; note that arrowhead 2 slope is missing data, so currently is unusable
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
#_(:m1 slope-master-flattened)

;; contains the ignition cell for each fire
;; note that these ignition cells are 1-indexed, this is dealt with later
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
#_(:m1 ignition-cell-master)

(defn construct-empty-grid
  "Returns a vector of vectors filled with 0s with the same dimensions as the specified fire"
  [fire-name]
  (vec (repeat (count ((keyword (name fire-name)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire-name)) elevation-master))) 0)))))
#_(construct-empty-grid "m1")

(defn construct-initial-grid
  "Returns initial grid (vector of vectors) with a 1 where the ignition cell is.
   Note that theignition cell values are 1 indexed, so we subtract 1 from its value
   when placing it in the initial grid"
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

;; our initial grids for each of the 10 fires
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

;; our initial grids for each of the 10 fires flattened
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
#_(:m1 initial-fire-grids-flattened)

;; our initial time grids for each of the 10 fires flattened
;; (used to keep track of time each cell has been burning
;; so all grids initialized to 0 everywhere)
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
#_(:m1 initial-time-grids-flattened)

