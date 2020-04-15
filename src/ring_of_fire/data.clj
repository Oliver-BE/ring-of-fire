(ns ring-of-fire.data
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-in-csv
  "Reads in a csv given a string holding the path to a file
  as a lazy sequence of lazy sequences"
  [path-to-file]
  (with-open [reader (io/reader path-to-file)]
    (doall
      (csv/read-csv reader))))
#_(read-in-csv "data/Arrowhead/Fire1/FinalScarGrid.csv")

(defn csv-to-dict
  "Converts CSV into a sequence of dictionaries"
  [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))
#_(csv-to-dict (read-in-csv "data/Arrowhead/Fire1/Weather.csv"))

(defn write-csv
  "Writes a lazy sequence of lazy sequences into a csv at a given
  source destination"
  [data file-name]
  (with-open [writer (io/writer file-name)]
    (csv/write-csv writer data)))
#_(write-csv [["abc" "def"] ["ghi" "jkl"]] "data/output.csv")

(defn clean-row
  "Helper function to clean data"
  [row]
  (map #(read-string %) (str/split (nth row 0) #" ")))

(defn clean
  "Cleans data in string format into longs"
  [data]
  (map #(clean-row %) data))
#_(clean '(["1 1 1"]["0 0 0"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialized functions for reading in different Cell2Fire data inputs   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-in-fire-scar
  "Returns a fully cleaned fire scar (a lazy sequence of lazy sequences)"
  [path]
  (clean (read-in-csv path)))


(defn read-in-asc
  "Returns a fully cleaned asc file as a lazy sequence of lazy sequences
  includes Forest.asc, elevation.asc, and slope.asc"
  [path]
  (drop 6 (clean (read-in-csv path))))


(defn read-in-ignition-cell
  "Returns the cell number that corresponds to the ignition point
  where the top left cell is 1, and that number increments from left to right
  and column by column"
  [path]
  (read-string (nth (nth (read-in-csv path) 1) 1)))

(defn read-in-weather
  "Returns a fully cleaned weather file as a sequence of
  dictionaries (each observation/row is its own dictionary)"
  [path]
  (csv-to-dict (read-in-csv path)))

(defn read-in-fbp-lookup
  "Returns a fully cleaned fbp lookup table file as a sequence of
  dictionaries (each observation/row is its own dictionary)"
  [path]
  (csv-to-dict (read-in-csv path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read in data as Clojure objects                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; Static Data      ;;
;;;;;;;;;;;;;;;;;;;;;;

;; the fbp-lookup-table is the same for every fire (just maps numbers in
;; forest.asc files to types of trees/fuel type/what color to map those squares
(def fbp-lookup-table (read-in-fbp-lookup "data/Arrowhead/fbp_lookup_table.csv"))

;;;;;;;;;;;;;;;;;;;;;;
;; Arrowhead Fire 1 ;;
;;;;;;;;;;;;;;;;;;;;;;

;; why does this print vertically???
(def arrowhead1-final-scar (read-in-fire-scar "data/Arrowhead/Fire1/FinalScarGrid.csv"))
(def arrowhead1-forest (read-in-asc "data/Arrowhead/Fire1/Forest.asc"))
(def arrowhead1-ignition-cell (read-in-ignition-cell "data/Arrowhead/Fire1/IgnitionPoints.csv"))
(def arrowhead1-elevation (read-in-asc "data/Arrowhead/Fire1/elevation.asc"))
(def arrowhead1-slope (read-in-asc "data/Arrowhead/Fire1/slope.asc"))
(def arrowhead1-weather (read-in-weather "data/Arrowhead/Fire1/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;
;; Arrowhead Fire 2 ;;
;;;;;;;;;;;;;;;;;;;;;;

(def arrowhead2-final-scar (read-in-fire-scar "data/Arrowhead/Fire2/FinalScarGrid.csv"))
(def arrowhead2-forest (read-in-asc "data/Arrowhead/Fire2/Forest.asc"))
(def arrowhead2-ignition-cell (read-in-ignition-cell "data/Arrowhead/Fire2/IgnitionPoints.csv"))
(def arrowhead2-elevation (read-in-asc "data/Arrowhead/Fire2/elevation.asc"))
;; NOTE we are missing some data here
(def arrowhead2-slope (read-in-asc "data/Arrowhead/Fire2/slope_missing.asc"))
#_(count arrowhead2-slope) ;;73 (should be 77 as below)
#_(count arrowhead2-forest) ;;77
#_(count arrowhead2-elevation) ;;77
(def arrowhead2-weather (read-in-weather "data/Arrowhead/Fire2/Weather.csv"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Central Kootenay Fire 1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def kootenay1-final-scar (read-in-fire-scar "data/CentralKootenay/Fire1/FinalScarGrid.csv"))
(def kootenay1-forest (read-in-asc "data/CentralKootenay/Fire1/Forest.asc"))
(def kootenay1-ignition-cell (read-in-ignition-cell "data/CentralKootenay/Fire1/IgnitionPoints.csv"))
(def kootenay1-elevation (read-in-asc "data/CentralKootenay/Fire1/elevation.asc"))
(def kootenay1-slope (read-in-asc "data/CentralKootenay/Fire1/slope.asc"))
(def kootenay1-weather (read-in-weather "data/CentralKootenay/Fire1/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Central Kootenay Fire 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def kootenay2-final-scar (read-in-fire-scar "data/CentralKootenay/Fire2/FinalScarGrid.csv"))
(def kootenay2-forest (read-in-asc "data/CentralKootenay/Fire2/Forest.asc"))
(def kootenay2-ignition-cell (read-in-ignition-cell "data/CentralKootenay/Fire2/IgnitionPoints.csv"))
(def kootenay2-elevation (read-in-asc "data/CentralKootenay/Fire2/elevation.asc"))
(def kootenay2-slope (read-in-asc "data/CentralKootenay/Fire2/slope.asc"))
(def kootenay2-weather (read-in-weather "data/CentralKootenay/Fire2/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glacier National Park Fire 1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def glacier1-final-scar (read-in-fire-scar "data/GlacierNationalPark/Fire1/FinalScarGrid.csv"))
(def glacier1-forest (read-in-asc "data/GlacierNationalPark/Fire1/Forest.asc"))
(def glacier1-ignition-cell (read-in-ignition-cell "data/GlacierNationalPark/Fire1/IgnitionPoints.csv"))
(def glacier1-elevation (read-in-asc "data/GlacierNationalPark/Fire1/elevation.asc"))
(def glacier1-slope (read-in-asc "data/GlacierNationalPark/Fire1/slope.asc"))
(def glacier1-weather (read-in-weather "data/GlacierNationalPark/Fire1/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glacier National Park Fire 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def glacier2-final-scar (read-in-fire-scar "data/GlacierNationalPark/Fire2/FinalScarGrid.csv"))
(def glacier2-forest (read-in-asc "data/GlacierNationalPark/Fire2/Forest.asc"))
(def glacier2-ignition-cell (read-in-ignition-cell "data/GlacierNationalPark/Fire2/IgnitionPoints.csv"))
(def glacier2-elevation (read-in-asc "data/GlacierNationalPark/Fire2/elevation.asc"))
(def glacier2-slope (read-in-asc "data/GlacierNationalPark/Fire2/slope.asc"))
(def glacier2-weather (read-in-weather "data/GlacierNationalPark/Fire2/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mica Creek Park Fire 1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def mica1-final-scar (read-in-fire-scar "data/MicaCreek/Fire1/FinalScarGrid.csv"))
(def mica1-forest (read-in-asc "data/MicaCreek/Fire1/Forest.asc"))
(def mica1-ignition-cell (read-in-ignition-cell "data/MicaCreek/Fire1/IgnitionPoints.csv"))
(def mica1-elevation (read-in-asc "data/MicaCreek/Fire1/elevation.asc"))
(def mica1-slope (read-in-asc "data/MicaCreek/Fire1/slope.asc"))
(def mica1-weather (read-in-weather "data/MicaCreek/Fire1/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mica Creek Park Fire 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def mica2-final-scar (read-in-fire-scar "data/MicaCreek/Fire2/FinalScarGrid.csv"))
(def mica2-forest (read-in-asc "data/MicaCreek/Fire2/Forest.asc"))
(def mica2-ignition-cell (read-in-ignition-cell "data/MicaCreek/Fire2/IgnitionPoints.csv"))
(def mica2-elevation (read-in-asc "data/MicaCreek/Fire2/elevation.asc"))
(def mica2-slope (read-in-asc "data/MicaCreek/Fire2/slope.asc"))
(def mica2-weather (read-in-weather "data/MicaCreek/Fire2/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revelstoke Park Fire 1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def revelstoke1-final-scar (read-in-fire-scar "data/Revelstoke/Fire1/FinalScarGrid.csv"))
(def revelstoke1-forest (read-in-asc "data/Revelstoke/Fire1/Forest.asc"))
(def revelstoke1-ignition-cell (read-in-ignition-cell "data/Revelstoke/Fire1/IgnitionPoints.csv"))
(def revelstoke1-elevation (read-in-asc "data/Revelstoke/Fire1/elevation.asc"))
(def revelstoke1-slope (read-in-asc "data/Revelstoke/Fire1/slope.asc"))
(def revelstoke1-weather (read-in-weather "data/Revelstoke/Fire1/Weather.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revelstoke Park Fire 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def revelstoke2-final-scar (read-in-fire-scar "data/Revelstoke/Fire2/FinalScarGrid.csv"))
(def revelstoke2-forest (read-in-asc "data/Revelstoke/Fire2/Forest.asc"))
(def revelstoke2-ignition-cell (read-in-ignition-cell "data/Revelstoke/Fire2/IgnitionPoints.csv"))
(def revelstoke2-elevation (read-in-asc "data/Revelstoke/Fire2/elevation.asc"))
(def revelstoke2-slope (read-in-asc "data/Revelstoke/Fire2/slope.asc"))
(def revelstoke2-weather (read-in-weather "data/Revelstoke/Fire2/Weather.csv"))


