(ns ring-of-fire.scratch-work
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]
            [ring-of-fire.propel-helper :refer :all]))

(def fire-instructions
  (list
    'slope
    'ISI  ;;  Initial Spread Index
    'BUI  ;;  Buildup Index
    'FWI  ;;  Fire Weather Index
    'NT   ;;  Neighbor Average Time Burning
    'NBD  ;;  Net Burning Direction
    'WS   ;;  Wind Speed
    'WD   ;;  Wind Direction
    'TB   ;;  Time Burning
    ':split
    ;;  and some propel ones...
    ))

 (def a-program '(exec_dup (boolean_and WS boolean_not NT WD))

(defn fire-error-function
  "Calls run-fire on each fire in test set... and more on next slide ")
   ;;  ("kootenay1"        {:k1   [[0 0 1 0]     :a2    [[1 1 0 0]
   ;;   "arrohead2")  -->          [1 1 1 0]             [0 1 1 1]
   ;;                              [0 1 0 0]             [1 0 1 0]
   ;;                              [0 1 0 0]]            [0 1 1 1] }
   ;;
   ;;   Fire names    -->    Map of fire scars
(defn run-fire
  "Runs a fire for 24 hours and returns the final fire scar. ")
   ;;  [[0 0 0 0]           [[0 0 1 0]
   ;;   [0 1 0 0]     -->    [1 1 1 0]
   ;;   [0 0 0 0]            [0 1 0 0]
   ;;   [0 0 0 0]]           [0 1 0 0]]
   ;;
   ;;   Ignition pt.  -->    Fire scar

(defn update-grid
  "Updates a fire grid from one time step to the next")
   ;;  [[0 0 0 0]           [[0 0 0 0]
   ;;   [0 1 0 0]     -->    [0 1 1 0]
   ;;   [0 0 0 0]            [0 0 0 0]
   ;;   [0 0 0 0]]           [0 0 0 0]]
   ;;
   ;;   Minute 1      -->    Minute 2

(defn update-cell
  "Updates a cell to its next state by interpreting a push program")
   ;;       0         -->     0 || 1
   ;;       Unburned  -->     Unburned or Burning
   ;;
   ;;       1         -->     1 || 2
   ;;       Burning   -->     Burning or Burned




