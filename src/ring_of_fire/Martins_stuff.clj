(ns ring-of-fire.Martins-stuff)


;;;;;martin stuff
(def conway-instructions
  (list
    'exec_if
    'boolean_and
    'boolean_or
    'boolean_not
    'boolean_=
    'close
    0
    1))

(defn conway-target-function [input]
  (let [UL (nth input 0)
        UU (nth input 1)
        UR (nth input 2)
        LL (nth input 3)
        prev (nth input 4)
        RR (nth input 5)
        DL (nth input 6)
        DD (nth input 7)
        DR (nth input 8)
        live-neighbors (reduce + [UL UU UR LL RR DL DD DR])]
    (if (= prev 1)                                          ;was previously alive
      (if (or (= live-neighbors 2) (= live-neighbors 3))
        1
        0)
      (if (= live-neighbors 3)
        1
        0))))

;;my data methods
(defn build-data [world]
  (partition 9
             (flatten
               (for [x (range size)]
                 (for [y (range size)]
                   (get-input-data world x y))))))


(defn build-individual [world x y]
  (let [world-2 (step-forward world)]
    ;(print-world world)
    ;(print-world world-2)
    (concat
      (get-input-data world x y) (get-output-data world-2 x y))))

#_(build-individual (random-world) 1 1)

(defn get-output-data [world x y]
  (if (= "1" (nth (nth world x) y))
    '(1)
    '(0)))
#_(get-output-data (random-world) 1 1)

#_(get-input-data (random-world) 1 1)
(defn get-input-data [world x y]
  (for [i [-1 0 1]
        j [-1 0 1]]
    (if (= "1" (nth (nth world (mod (+ x i) size))
                    (mod (+ y j) size)))
      1
      0)))


(def conway-inputs (build-data (random-world)))

;;;;;martin stuff
