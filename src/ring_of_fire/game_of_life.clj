(ns ring-of-fire.game-of-life
  (:use [clojure.repl]))

;; A version of Highlife Game of Life that prints the world to the REPL
;; Lee Spector (lspector@amherst.edu), 2020

(def size 20)

;; creates a lazy sequence of lazy sequences that randomly fills in a world grid
(defn random-world []
  (repeatedly size
              (fn []
                (repeatedly size #(rand-nth [" " "*"])))))

;; to check type of random-world output
;; (type (repeatedly size
;;                  (fn []
;; (repeatedly size #(rand-nth [" " "*"])))))

;; prints world to the screen
(defn print-world [world]
  (println "-----")
  ;; row is a name for "world"
  ;; there is an implicit for loop at work here which prints out
  ;; each lazy sequence from world in order (remember world is a lazy
  ;; sequence of lazy sequences
  (doseq [row world]
    (println row)))

;; counts live neighbors
(defn live-neighbors [world x y]
  ;; goes through 3 by 3 grid of neighboring cells
  (reduce + (for [i [-1 0 1]
                  j [-1 0 1]]
              ;; skips over cell that you are in
              (if (= i j 0)
                0
                ;; checks for alive cells and deals with wrapping around (edge cases)
                (if (= "*" (nth (nth world (mod (+ x i) size))
                                (mod (+ y j) size)))
                  1
                  0)))))

;; implements actual rules of life, returns a new lazy sequence of lazy sequences
;; aka a new world (uses highlife rules)
(defn step-forward [world]
  ;; go through entire grid
  (for [x (range size)]
    (for [y (range size)]
      (let [neigh (live-neighbors world x y)]
        ;; check to see if you're dead to begin with
        (if (= " " (nth (nth world x) y))
          ;; then check rules of life if dead
          (if (or (= neigh 3) (= neigh 6)) "*" " ")
          ;; check rules of life if alive
          (if (<= 2 neigh 3) "*" " "))))))


;; code to visually check count live neighbors
;; (let [world (random-world)
;;  neigh (live-neighbors world 1 1)]
;;  (print-world world)
;;  (println neigh)
;;  (or (= neigh 3) (= neigh 6)))

;; running game of live
(defn life [steps]
  (loop [world (random-world)
         step 0]
    ;; at every step print the world
    (print-world world)
    (if (>= step steps)
      :done
      ;; make sure we increment the world each time using step-forward
      (recur (step-forward world)
             (inc step)))))

;; Evaluate the following to run the game, starting with a random world,
;; for 50 steps:
#_(life 50)
