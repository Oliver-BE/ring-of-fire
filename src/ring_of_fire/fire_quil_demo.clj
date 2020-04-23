(ns ring-of-fire.fire-quil-demo
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def size 20)

;; creates a lazy sequence of lazy sequences that randomly fills in a world grid
(defn random-world []
  (repeatedly size
              (fn []
                ;; available, burned, burning, non-flammable cells respectively
                (repeatedly size #(rand-nth [" " "B" "B!" "X"])))))

;; code to visually check count live neighbors by corroborating
;; the number with the printed out world
;; (let [world (random-world)
;;  neigh (burning-neighbors world 1 1)]
;;  (print-world world)
;;  (println neigh)
;;  (or (= neigh 3) (= neigh 6)))

(defn print-world [world]
  (println "-----")
  (doseq [row world]
    (println row)))

;; counts burning neighbors
(defn burning-neighbors [world x y]
  ;; goes through 3 by 3 grid of neighboring cells
  (reduce + (for [i [-1 0 1]
                  j [-1 0 1]]
              ;; skips over cell that you are in
              (if (= i j 0)
                0
                ;; checks for burning cells and deals with wrapping around (edge cases)
                (if (= "B!" (nth (nth world (mod (+ x i) size))
                                 (mod (+ y j) size)))
                  1
                  0)))))

;; implements naive rules of fire, returns a new lazy sequence of lazy sequences
;; aka a new world
(defn step-forward [world]
  ;; go through entire grid
  (for [x (range size)]
    (for [y (range size)]
      (let [neigh (burning-neighbors world x y)
            state (nth (nth world x) y)]
        ;; check to see if you're available
        (if (= " " state)
          ;; TRUE: become burning if 2 burning neighbors or more
          (if (>= neigh 2) "B!" " ")
          ;; FALSE: if you're not available then check if burning
          (if (= "B!" state)
            ;; TRUE: if you are, become burned if 4 burning neighbors
            (if (= neigh 3) "B" "B!")
            ;; FALSE: if you're not burning don't change your state
            state))))))


;; quil stuff

(defn setup []
  (q/no-stroke)
  (q/frame-rate 10)
  (random-world))

(defn draw-world [world]
  (dotimes [r size]
    (dotimes [c size]
      (let [state (nth (nth world r) c)]
        ;; if burning then red
        (q/fill (if (= state "B!")
                  (q/color 255 0 0)
                  ;; else if burned then yellow
                  (if (= state "B")
                    (q/color 255 255 0)
                    ;; else if un-flammable then black
                    (if (= state "X")
                      (q/color 0 0 0)
                      ;; else its available so make it white
                      (q/color 255 255 255)
                      )))))

      (q/rect (* r 10) (* c 10) 10 10))))

(q/defsketch life
             :host "host"
             :size [200 200]
             :setup setup
             :update step-forward
             :draw draw-world
             :middleware [m/fun-mode])