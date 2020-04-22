`(ns ring-of-fire.core
   (:use [clojure.repl])
   (:require [clojure.data.csv :as csv]
             [clojure.java.io :as io]
             [ring-of-fire.data :refer :all]))

#_(read-in-csv "data/Arrowhead/Fire1/FinalScarGrid.csv")



(defn error
  "Compares each cell in the two grids and finds the sum of differences between the two"
  [evolved-scar final-scar]
  (reduce + (map #(Math/abs (reduce - %))
                 (partition 2 (interleave (flatten evolved-scar) (flatten final-scar))))))
;; this should have an error of 1 as only one value is different
#_(error [[1 0 0] [0 0 1]] [[1 0 0] [0 1 1]])
;; this should have an error of 0 since it's the same fire scar twice
#_(error arrowhead1-final-scar arrowhead1-final-scar)


;;;;;;;;;;;;;;;;
;; Test pairs ;;
;;;;;;;;;;;;;;;;

;; each fire has input (forest, ignition cell, elevation, slope, weather)
;; matched with its output (final fire scar)

(def test-pairs [[[arrowhead1-forest arrowhead1-ignition-cell arrowhead1-elevation arrowhead1-slope arrowhead1-weather] arrowhead1-final-scar]
                 [[arrowhead2-forest arrowhead2-ignition-cell arrowhead2-elevation arrowhead2-slope arrowhead2-weather] arrowhead2-final-scar]
                 [[kootenay1-forest kootenay1-ignition-cell kootenay1-elevation kootenay1-slope kootenay1-weather] kootenay1-final-scar]
                 [[kootenay2-forest kootenay2-ignition-cell kootenay2-elevation kootenay2-slope kootenay2-weather] kootenay2-final-scar]
                 [[glacier1-forest glacier1-ignition-cell glacier1-elevation glacier1-slope glacier1-weather] glacier1-final-scar]
                 [[glacier2-forest glacier2-ignition-cell glacier2-elevation glacier2-slope glacier2-weather] glacier2-final-scar]
                 [[mica1-forest mica1-ignition-cell mica1-elevation mica1-slope mica1-weather] mica1-final-scar]
                 [[mica2-forest mica2-ignition-cell mica2-elevation mica2-slope mica2-weather] mica2-final-scar]
                 [[revelstoke1-forest revelstoke1-ignition-cell revelstoke1-elevation revelstoke1-slope revelstoke1-weather] revelstoke1-final-scar]
                 [[revelstoke2-forest revelstoke2-ignition-cell revelstoke2-elevation revelstoke2-slope revelstoke2-weather] revelstoke2-final-scar]])

(def example-push-state
  {:exec    '()
   :integer '(1 2 3 4 5 6 7)
   :string  '("abc")
   :input   {:in1 4}})

; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; TMH: ERCs?
(def default-instructions
  (list
    'elevation
    'slope
    'APCP
    'TMP
    'RH
    'WS
    'WD
    'FFMC
    'DMC
    'DC
    'ISI
    'BUI
    'FWI
    'integer_+
    'integer_-
    'integer_*
    'integer_%
    'integer_=
    'exec_dup
    'exec_if
    'boolean_and
    'boolean_or
    'boolean_not
    'boolean_=
    'string_=
    'string_take
    'string_drop
    'string_reverse
    'string_concat
    'string_length
    'string_includes?
    'close
    0
    1
    true
    false
    ""
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "A"
    "C"
    "G"
    "T"))

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


(def opens                                                  ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1
   'exec_if  2})

;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec    '()
   :integer '()
   :string  '()
   :boolean '()
   :input   {}})

(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))

(defn not-lazy
  "Returns lst if it is not a list, or a non-lazy version of lst if it is."
  [lst]
  (if (seq? lst)
    (apply list lst)
    lst))

(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state and :args is a list of args from the stacks. If there
  aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Instructions

(defn elevation
  "Pushes the input labeled :elevation on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:elevation (:input state))))

(defn slope
  "Pushes the input labeled :slope on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:slope (:input state))))

(defn APCP
  "Pushes the input labeled :APCP on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:APCP (:input state))))

(defn TMP
  "Pushes the input labeled :TMP on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:TMP (:input state))))

(defn RH
  "Pushes the input labeled :RH on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:RH (:input state))))

(defn WS
  "Pushes the input labeled :WS on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:WS (:input state))))

(defn WD
  "Pushes the input labeled :WD on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:WD (:input state))))

(defn FFMC
  "Pushes the input labeled :FFMC on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:FFMC (:input state))))

(defn DMC
  "Pushes the input labeled :DMC on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:DMC (:input state))))

(defn DC
  "Pushes the input labeled :DC on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:DC (:input state))))

(defn ISI
  "Pushes the input labeled :isi on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:ISI (:input state))))

(defn BUI
  "Pushes the input labeled :BUI on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:BUI (:input state))))

(defn FWI
  "Pushes the input labeled :FWI on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:FWI (:input state))))

(defn integer_+
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  [state]
  (make-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             int1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (make-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))

(defn boolean_and
  [state]
  (make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))

(defn string_=
  [state]
  (make-push-instruction state = [:string :string] :boolean))

(defn string_take
  [state]
  (make-push-instruction state
                         #(apply str (take %1 %2))
                         [:integer :string]
                         :string))

(defn string_drop
  [state]
  (make-push-instruction state
                         #(apply str (drop %1 %2))
                         [:integer :string]
                         :string))

(defn string_reverse
  [state]
  (make-push-instruction state
                         #(apply str (reverse %))
                         [:string]
                         :string))

(defn string_concat
  [state]
  (make-push-instruction state
                         #(apply str (concat %1 %2))
                         [:string :string]
                         :string))

(defn string_length
  [state]
  (make-push-instruction state count [:string] :integer))

(defn string_includes?
  [state]
  (make-push-instruction state clojure.string/includes? [:string :string] :boolean))

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-raw (first (:exec state))
        first-instruction (if (symbol? first-raw)
                            (eval first-raw)
                            first-raw)]
    (cond
      (fn? first-instruction)
      (first-instruction popped-state)
      ;
      (integer? first-instruction)
      (push-to-stack popped-state :integer first-instruction)
      ;
      (string? first-instruction)
      (push-to-stack popped-state :string first-instruction)
      ;
      (seq? first-instruction)
      (update popped-state :exec #(concat %2 %1) first-instruction)
      ;
      (or (= first-instruction true) (= first-instruction false))
      (push-to-stack popped-state :boolean first-instruction)
      ;
      :else
      (throw (Exception. (str "Unrecognized Push instruction in program: "
                              first-instruction))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))]     ;; [open <n>] marks opens
    (loop [push ()                                          ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)                                   ;; maybe we're done?
        (if (some opener? push)                             ;; done with plushy, but unclosed open
          (recur push '(close))                             ;; recur with one more close
          push)                                             ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push)                         ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy)))                   ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy))))))))   ;; anything else

;;;;;;;;;
;; GP

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))

(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap)))
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap)))))})

(defn report
  "Reports information each generation."
  [pop generation]
  (let [best (first pop)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    (println "Best errors:" (:errors best))
    (println "Best behaviors:" (:behaviors best))
    (println)))

(defn propel-gp
  "Main GP loop."
  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size]
    :as   argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                      population-size
                      #(hash-map :plushy
                                 (make-random-plushy instructions
                                                     max-initial-plushy-size)))]
    (let [evaluated-pop (sort-by :total-error
                                 (map (partial error-function argmap)
                                      population))]
      (report evaluated-pop generation)
      (cond
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size
                                 #(new-individual evaluated-pop argmap)))))))


(defn regression-error-function
  "Finds the behaviors and errors of an individual: Error is the absolute deviation between the target output value and the program's selected behavior, or 1000000 if no behavior is produced. The behavior is here defined as the final top item on the :integer stack."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs (repeatedly 50 (fn [] (repeatedly 9 #(rand-int 2))))
        correct-outputs (map #(conway-target-function %) inputs)
        outputs (map (fn [input]
                       (peek-stack
                         (interpret-program
                           program
                           (assoc empty-push-state :input {:in1 input})
                           (:step-limit argmap))
                         :integer))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (abs (- correct-output output))))
                    correct-outputs
                    outputs)]
    (assoc individual
      :behaviors outputs
      :errors errors
      :total-error (apply +' errors))))





(defn run-fire [fire-string]
  (loop [current-grid (initial-fire-grid fire-string)
         time 0]
    (if (> time 1440)
      (recur
        (update-grid current-grid time fire-string)
        (inc time))
      (update-grid current-grid time fire-string)
      )))


;update each fire gird from one time step to the next
(defn update-grid [current-grid time fire-string]
  (fn [input]
    (peek-stack
      (interpret-program
        program
        (assoc empty-push-state :input {:in1 input})
        (:step-limit argmap))
      :integer)))



;;;BEGIN ISAAC ;;;;;;;;;;;;;;;;;;;;;

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

(defn construct-empty-grid
  ;;Returns a vector of vectors filled with 0s with the same dimensions as the specified fire
  [fire]
  (vec (repeat (count ((keyword (name fire)) elevation-master))
               (vec (repeat (count (first ((keyword (name fire)) elevation-master))) 0)))))

(defn get-neighbors
  ;;Returns a sequence of sequences containing each neighbor's pertinent information
  [cell-id fire current-grid]
  (vec (burning-neighbors cell-id current-grid))
  )

(defn burning-neighbors
  ;;Returns the burning neighbors of a given cell
  [cell-id current-grid]
  (let [flat-grid (flatten current-grid)]
    (if (= 1 (nth flat-grid (- cell-id 1))) (- cell-id 1))
    (if (= 1 (nth flat-grid (+ cell-id 1))) (+ cell-id 1))
    ))

(defn get-cell [cell-id]
  )

(defn current-weather-var
  ;;Returns the value of a specified weather variable for a specified fire at a specified time.
  [desired-var fire time]
  ((keyword (name desired-var)) (nth ((keyword (name fire)) weather-master) (Math/floor (/ time 60)))))
#_(current-weather-var "FFMC" "a1" 54)

(defn elevation-at-cell
  ;;Returns the elevation of a specified cell in a specified fire
  [cell-id fire]
  (nth (flatten ((keyword (name fire)) elevation-master)) cell-id))
#_(elevation-at-cell 150 "r1")

(defn slope-at-cell
  ;;Returns the elevation of a specified cell in a specified fire
  [cell-id fire]
  (nth (flatten ((keyword (name fire)) slope-master)) cell-id))
#_(slope-at-cell 150 "r1")

(defn update-cell
  ;;Update cell to next state by interpreting push program
  [cell-id fire time current-state]
  (peek-stack
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
                                      :canBurn       (check-fuel cell-id fire)
                                      ;;add canBurn functionality
                                      :current-state current-state
                                      })
      (:step-limit argmap))
    :integer))

;;;  END ISAAC ;;;;;;;;;;;;;;;;;;;




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

;;;;;;;;;
;; String classification

(defn string-classification-error-function
  "Finds the behaviors and errors of an individual: Error is 0 if the value and the program's selected behavior match, or 1 if they differ, or 1000000 if no behavior is produced. The behavior is here defined as the final top item on the :boolean stack."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs ["GCG" "GACAG" "AGAAG" "CCCA" "GATTACA" "TAGG" "GACT"]
        correct-outputs [false false false false true true true]
        outputs (map (fn [input]
                       (peek-stack
                         (interpret-program
                           program
                           (assoc empty-push-state :input {:in1 input})
                           (:step-limit argmap))
                         :boolean))
                     inputs)
        errors (map (fn [correct-output output]
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

#_(propel-gp {:instructions            conway-instructions
              :error-function          conway-error-function
              :max-generations         500
              :population-size         50
              :max-initial-plushy-size 50
              :step-limit              100
              :parent-selection        :lexicase
              :tournament-size         5})



;;CONWAYS Game of life BELOW
(def size 10)

(defn random-world []
  (repeatedly size
              (fn []
                (repeatedly size #(rand-nth ["0" "1"])))))

(def random-desired-output (random-world))

(defn print-world [world]
  (println "-----")
  (doseq [row world]
    (println row)))

(defn live-neighbors [world x y]
  (reduce + (for [i [-1 0 1]
                  j [-1 0 1]]
              (if (= i j 0)
                0
                (if (= "1" (nth (nth world (mod (+ x i) size))
                                (mod (+ y j) size)))
                  1
                  0)))))

(defn step-forward [world]
  (for [x (range size)]
    (for [y (range size)]
      (let [neigh (live-neighbors world x y)]
        (if (= " " (nth (nth world x) y))
          (if (= neigh 3) "1" "0")
          (if (<= 2 neigh 3) "1" "0"))))))

(defn life [steps]
  (loop [world (random-world)
         step 0]
    (print-world world)
    (if (>= step steps)
      :done
      (recur (step-forward world)
             (inc step)))))


;; Evaluate the following to run the game, starting with a random world,
;; for 50 steps:
;;
#_(life 20)


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
