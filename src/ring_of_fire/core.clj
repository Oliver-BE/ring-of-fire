<<<<<<< HEAD:ring_of_fire/src/core.clj
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
          (update-grid current-grid time fire-string))))


;update each fire gird from one time step to the next
(defn update-grid [current-grid time fire-string]
  (fn [input]
    (peek-stack
      (interpret-program
        program
        (assoc empty-push-state :input {:in1 input})
        (:step-limit argmap))
      :integer)))

=======
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
>>>>>>> 8f9dcdab4fcfca50774e0b9196a93d828123497a:src/ring_of_fire/core.clj

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
  "Returns the number of rows of a vector of vectors"
  [grid]
  (count grid))
#_(num-rows (:m1 empty-cell-grids))
#_(num-rows mica1-forest)
#_(num-rows mica1-elevation)

(defn num-columns
  "Returns the number of rows of a vector of vectors"
  [grid]
  (count (nth grid 0)))
#_(num-columns (:m1 empty-cell-grids))
#_(num-columns mica1-forest)
#_(num-columns mica1-elevation)


;; function to place correct initial burning point into a grid
;; use ignition-cell-master to find cell number, add it to the empty-cell-grid
(defn create-fuel-grid
  "Takes in appropriate fire name and uses its forest grid to return new grid
  where 0 refers to non-fuel and 1 refers to burnable / fuel"
  [fire-name]
  (let [grid ((keyword (name fire-name)) forest-master)]
    (partition (num-columns grid)
               (map #(if (and (>= % 100) (<= % 105))
                       (* 0 %)
                       (+ 1 (* 0 %))
                       )
                    (flatten grid)))))
#_(create-fuel-grid "m1")
#_(:m1 forest-master)
#_(:m1 final-scar-grid-master)

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

;; add functionality so we only update cells that are burning
;; or having at least one burning neighbor and that they are not
;; non-fuel cells (use fuel-master to check this)
;; in general:
;; 0 means unburned, 1 means burning, 2 means burned

(defn update-grid
  "Updates a fire grid from one time step to the next"
  [fire-grid]
  (partition (num-columns fire-grid)
             (map update-cell (flatten fire-grid))))


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

