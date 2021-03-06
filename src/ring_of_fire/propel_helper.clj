(ns ring-of-fire.propel-helper
  (:use [clojure.repl])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ring-of-fire.data :refer :all]
    ;;[ring-of-fire.core :refer :all]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spector Propel Code      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable definitions     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(def example-push-state
  {:exec    '()
   :integer '(1 2 3 4 5 6 7)
   :string  '("abc")
   :input   {:in1 4}})

;; Instructions must all be either functions that take one Push state and return another
;; or constant literals.

(def fire-instructions
  (list
    ;; fire vars
    ;'elevation
    ;'current-value
    'slope
    'ISI
    'BUI
    'FWI
    'NT
    'NBD
    'WS
    'WD
    'TB
    ':split

    ;; other instructions
    'integer_+
    'integer_-
    'integer_*
    'integer_%
    'integer_=
    ;'exec_dup
    'exec_if
    'boolean_and
    'boolean_or
    'boolean_not
    'boolean_=
    'close
    0
    1
    2
    true
    false
    ))

#_(def unlimited-fire-instructions
  (list
    ;; fire vars
    ;'elevation
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
    'current-value
    'time-step
    'nw
    'n
    'ne
    'e
    'w
    'sw
    's
    'se
    'NT
    ;; include more here

    ;; other instructions
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

    ;; remove these?
    ;'string_=
    ;'string_take
    ;'string_drop
    ;'string_reverse
    ;'string_concat
    ;'string_length
    ;'string_includes?

    'close
    0
    1
    2
    true
    false
    ))


;; number of blocks opened by instructions (default = 0)
(def opens
  {'exec_dup 1
   'exec_if  2})


(def empty-push-state
  {:exec    '()
   :integer '()
   :string  '()
   :boolean '()
   :input   {}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we could probably condense these into one function where we push the state
;; and the desired input [state input] and then do (name input (state)) instead
;; of :input state

(defn elevation
  "Pushes the input labeled :elevation on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:elevation (:input state))))

(defn slope
  "Pushes the input labeled :slope on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:slope (:input state))))

#_(defn APCP
  "Pushes the input labeled :APCP on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:APCP (:input state))))

#_(defn TMP
  "Pushes the input labeled :TMP on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:TMP (:input state))))

#_(defn RH
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

(defn current-value
  "Pushes the input labeled :current-value on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:current-value (:input state))))

(defn TB
  "Pushes the input labeled :TB on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:TB (:input state))))

#_(defn nw
  "Pushes the input labeled :nw on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:nw (:input state))))

#_(defn n
  "Pushes the input labeled :n on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:n (:input state))))

#_(defn ne
  "Pushes the input labeled :ne on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:ne (:input state))))

#_(defn e
  "Pushes the input labeled :e on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:e (:input state))))


#_(defn w
  "Pushes the input labeled :w on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:w (:input state))))

#_(defn sw
  "Pushes the input labeled :sw on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:sw (:input state))))

#_(defn s
  "Pushes the input labeled :s on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:s (:input state))))

#_(defn se
  "Pushes the input labeled :se on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:se (:input state))))

(defn NBD
  "Pushes the input labeled :NBD on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:NBD (:input state))))

(defn NT
  "Pushes the input labeled :NT on the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:NT (:input state))))




;; add more fire instructions here


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


;; take these out probably

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
      ;;ALPHA CHECKKKK
      (nil? first-instruction)
      (throw (Exception. (str "ALPHA NIL EXCEPTION: " popped-state " FIRST RAW: " first-raw)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genetic program functions      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn one-individual-per-error-vector-for-lexicase
  "Returns population with only one randomly-chosen individual
  for any particular error vector."
  [population]
  (map rand-nth (vals (group-by :errors population))))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors (one-individual-per-error-vector-for-lexicase pop)
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn lexicase-selection-timed "Selects an individual from the population using lexicase selection including runtime."
  [pop argmap]
  (time (lexicase-selection pop argmap)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to get cell numbers from core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def final-scars {:a1 arrowhead1-final-scar
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
#_(:a1 final-scars)

(defn num-row
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count ((keyword (name fire-name)) final-scars)))
#_(num-rows "r1")
#_(num-rows mica1-forest)
#_(num-rows mica1-elevation)


(defn num-cols
  "Returns the number of rows of a vector of vectors
  given a fire name"
  [fire-name]
  (count (nth ((keyword (name fire-name)) final-scars) 0)))
#_(num-columns "r1")
#_(num-columns mica1-forest)
#_(num-columns mica1-elevation)

(defn tot-cells
  "Returns the total number of cells in a vector of fires"
  [fire-vec]
  (reduce + (map #(* (num-row %) (num-cols %)) fire-vec)))
#_(tot-num-cells ["a2" "m1" "m2"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn report
  "Reports information each generation."
  [pop generation chosen-selection]
  (let [best (first pop)
        num-cells (tot-cells chosen-selection)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (print "Best plushy: ") (prn (:plushy best))
    (print "Best program: ") (prn (push-from-plushy (:plushy best)))
    (println "Best total error:" (:total-error best))
    (println "Total number of cells:" num-cells)
    (println "Best percent error:" (float (/ (:total-error best) num-cells)))
    (println "Fires evaluated: " chosen-selection)
    ;Probably would like to print out the error in percentage form, so like 64% of cells were correctly predicted
    ;(println "Best errors:" (:errors best))
    ;Ideally we want to print out a comparison of the best predicted fire to the actual fire scar g
    ;(println "Best behaviors:" (:behaviors best))
    (println)))

; make our initial worlds for each of the 10 fires
(def fire-names ["k1", "k2", "g2", "m1", "m2", "r1"])

;our test set of fires
(def test-set-fire-names ["a1", "g1", "r2"])

(defn propel-gp
  "Main GP loop."
  [{:keys [fire-selection time-step population-size max-generations error-function instructions
           max-initial-plushy-size]
    :as   argmap}]
  (println "Starting GP with args:" argmap)
  (loop [generation 0
         population (repeatedly
                      population-size
                      #(hash-map :plushy
                                 (make-random-plushy instructions
                                                     max-initial-plushy-size)))]
    (let [chosen-selection (take fire-selection (shuffle fire-names))
          evaluated-pop (sort-by :total-error
                                 (pmap (partial error-function argmap chosen-selection)
                                       population))]
      (report evaluated-pop generation chosen-selection)
      (cond
        (zero? (:total-error (first evaluated-pop))) (println "SUCCESS")
        (>= generation max-generations) nil
        :else (recur (inc generation)
                     (repeatedly population-size
                                 #(new-individual evaluated-pop argmap)))))))
#_(propel-gp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spector example error functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn target-function-hard
  "Target function: f(x) = 7x^2 - 20x + 13"
  [x]
  (+ (* 7 x x)
     (* -20 x)
     13))

(defn target-function
  "Target function: f(x) = x^3 + x + 3"
  [x]
  (+ (* x x x)
     x
     3))

(defn regression-error-function
  "Finds the behaviors and errors of an individual: Error is the absolute deviation between the target output value and the program's selected behavior, or 1000000 if no behavior is produced. The behavior is here defined as the final top item on the :integer stack."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        inputs (range -10 11)
        correct-outputs (map target-function inputs)
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
