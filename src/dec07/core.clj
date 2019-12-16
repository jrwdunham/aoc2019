(ns dec07.core
  (:require [clojure.string :as str]))

(def input-path "resources/input-dec07.txt")

(defn dig->int
  [dig]
  (-> dig
      (str/replace #"^0*(.+)$" "$1")
      read-string))

(defn parse
  "Parse an input program string to a vector of integers."
  [input]
  (->> (str/split input #",")
       (map read-string)
       vec))

(defn get-moded-vals
  "Given `program` (vec[int]) at index `cur-pos` (int), return a 6-ary vector of
  ints whose elements are an opcode, the three values following the opcode in the
  program (p1-p3), and two 'moded' parameter values, which may be the immediate
  values of p1 and p2, or else the positional values (in the program) at indices
  p1 and p2. Whether a value is immediate or positional is determined by a
  parsing of the opcode."
  [program cur-pos]
  (let [[op p1 p2 p3] (drop cur-pos program)
        [pos1 pos2] (map #(get program %) [p1 p2])]
    (if (<= op 99)
      [op p1 p2 p3 pos1 pos2]
      (let [[opcode-2 opcode-1 p1-mode p2-mode]
            (concat (-> op str reverse seq) (repeat \0))
            [op p1-mode p2-mode]
            (map (comp dig->int str)
                 [(str opcode-1 opcode-2) p1-mode p2-mode])]
        [op p1 p2 p3 (if (= 0 p1-mode) pos1 p1) (if (= 0 p2-mode) pos2 p2)]))))

(defn run
  "Run `program` starting at index `cur-pos`. Supply the program with vec of ints
  `inputs` and return an int output. Note: the inputs ffirst input will b"
  ([program inputs] (run program inputs 0))
  ([program inputs cur-pos]
   (let [[op p1 _ p3 v1 v2] (get-moded-vals program cur-pos)
         op-v1-v2->p3 (fn [op]
                       (run (assoc program p3 (op v1 v2)) inputs (+ cur-pos 4)))
         op-v1->v2 (fn [op] (if (op 0 v1) (run program inputs (+ cur-pos 3))
                                (run program inputs v2)))
         op-v1-v2->characteristic->p3
         (fn [op] (run (assoc program p3 (if (op v1 v2) 1 0))
                    inputs (+ cur-pos 4)))]
      (case op
        1 (op-v1-v2->p3 +)
        2 (op-v1-v2->p3 *)
        3 (let [[first-input & rest-inputs] inputs
                next-inputs (if rest-inputs rest-inputs [first-input])]
            (run (assoc program p1 first-input) next-inputs (+ cur-pos 2)))
        4 (run program [v1] (+ cur-pos 2))
        5 (op-v1->v2 =)
        6 (op-v1->v2 not=)
        7 (op-v1-v2->characteristic->p3 <)
        8 (op-v1-v2->characteristic->p3 =)
        99 (first inputs)))))

(defn run-halting
  "Run `program` at position `cur-pos` with `inputs` (vec[int]). Returns a 2-vec
  where the first element is an output and the second is either `nil` (the
  termination case) or a continuation function that takes a new input and
  continues running the program from the position where it had previously
  halted."
  ([program inputs] (run-halting program inputs 0))
  ([program inputs cur-pos]
   (let [[op p1 _ p3 v1 v2] (get-moded-vals program cur-pos)
         op-v1-v2->p3
         (fn [op] (run-halting (assoc program p3 (op v1 v2)) inputs
                               (+ cur-pos 4)))
         op-v1->v2
         (fn [op] (if (op 0 v1)
                    (run-halting program inputs (+ cur-pos 3))
                    (run-halting program inputs v2)))
         op-v1-v2->characteristic->p3
         (fn [op] (run-halting (assoc program p3 (if (op v1 v2) 1 0))
                               inputs (+ cur-pos 4)))]
      (case op
        1 (op-v1-v2->p3 +)
        2 (op-v1-v2->p3 *)
        3 (run-halting (assoc program p1 (first inputs)) (rest inputs)
                       (+ cur-pos 2))
        4 [v1 (fn [input] (run-halting program [input] (+ cur-pos 2)))]
        5 (op-v1->v2 =)
        6 (op-v1->v2 not=)
        7 (op-v1-v2->characteristic->p3 <)
        8 (op-v1-v2->characteristic->p3 =)
        99 [(first inputs) nil]))))

(defn run-program-in-feedback-loop
  "Run `(count phase-settings)` instances of `program` in series, feeding phase
  setting i to program i as first input. Input `input` will be provided as the
  second argument to the first program and its output will be provided as the
  second argument to the second program, etc. When a program returns an output,
  it may optionally also return a continuation, a function that takes an input
  and continues execution of the program from the last state. If the series
  completes and continuations are available, the series, now represented in the
  recursive call as a vector of continuation functions in `functions`, is run
  again with the final output of the previous series' output being provided as
  the intput to the new series. If no continuations are available, the final
  program's output is returned."
  [program phase-settings functions input]
  (let [fns (or functions
                (map (fn [ps] (fn [in] (run-halting program [ps in])))
                     phase-settings))
        {new-series :amplifiers output :in}
        (reduce (fn [{:keys [in] :as acc} f]
                  (let [[output continuation] (f in)
                        new-acc (assoc acc :in output)]
                    (if continuation
                      (update new-acc :amplifiers conj continuation)
                      new-acc)))
                {:in input :amplifiers []}
                fns)]
    (if (seq new-series)
      (run-program-in-feedback-loop nil nil new-series output)
      output)))

(defn run-amplifiers
  [program & [psa psb psc psd pse]]
  (reduce (fn [input ps] (run program [ps input]))
          0
          [psa psb psc psd pse]))

(defmacro get-permutations
  [lowest highest size]
  (let [syms (vec (repeatedly size gensym))]
    `(for [~@(mapcat (fn [s] [s `(range ~lowest ~(inc highest))]) syms)
           :when (= ~size (count (set ~syms)))]
       ~syms)))

(defn find-highest-signal
  [program]
  (->> (get-permutations 0 4 5)
       (map (fn [p] (apply run-amplifiers program p)))
       sort
       last))

(defn find-highest-feedback-loop-signal
  [program]
  (->> (get-permutations 5 9 5)
       (map (fn [p] (run-program-in-feedback-loop program p nil 0)))
       sort
       last))
