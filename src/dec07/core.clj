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
  "TODO: rename!"
  ([program inputs] (run-halting program inputs 0))
  ([program inputs cur-pos]

   #_(println
    (format
     (str "Running program\n%s\n"
          "at position\n%s\n%s\n"
          "with inputs\n%s\n") program cur-pos (nth program cur-pos) inputs))

   (let [[op p1 _ p3 v1 v2] (get-moded-vals program cur-pos)
         op-v1-v2->p3
         (fn [op]
           #_(println "Setting value at position %s to %s" p3 (op v1 v2))
           (run-halting (assoc program p3 (op v1 v2)) inputs (+ cur-pos 4)))
         op-v1->v2 (fn [op] (if (op 0 v1) (run-halting program inputs (+ cur-pos 3))
                                (run-halting program inputs v2)))
         op-v1-v2->characteristic->p3
         (fn [op] (run-halting (assoc program p3 (if (op v1 v2) 1 0))
                    inputs (+ cur-pos 4)))]
      (case op
        1 (op-v1-v2->p3 +)
        2 (op-v1-v2->p3 *)

        3 (let [[first-input & rest-inputs] inputs]
            #_(when-not rest-inputs (println "WARNING: HAVE NO MORE INPUTS\n"))
            (run-halting (assoc program p1 first-input) rest-inputs (+ cur-pos 2)))

        ;; 4 (run-halting program [v1] (+ cur-pos 2))
        ;; 4 [v1 (fn [inputs] (run-halting program inputs (+ cur-pos 2)))]
        ;; return an output continuation
        4 (do
            #_(println
             (format (str "program\n%s\nis returning intermediate output\n%s\nand"
                          " will restart at position\n%s\n%s\n"
                          "with new inputs.\n\n") program v1 (+ cur-pos 2)
                          (nth program (+ cur-pos 2))))
            [:continuation v1 (fn [inputs] (run-halting program [inputs]
                                                        (+ cur-pos 2)))])

        5 (op-v1->v2 =)
        6 (op-v1->v2 not=)
        7 (op-v1-v2->characteristic->p3 <)
        8 (op-v1-v2->characteristic->p3 =)
        99 [:termination (first inputs) nil]))))

(defn x
  [program phase-settings functions input]
  (let [fns (or functions
                (map (fn [ps] (fn [in] (run-halting program [ps in])))
                     phase-settings))
        {new-series :amplifiers output :in}
        (reduce (fn [{:keys [in amplifiers] :as acc} f]
                  #_(println "calling function" f)
                  #_(println "on vector of in [in]" [in])
                  (let [[state output continuation] (f in)]
                    (cond
                      (= state :termination) (assoc acc :in output)
                      output (-> acc
                                 (assoc :in output)
                                 (update :amplifiers conj continuation))
                      :else (update acc :amplifiers conj continuation))))
                {:in input :amplifiers []}
                fns)]
    (if (seq new-series)
      (x nil nil new-series output)
      output)))

(defn run-amplifiers
  [program & [psa psb psc psd pse]]
  (reduce (fn [input ps] (run program [ps input]))
          0
          [psa psb psc psd pse]))

(defn find-highest-signal
  [program]
  (-> (for [a (range 5) b (range 5) c (range 5) d (range 5) e (range 5)
            :when (= (sort [a b c d e]) (sort (vec (set [a b c d e]))))]
        (run-amplifiers program a b c d e))
      sort
      last))

(defn find-highest-feedback-loop-signal
  [program]
  (-> (for [a (range 5 10) b (range 5 10) c (range 5 10) d (range 5 10) e (range 5 10)
            :when (= (sort [a b c d e]) (sort (vec (set [a b c d e]))))]
        (x program [a b c d e] nil 0))
      sort
      last))
