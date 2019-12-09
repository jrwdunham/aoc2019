(ns dec09.core
  (:require [clojure.string :as str]))

(def input-path "resources/input-dec09.txt")

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

(defn get-moded-parameter-value
  [mode positional-value immediate-value relative-value]
  (case mode
    0 positional-value
    1 immediate-value
    relative-value))

(defn get-moded-vals
  "Given `program` (vec[int]) at index `cur-pos` (int), return a 6-ary vector of
  ints whose elements are an opcode, the three values following the opcode in the
  program (p1-p3), and two 'moded' parameter values, which may be the immediate
  values of p1 and p2, or else the positional values (in the program) at indices
  p1 and p2. Whether a value is immediate or positional is determined by a
  parsing of the opcode."
  [program cur-pos relative-base]
  (let [[op p1 p2 p3] (drop cur-pos program)
        op (format "%04d" op)
        [pos1 pos2] (map #(get program %) [p1 p2])
        [rel1 rel2] (map #(get program (+ relative-base %)) [p1 p2])
        [opcode-2 opcode-1 p1-mode p2-mode]
        (concat (-> op reverse seq) (repeat \0))
        [op p1-mode p2-mode]
        (map (comp dig->int str)
             [(str opcode-1 opcode-2) p1-mode p2-mode])]
    [op p1 p2 p3
     (get-moded-parameter-value p1-mode pos1 p1 rel1)
     (get-moded-parameter-value p2-mode pos2 p2 rel2)]))

(defn run-halting
  "Run `program` at position `cur-pos` with `inputs` (vec[int]). Returns a 2-vec
  where the first element is an output and the second is either `nil` (the
  termination case) or a continuation function that takes a new input and
  continues running the program from the position where it had previously
  halted."
  ([program inputs] (run-halting program inputs 0 0))
  ([program inputs cur-pos relative-base]
   (let [[op p1 _ p3 v1 v2] (get-moded-vals program cur-pos relative-base)
         op-v1-v2->p3
         (fn [op] (run-halting (assoc program p3 (op v1 v2)) inputs
                               (+ cur-pos 4) relative-base))
         op-v1->v2
         (fn [op] (if (op 0 v1)
                    (run-halting program inputs (+ cur-pos 3) relative-base)
                    (run-halting program inputs v2 relative-base)))
         op-v1-v2->characteristic->p3
         (fn [op] (run-halting (assoc program p3 (if (op v1 v2) 1 0))
                               inputs (+ cur-pos 4) relative-base))]
      (case op
        1 (op-v1-v2->p3 +)
        2 (op-v1-v2->p3 *)
        3 (run-halting (assoc program p1 (first inputs)) (rest inputs)
                       (+ cur-pos 2) relative-base)
        4 [v1 (fn [input] (run-halting program (or (and input [input]) [])
                                       (+ cur-pos 2) relative-base))]
        5 (op-v1->v2 =)
        6 (op-v1->v2 not=)
        7 (op-v1-v2->characteristic->p3 <)
        8 (op-v1-v2->characteristic->p3 =)
        9 (run-halting program inputs (+ cur-pos 2) (+ relative-base v1))
        99 [(first inputs) nil]))))
