(ns dec09.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

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

(defn get-cmd-str
  [{:keys [program cur-pos]} l]
  (str/join "," (->> program
                     (drop cur-pos)
                     (take l)
                     (map str))))

(defn get-moded-parameter-value
  [mode positional-value immediate-value relative-value debug?]
  (case mode
    0 positional-value
    1 immediate-value
    relative-value))

(defn get-param-val
  [program memory index]
  (get program index (get memory index 0)))

(defn get-moded-parameter
  [p mode relative-base]
  (if (= 2 mode)
    (+ p relative-base)
    p))

(defn get-params
  "Given `program` (vec[int]) at index `cur-pos` (int), return a 6-ary vector of
  ints whose elements are an opcode, the three values following the opcode in the
  program (p1-p3), and two 'moded' parameter values, which may be the immediate
  values of p1 and p2, or else the positional values (in the program) at indices
  p1 and p2. Whether a value is immediate or positional is determined by a
  parsing of the opcode."
  [{:keys [program cur-pos relative-base memory output debug?]
    :or {cur-pos 0 relative-base 0 memory {} output []}
    :as state}]
  (let [getter (partial get-param-val program memory)
        relative-getter (fn [index] (when index (getter (+ index relative-base))))
        [op p1 p2 p3] (drop cur-pos program)
        op (try (format "%04d" op)
                (catch java.lang.StackOverflowError e
                  (throw (Exception.
                          (format
                           (str "Exception attempting to call format percent 04d"
                                " on input %s (%s). Exception %s.")
                           op (type op) (str e))))))
        [pos1 pos2] (mapv getter [p1 p2])
        [rel1 rel2] (mapv relative-getter [p1 p2])
        [opcode-2 opcode-1 p1-mode p2-mode p3-mode]
        (concat (-> op reverse seq) (repeat \0))
        [op p1-mode p2-mode p3-mode]
        (mapv (comp dig->int str)
              [(str opcode-1 opcode-2) p1-mode p2-mode p3-mode])
        v1 (get-moded-parameter-value p1-mode pos1 p1 rel1 debug?)
        v2 (get-moded-parameter-value p2-mode pos2 p2 rel2 debug?)
        [p1 p2 p3] (mapv get-moded-parameter
                         [p1 p2 p3]
                         [p1-mode p2-mode p3-mode]
                         (repeat relative-base))]
    (merge
     state
     {:params
      {:op op :p1 p1 :p2 p2 :p3 p3 :v1 v1 :v2 v2}
      :cur-pos cur-pos
      :relative-base relative-base
      :output output
      :memory memory})))

(defn assoc-at-index
  [{:keys [program] :as state} index val]
  (assoc-in
   state
   [(if (<= index (inc (count program))) :program :memory) index]
   val))

(defn arity-2
  [op {{:keys [p3 v1 v2]} :params debug? :debug? :as state} input]
  (when debug?
    (println (format "%s: set position %s to value %s"
                     (get-cmd-str state 4) p3 (op v1 v2))))
  [(-> state
       (assoc-at-index p3 (op v1 v2))
       (update :cur-pos + 4))
   input])

(defn store-input
  [{{:keys [p1]} :params debug? :debug? :as state} input]
  (when debug?
    (println (format "%s: store input %s at position p1 %s"
                     (get-cmd-str state 2) input p1)))
  [(-> state
       (assoc-at-index p1 input)
       (update :cur-pos + 2))
   input])

(defn output-v1
  [{{:keys [v1]} :params debug? :debug? :as state} input]
  (when debug?
    (println (format "%s: append v1 %s to output." (get-cmd-str state 2) v1)))
  [(-> state
       (update :cur-pos + 2)
       (update :output conj v1))
   input])

(defn jump?
  "Jump program to position `v2` if `v1` is int-wise truth/false-y. That is, If
  `relation-to-zero` is `not=`, and `v1` is `1`, then set current position to
  `v2`; otherwise continue on."
  [relation-to-zero {{:keys [v1 v2]} :params :as state} input]
  (if (relation-to-zero 0 v1)
    [(update state :cur-pos + 3) input]
    [(assoc state :cur-pos v2) input]))

(defn increase-relative-base
  "Increase the relative base by the value of `v1`."
  [{{:keys [v1]} :params debug? :debug? :as state} input]
  (when debug?
    (println (format "%s: change relative base from %s to %s"
                     (get-cmd-str state 2)
                     (:relative-base state)
                     (+ v1 (:relative-base state)))))
  [(-> state
       (update :cur-pos + 2)
       (update :relative-base + v1))
   input])

(defn return
  [{:keys [output]} _]
  output)

(defn run
  "Run `program` at position `cur-pos` with `inputs` (vec[int]). Returns a 2-vec
  where the first element is an output and the second is either `nil` (the
  termination case) or a continuation function that takes a new input and
  continues running the program from the position where it had previously
  halted."
  ([state] (run state nil))
  ([state input]
   (let [{{:keys [op]} :params :as state} (get-params state)]
     (if (= 99 op)
       (:output state)
       (let [[state input]
             (apply (case op
                      1 (partial arity-2 +)
                      2 (partial arity-2 *)
                      3 (partial store-input)
                      4 (partial output-v1)
                      5 (partial jump? =)
                      6 (partial jump? not=)
                      7 (partial arity-2 #(if (< %1 %2) 1 0))
                      8 (partial arity-2 #(if (= %1 %2) 1 0))
                      9 increase-relative-base)
                    [state input])]
         (recur state input))))))

