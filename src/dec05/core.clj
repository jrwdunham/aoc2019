(ns dec05.core
  (:require [clojure.string :as str]))

(def program-path "resources/input-dec05.txt")

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
  "Run `program` starting at index `cur-pos`. Supply the program with int
  `input` and return an int output."
  ([program input] (run program input 0 false))
  ([program input cur-pos] (run program input cur-pos false))
  ([program input cur-pos return-program?]
   (let [[op p1 _ p3 v1 v2] (get-moded-vals program cur-pos)
         op-v1-v2->p3 (fn [op]
                       (run (assoc program p3 (op v1 v2)) input (+ cur-pos 4) return-program?))
         op-v1->v2 (fn [op] (if (op 0 v1) (run program input (+ cur-pos 3) return-program?)
                                (run program input v2 return-program?)))
         op-v1-v2->characteristic->p3
         (fn [op] (run (assoc program p3 (if (op v1 v2) 1 0))
                    input (+ cur-pos 4) return-program?))]
      (case op
        1 (op-v1-v2->p3 +)
        2 (op-v1-v2->p3 *)
        3 (run (assoc program p1 input) input (+ cur-pos 2) return-program?)
        4 (run program v1 (+ cur-pos 2) return-program?)
        5 (op-v1->v2 =)
        6 (op-v1->v2 not=)
        7 (op-v1-v2->characteristic->p3 <)
        8 (op-v1-v2->characteristic->p3 =)
        99 (if return-program? program input)))))

(defn run-TEST
  []
  (-> program-path slurp parse (run 1)))

(defn equals-8?
  [input]
  (= 1 (-> "3,9,8,9,10,9,4,9,99,-1,8" parse (run input))))

(defn equals-8-immediate-mode?
  [input]
  (= 1 (-> "3,3,1108,-1,8,3,4,3,99" parse (run input))))

(defn less-than-8?
  [input]
  (= 1 (-> "3,9,7,9,10,9,4,9,99,-1,8" parse (run input))))

(defn less-than-8-immediate-mode?
  [input]
  (= 1 (-> "3,3,1107,-1,8,3,4,3,99" parse (run input))))

(defn equals-0?
  [input]
  (= 0 (-> "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" parse (run input))))

(defn equals-0-immediate-mode?
  [input]
  (= 0 (-> "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" parse (run input))))

(defn relation-to-8
  [input]
  (case
      (-> (str "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
               "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,"
               "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
          parse (run input))
    999 "below 8"
    1001 "above 8"
    "8"))

(defn get-thermal-radiator-diagnostic-code
  []
  (-> program-path slurp parse (run 5)))
