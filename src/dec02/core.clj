(ns dec02.core
  (:require [clojure.string :as str]))

(def programs-and-solutions
  {"1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"
   "1,0,0,0,99" "2,0,0,0,99"
   "2,3,0,3,99" "2,3,0,6,99"
   "2,4,4,5,99,0" "2,4,4,5,99,9801"
   "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"})

(def program-path "resources/input-dec02.txt")
(def part-2-target 19690720)
(def noun-pos 1)
(def verb-pos 2)

(defn pre-process
  "Before running the program, replace the noun position (1) with `noun-val` (12)
  and replace verb position (2) with `verb-val` (2)."
  ([program] (pre-process program 12 2))
  ([program noun-val verb-val]
   (-> program
       (assoc noun-pos noun-val)
       (assoc verb-pos verb-val))))

(defn parse
  "Parse an input program string to a vector of integers."
  [input]
  (->> (str/split input #",")
       (map read-string)
       vec))

(defn serialize
  "Serialize program, a vec of ints, to a string of comma-delimited digits."
  [program]
  (str/join \, (map str program)))

(defn run
  "Run `program` until it halts and return the updated program."
  ([program] (run program 0))
  ([program cur-pos]
   (let [[op i1-pos i2-pos o-pos] (drop cur-pos program)
         i1 (get program i1-pos)
         i2 (get program i2-pos)]
     (case op
       99 program
       (run (assoc program o-pos ((case op 1 + *) i1 i2)) (+ cur-pos 4))))))

(defn solve-part-1
  "This should generate the correct answer to
  'What value is left at position 0 after the program halts?' For my input, this
  is 11590668."
  [path]
  (-> path
      slurp
      parse
      pre-process
      run
      (get 0)))

(defn solve-part-2
  "Find the input noun and verb that cause the program at `path` to produce the
  output `target` Return `100 * noun + verb`. (For example, if noun=12 and
  verb=2, the answer would be 1202."
  [path target]
  (let [program (-> path slurp parse)
        [noun verb]
        (->> (for [noun (range 100) verb (range 100)] [noun verb])
             (map (fn [[noun verb]]
                    [(-> (pre-process program noun verb) run (get 0))
                     noun verb]))
             (filter (fn [[output _ _]] (= target output)))
             first
             rest)]
    (+ (* 100 noun) verb)))

