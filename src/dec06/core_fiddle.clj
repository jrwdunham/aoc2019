(ns dec06.core-fiddle
  (:require [dec06.core :as sut]
            [dec06.other :as oth]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

(comment

  ;; Verify Part One solution:
  (= 333679
     (-> sut/orbits-file-path
         slurp
         sut/analyze-orbits
         sut/count-orbits))

  (let [input (-> sut/orbits-file-path slurp)]
    (with-out-str
      (time
       (-> input
           sut/analyze-orbits
           sut/count-orbits))))

  (let [input (-> sut/orbits-file-path slurp)
        nodes (into {}
                    (map (fn [[p n]] [n p]))
                    (oth/parse-lines #(str/split % #"\)") input))
        nodes-to-com
        (fn [n] (set (take-while some? (rest (iterate nodes n)))))
        nodes-to-com-new
        (fn [n] (take-while some? (rest (iterate nodes n))))]
    #_(take-while some? (rest (iterate nodes "YOU")))
    #_(with-out-str
      (time
       (transduce (map (comp count nodes-to-com key)) + nodes)))
    (with-out-str
      (time (->> nodes
                 (map (comp count nodes-to-com key))
                 (reduce +))))
    )

  ;; Verify Part Two solution:
  (= 370
     (->> sut/orbits-file-path
          slurp
          sut/analyze-orbits
          (sut/get-minimum-orbital-transfers-between :o-YOU :o-SAN)))

  (= 42
     (->
      (str
       "COM)B\n"
       "B)C\n"
       "C)D\n"
       "D)E\n"
       "E)F\n"
       "B)G\n"
       "G)H\n"
       "D)I\n"
       "E)J\n"
       "J)K\n"
       "K)L\n")
      sut/analyze-orbits
      sut/count-orbits))

  (= 10
     (sut/count-orbits
      {:o-A :o-COM
       :o-X :o-COM
       :o-Y :o-X
       :o-Z :o-X
       :o-C :o-A
       :o-B :o-A}))

  (-> sut/orbits-file-path
      slurp
      sut/analyze-orbits)

  (->
   (str
    "COM)B\n"
    "B)C\n"
    "C)D\n"
    "D)E\n"
    "E)F\n"
    "B)G\n"
    "G)H\n"
    "D)I\n"
    "E)J\n"
    "J)K\n"
    "K)L\n"
    "K)YOU\n"
    "I)SAN\n")
   sut/analyze-orbits)

  (let [orbits
        (-> (str
             "COM)B\n"
             "B)C\n"
             "C)D\n"
             "D)E\n"
             "E)F\n"
             "B)G\n"
             "G)H\n"
             "D)I\n"
             "E)J\n"
             "J)K\n"
             "K)L\n"
             "K)YOU\n"
             "I)SAN\n")
            sut/analyze-orbits)]
    #_(sut/get-minimum-orbital-transfers-between :o-YOU :o-SAN orbits)
    (sut/get-all-orbitees :o-YOU orbits)
    )

  (->> sut/orbits-file-path
       slurp
       (oth/parse-lines #(str/split % #"\)")))

  (meta (with-meta {} {:a :yes}))

  (conj [1] 2)

  (conj (transient [1]) 2)

  (persistent! (reduce conj! (transient [1]) [2 3 4]))

  (= [1 2] [1 2])

  (= (transient [1 2]) [1 2])

  (into)

  (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))

  (reduce conj [-2 -1 0] [1 2 3])

  (conj [0] 1)

  (instance? clojure.lang.IEditableCollection [])

  (instance? clojure.lang.IEditableCollection (list 2 3))

  (conj!)

  (let [input (-> sut/orbits-file-path slurp)]
    (time
    (into {}
          (map (fn [[p n]] [n p]))
          (oth/parse-lines #(str/split % #"\)") input))))

  (let [input (-> sut/orbits-file-path slurp)]
    (time
     (->> (oth/parse-lines #(str/split % #"\)") input)
          (map (fn [[p n]] [n p]))
          (into {}))))

  (let [input (-> sut/orbits-file-path slurp)
        x (into {}
                (map (fn [[p n]] [n p]))
                (oth/parse-lines #(str/split % #"\)") input))
        y (->> (oth/parse-lines #(str/split % #"\)") input)
               (map (fn [[p n]] [n p]))
               (into {}))]
    (= x y))

  (let [input (-> sut/orbits-file-path slurp)
        nodes (into {}
                    (map (fn [[p n]] [n p]))
                    (oth/parse-lines #(str/split % #"\)") input))
        nodes-to-com (fn [n] (set (take-while some? (rest (iterate nodes n)))))
    ]
  (transduce (map (comp count nodes-to-com key)) + nodes)
    #_(take-while some? (rest (iterate nodes "YOU")))
    )

  ;; Elapsed time: 21.728852 msecs
  (with-out-str
    (time
     (->> (range 1000000)
          (r/map inc)
          (into [])
          count)))

  ;; Elapsed time: 65.73213 msecs
  (with-out-str
    (time
     (->> (range 1000000)
          (map inc)
          count)))

  ;; Elapsed time: 21.728852 msecs
  (with-out-str
    (time
     (->> (range 1000000)
          (r/map inc)
          (r/map #(* % %))
          (r/filter even?)
          (into [])
          count)))

  ;; Elapsed time: 84.799993 msecs
  (with-out-str
    (time
     (->> (range 1000000)
          (map inc)
          (map #(* % %))
          (filter even?)
          count)))

  (let [my-map (fn [f coll]
                 (reverse
                 (reduce (fn [x y] (conj x (f y)))
                         ()
                         coll)))]
    (apply =
    [(my-map inc (range 10))
     (my-map inc (vec (range 10)))
     (map inc (range 10))
     (map inc (vec (range 10)))]))

  (type (map inc))

  (((map inc) +) 2 3)

  (reduce ((map inc) +) 0 [1 2 3 4 5])

  (empty [])

  (conj [] 1)

  (let [square (fn [x] (* x x))]
    (->> (range 10)
         (map (comp square inc))
         #_(filter #(< % 10))
         ))

  (let [mapping (fn [f]
                  (fn [reducing]
                    (fn [result input]
                      (reducing result (f input)))))
        filtering (fn [predicate]
                    (fn [reducing]
                      (fn [result input]
                        (if (predicate input)
                          (reducing result input)
                          result))))
        square (fn [x] (* x x))
        xform (comp (filtering even?)
                        #_(filtering #(< % 10))
                        (mapping square)
                        (mapping inc))
        xform-bad (comp (mapping inc)
                    (mapping square)
                    (filtering #(< % 10))
                    (filtering even?))]
    #_(reduce (xform conj) [] (range 10))
    #_(((mapping square) conj) [] 2)
    (reduce ((mapping square) conj) [] [4 5 6])
    )

  (transduce (map inc) + 100 [1 2 3])

  (+ 9 9 9)

  (transduce)

  (str *ns*)

  (let [add-orbits (fn [planets]
                     (doseq [[p1 p2] (partition 2 planets)]
                       (derive p1 p2)))]
    (->> (-> sut/orbits-file-path
             slurp
             (str/replace #"\)" "\n")
             str/split-lines)
         (map (partial keyword (str *ns*)))
         add-orbits))

  (take 6 (iterate identity 2))

  (take 6 (repeat 2))

  (= (take 10 (iterate inc 0)) (range 10))

  cat

  (let [input (->> sut/orbits-file-path slurp)]
    (time
     (let [pairs (->> input
                      str/split-lines
                      (map (comp (fn [[x y]] [y x])
                                 #(str/split % #"\)")))
                      (into {}))
           greater-than (fn [n V]
                          (take-while identity (rest (iterate V n))))]
       (->> (set (apply concat pairs))
            (map #(count (greater-than % pairs)))
            (reduce +)))))

  (let [pairs (->>
               (str
                "COM)B\n"
                "B)C\n"
                "C)D\n"
                "D)E\n"
                "E)F\n"
                "B)G\n"
                "G)H\n"
                "D)I\n"
                "E)J\n"
                "J)K\n"
                "K)L\n")
               str/split-lines
               (map (comp (fn [[x y]] [y x])
                          #(str/split % #"\)")))
               (into {}))
        greater-than (fn [n V]
                       (take-while identity (rest (iterate V n))))]
    #_[pairs
     (->> (set (apply concat pairs)))
     (->> (set (apply concat pairs))
          (map #(count (greater-than % pairs))))]
    (->> (set (apply concat pairs))
         (map #(count (greater-than % pairs)))
         (reduce +))
    )



)
