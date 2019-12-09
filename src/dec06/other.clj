(ns dec06.other
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; From dmarjenburgh
;; https://gitlab.com/dmarjenburgh/adventofcode/blob/master/src/adventofcode/year_2019.clj#L123-129

(defn parse-lines
  ([input] (parse-lines identity input))
  ([f input] (mapv f (str/split-lines input))))

(defn day6 [part input]
  (let [nodes (into {}
                    (map (fn [[p n]] [n p]))
                    (parse-lines #(str/split % #"\)") input))
        nodes-to-com (fn [n] (set (take-while some? (rest (iterate nodes n)))))]
    (case part
      1 (transduce (map (comp count nodes-to-com key)) + nodes)
      2 (let [[p1 p2] (map nodes-to-com ["YOU" "SAN"])]
          (count (set/difference (set/union p1 p2) (set/intersection p1 p2)))))))
