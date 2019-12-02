(ns dec02.core-fiddle
  (:require [dec02.core :as sut]))

(comment

  (->> sut/programs-and-solutions
       (map (fn [[p s]] (= s (-> p sut/parse sut/run sut/serialize))))
       (every? identity))

  (= 11590668 (sut/solve-part-1 sut/program-path))

  (sut/solve-part-2 sut/program-path sut/part-2-target)

  (= 2254 (sut/solve-part-2 sut/program-path sut/part-2-target))

)
