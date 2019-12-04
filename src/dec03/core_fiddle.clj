(ns dec03.core-fiddle
  (:require [dec03.core :as sut]))

(comment

  ;; Confirm the algorithm works for part one for the example cases.
  (->> sut/puzzles
       (map (fn [{:keys [wire-1 wire-2 distance]}]
              (= distance (sut/get-closest-intersection wire-1 wire-2))))
       (every? identity))

  (peek [1 2 3])

  (last [1 2 3])

  ;; Confirm the algorithm works for part two for the example cases.
  (->> sut/puzzles
       (map (fn [{:keys [wire-1 wire-2 steps]}]
               (= steps (sut/get-closest-intersection-by-steps wire-1 wire-2))))
       (every? identity))

  ;; The correct answer for part one for my input---viz. 1626---should be
  ;; returned when this is executed.
  (apply sut/get-closest-intersection (sut/parse-wires-file sut/wires-path))

  ;; The correct answer for part two for my input---viz. 27330---should be
  ;; returned when this is executed.
  (apply sut/get-closest-intersection-by-steps
         (sut/parse-wires-file sut/wires-path))

)

