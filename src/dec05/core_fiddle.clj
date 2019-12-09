(ns dec05.core-fiddle
  (:require [dec05.core :as sut]
            [dec05.other :as oth]
            [clojure.string :as str]))

(comment

  (oth)

  (-> sut/program-path
      slurp
      oth/solve1)

  ;; Correct answer to part one: 12896948
  (= 12896948 (sut/run-TEST))

  ;; Correct answer to part two: 7704130 
  (= 7704130 (sut/get-thermal-radiator-diagnostic-code))

  (sut/equals-8? 8)

  (sut/equals-8? 7)

  (sut/equals-8? 9)

  (sut/equals-8-immediate-mode? 8)

  (sut/equals-8-immediate-mode? 7)

  (sut/equals-8-immediate-mode? 9)

  (sut/less-than-8? 7)

  (sut/less-than-8? 8)

  (sut/less-than-8? 9)

  (sut/less-than-8-immediate-mode? 7)

  (sut/less-than-8-immediate-mode? 8)

  (sut/less-than-8-immediate-mode? 9)

  (sut/equals-0? -1)

  (sut/equals-0? 0)

  (sut/equals-0? 1)

  (sut/equals-0-immediate-mode? -1)

  (sut/equals-0-immediate-mode? 0)

  (sut/equals-0-immediate-mode? 1)

  (sut/relation-to-8 8)

  (sut/relation-to-8 7)

  (sut/relation-to-8 9)

  (format "%04d" 20091)

)
