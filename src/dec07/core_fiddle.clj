(ns dec07.core-fiddle
  (:require [dec07.core :as sut]
            [dec05.core :as dec05]))

(comment

  ;; Verify part one
  (= 262086
     (-> sut/input-path
         slurp
         sut/parse
         sut/find-highest-signal))

  (let [input 0
        phase-setting 3
        initial-program
        (dec05/parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
        modified-program
        (dec05/run initial-program phase-setting 0 true)
        output
        (dec05/run modified-program input 0)
        ]
    output)

  (let [program
        (dec05/parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")]
    (sut/run-amplifiers program 4 3 2 1 0))

  (let [program
        (dec05/parse (str "3,23,3,24,1002,24,10,24,1002,23,-1,23,"
                          "101,5,23,23,1,24,23,23,4,23,99,0,0"))]
    (sut/run-amplifiers program 0 1 2 3 4))

  (let [program
        (dec05/parse (str "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,"
                          "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))]
    (sut/run-amplifiers program 1 0 4 3 2))

  (let [program
        (dec05/parse (str "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,"
                          "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))]
    (sut/find-highest-signal program))

  (let [program
        (sut/parse
         (str "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,"
              "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))]
    (sut/run-program-in-feedback-loop program [9 8 7 6 5] nil 0))

  (let [program
        (sut/parse
         (str "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,"
              "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,"
              "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))]
    (sut/run-program-in-feedback-loop program [9 7 8 5 6] nil 0))

  (= 5371621
     (-> sut/input-path
         slurp
         sut/parse
         sut/find-highest-feedback-loop-signal))

  (sut/get-permutations 5 9 5)

  (sut/get-permutations 1 6 3)

)
