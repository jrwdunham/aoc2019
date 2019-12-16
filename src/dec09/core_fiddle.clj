(ns dec09.core-fiddle
  (:require [dec09.core :as sut]))

(comment

  (let [program
        (sut/parse "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")]
    (= program (sut/run {:program program :debug? true})))

  (let [program
        (sut/parse "1102,34915192,34915192,7,4,7,99,0")
        result (sut/run {:program program})]
    (= 16 (-> result first str count)))

  (let [[_ expect _ :as program] (sut/parse "104,1125899906842624,99")
        [result] (sut/run {:program program})]
    (= expect result))

  (let [program (sut/parse "109,12,203,-5,204,-5,99")
        input 34]
    (= input (first (sut/run {:program program :debug? true} input))))

  ;; Verify correct answer for part one
  (= 3507134798
     (let [program (-> sut/input-path slurp sut/parse)]
       (first (sut/run {:program program} 1))))

  ;; Verify correct answer for part two
  (= 84513
     (let [program (-> sut/input-path slurp sut/parse)]
       (first (sut/run {:program program} 2))))

)

