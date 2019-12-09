(ns dec09.core-fiddle
  (:require [dec09.core :as sut]))

(comment

  (format "%04d" 4)

  (-> (str "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,"
           "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
      sut/parse
      (sut/run-halting [0 1]))

  (let [f
        (-> "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
            sut/parse
            (sut/run-halting []))]
    f
    )

)

