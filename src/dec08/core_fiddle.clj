(ns dec08.core-fiddle
  (:require [dec08.core :as sut]))

(comment

  (= 1560 (sut/verify-image-data (sut/get-image) 25 6))

  (= (str "1  1  11   11  1  1 1  1 \n"
          "1  1 1  1 1  1 1  1 1  1 \n"
          "1  1 1    1    1  1 1111 \n"
          "1  1 1 11 1    1  1 1  1 \n"
          "1  1 1  1 1  1 1  1 1  1 \n"
          " 11   111  11   11  1  1 \n")
     (with-out-str (sut/draw-image (sut/get-image) 25 6)))

)


