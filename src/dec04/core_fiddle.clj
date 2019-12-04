(ns dec04.core-fiddle
  (:require [dec04.core :as sut]
            [dec04.other :as oth]))

(comment

  ;; correct answer for part one generated here: 1154
  ;; "Elapsed time: 485.806268 msecs"
  (-> (sut/get-possible-password-count sut/my-start sut/my-end)
      time)

  ;; taylorwood's performance is 4 times mine:
  ;; "Elapsed time: 114.604068 msecs"
  (-> (count (filter oth/is-password?
                     (range sut/my-start (inc sut/my-end))))
      time)

  ;; "Elapsed time: 3.415354 msecs"
  (-> (oth/dmarjenburgh-solution 1 "240920-789857") time)

  ;; correct answer for part two generated here: 750
  ;; "Elapsed time: 578.42914 msecs"
  (-> (sut/get-possible-password-count-part-two sut/my-start sut/my-end)
      time)

  ;; taylorwood's performance is 5 times mine:
  ;; "Elapsed time: 106.834341 msecs"
  (-> (count (filter oth/is-password-too?
                     (range sut/my-start (inc sut/my-end))))
      time)

  ;; misha, dmarjenburgh and yuhan's solutions are an order of magnitude faster:

  ;; "Elapsed time: 11.233194 msecs"
  (-> (oth/misha-solution sut/my-start sut/my-end)
      time)

  ;; "Elapsed time: 4.79278 msecs"
  (-> (oth/dmarjenburgh-solution 2 "240920-789857") time)

  ;; "Elapsed time: 2.483752 msecs"
  (time (oth/yuhan-day4 sut/my-start sut/my-end 2))

)
