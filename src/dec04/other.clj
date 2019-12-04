(ns dec04.other
  "Solutions from other folks are in this ns. Attributions given in comments."
  (:require [clojure.string :as str]))

;; Learning from taylorwood here.
;; See https://github.com/taylorwood/advent-of-code/blob/master/src/advent_of_code/2019/4.clj.

(defn digits [n]
  (loop [n n, ds ()]
    (let [d (mod n 10)]
      (if (pos? n)
        (recur (quot n 10) (cons d ds))
        ds))))

;; solve part one
(defn is-password? [n]
  (let [ds (digits n)]
    (and (apply <= ds)
         (some (partial apply =) (partition 2 1 ds)))))

;; solve part two
(defn is-password-too? [n]
  (let [ds (digits n)]
    (and (apply <= ds)
         (->> ds
              (partition-by identity)
              (map count)
              (some #(= 2 %))))))

;; From misha on Slack here.
(defn misha-solution
  [start end]
  (let [MIN      start
        MAX      end
        int-vec  (fn [n] (->> n str (map str) (mapv #(Integer/parseInt % 10))))
        low      (int-vec MIN)
        high     (int-vec MAX)
        too-low  #(neg? (compare % low))
        in-range #(neg? (compare % high))
        pwds     (for [a (range 0 10)
                      b (range a 10)
                      c (range b 10)
                      d (range c 10)
                      e (range d 10)
                      f (range e 10)
                      :let [n [a b c d e f]]
                      :when (->> n frequencies vals (some #{2}))]
                  n)]
    (->> pwds
        (drop-while too-low)
        (take-while in-range)
        (count))))

;; From dmarjenburgh
;; See https://gitlab.com/dmarjenburgh/adventofcode/blob/master/src/adventofcode/year_2019.clj#L65-78
(defn dmarjenburgh-solution
  [part input]
  (let [digits (fn [s] (mapv #(Character/digit ^Character % 10) s))
        [from to] (mapv digits (str/split input #"-"))
        filt (case part
               1 #(< (count (set %)) 6)
               #(contains? (set (vals (frequencies %))) 2))]
    (count (for [d1 (range 0 10)
                 d2 (range d1 10)
                 d3 (range d2 10)
                 d4 (range d3 10)
                 d5 (range d4 10)
                 d6 (range d5 10)
                 :let [d [d1 d2 d3 d4 d5 d6]]
                 :when (<= (compare from d) 0 (compare to d))
                 :when (filt d)]
             d))))

(defn incr
  [ds] ;; increase digit vector until it satisfies non-decreasing condition
         (let [d (peek ds)]
           (if (= d 9) ;; recursive
             (let [ds* (incr (pop ds))]
               (conj ds* (peek ds*))) ;; roll over using prev digit
             (conj (pop ds) (inc d)))))

(defn yuhan-day4
  [start end part]
  (let [dv     (fn [n] (mapv #(Character/getNumericValue %) (str n)))
        dv<    (fn [a b] (= -1 (compare a b)))
        startv (dv start)
        endv   (dv end)
        pw?    (fn [xs]
                 (some #((case part 1 <=, 2 =) 2 (count %))
                       (partition-by identity xs)))]
    (transduce
     (comp
      (drop-while #(dv< % startv))
      (take-while #(dv< % endv))
      (filter pw?))
     (completing (fn [n _] (inc n)))
     0
     (iterate incr (vec (repeat (count startv) (first startv)))))))
