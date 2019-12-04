(ns dec04.core)

(def my-start 240920)
(def my-end 789857)

(def pattern
  (re-pattern
   (str
    "("
    "(^|[^1])11($|[^1])"
    "|"
    "(^|[^2])22($|[^2])"
    "|"
    "(^|[^3])33($|[^3])"
    "|"
    "(^|[^4])44($|[^4])"
    "|"
    "(^|[^5])55($|[^5])"
    "|"
    "(^|[^6])66($|[^6])"
    "|"
    "(^|[^7])77($|[^7])"
    "|"
    "(^|[^8])88($|[^8])"
    "|"
    "(^|[^9])99($|[^9])"
    ")")))

(defn increasing?
  [digits]
  (->> digits
       seq
       (reduce (fn [prev digit]
                 (let [num (-> digit str read-string)]
                   (if prev
                     (if (<= prev num)
                       num
                       (reduced false))
                     num)))
               nil)
       boolean))

(defn get-possible-password-count
  [start end]
  (->> (range start (inc end))
       (map str)
       (filter (fn [d] (re-find #"(.)\1" d)))
       (filter increasing?)
       count))

(defn get-possible-password-count-part-two
  [start end]
  (->> (range start (inc end))
       (map str)
       (filter (fn [d] (re-find #"(.)\1" d)))
       (filter increasing?)
       (filter (fn [x] (re-find pattern x)))
       count))
