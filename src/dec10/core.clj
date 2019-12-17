(ns dec10.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(def input-path "resources/input-dec10.txt")

(defn parse
  [input]
  (->> (str/split-lines input)
       (map-indexed (fn [y row]
                      (->> row
                           seq
                           (map-indexed (fn [x coord] [x y (= coord \#)])))))
       (apply concat)
       (filter (fn [[_ _ asteroid?]] asteroid?))
       (map (fn [[x y _]] [x y]))))

(defn get-angle
  [x y ox oy]
  (Math/atan2 (- y oy) (- ox x)))

(defn get-asteroid-lines-of-sight
  [x y asteroids]
  [x y (->> asteroids
            (filter (fn [oth] (not= oth [x y])))
            (map (fn [[ox oy]] (get-angle x y ox oy)))
            set
            count)])

(defn get-asteroids-lines-of-sight
  [input]
  (let [carte (parse input)]
    (->> carte
         (map (fn [[x y]] (get-asteroid-lines-of-sight x y carte))))))

(defn get-best-base
  [input]
  (->> input
       get-asteroids-lines-of-sight
       (sort-by last)
       last))

(defn abs
  [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn get-distance
  [cx cy x y]
  (+ (abs (- cx x))
     (abs (- cy y))))

(defn get-asteroids-sorted-by-angle
  [cx cy input]
  (let [lowest (Math/atan2 0 -1)
        primum (Math/atan2 1 0)]
    (->> (parse input)
         (filter (fn [oth] (not= oth [cx cy])))
         (map (fn [[x y]]
                {:coord [x y]
                 :distance (get-distance cx cy x y)
                 :angle
                 (let [rx (- x cx)
                       ry (- cy y)
                       angle (Math/atan2 ry rx)
                       angle-lowest (- angle primum)]
                   (if (> angle-lowest 0)
                     (- (* -2 lowest) (- primum angle-lowest))
                     angle-lowest))}))
         (sort-by (fn [{:keys [angle distance]}] [angle (- 0 distance)]))
         reverse)))

(defn get-vaporization-order
  [cx cy asteroids-sorted-by-angle]
  (let [increment (* 2 (Math/atan2 0 -1))]
    (->> asteroids-sorted-by-angle
         (group-by :angle)
         (reduce
          (fn [acc [angle asteroids]]
            (concat acc
                    (->> asteroids
                         (sort-by :distance)
                         (map-indexed
                          (fn [idx {:keys [angle] :as ast}]
                            (assoc ast :rank (- angle (* idx increment))))))))
          [])
         (sort-by :rank)
         reverse)))

(defn vaporize-does-not-work
  [input]
  (let [[x y _] (get-best-base input)
        asteroids-sorted-by-angle (get-asteroids-sorted-by-angle x y input)]
    (get-vaporization-order x y asteroids-sorted-by-angle)))

(defn recursive-vaporize
  [asteroids-sorted-by-angle]
  (let [{:keys [ordered leftover]}
        (reduce
         (fn [{:keys [last-angle ordered leftover] :as acc}
              {:keys [coord angle distance] :as asteroid}]
           (if (= angle last-angle)
             (-> acc (update :leftover conj asteroid))
             (-> acc
                 (update :ordered conj asteroid)
                 (assoc :last-angle angle))))
         {:ordered []
          :leftover []}
         asteroids-sorted-by-angle)]
    (if-not (seq leftover)
      ordered
      (concat ordered (recursive-vaporize leftover)))))

(defn vaporize
  [input]
  (let [[x y _] (get-best-base input)
        asteroids-sorted-by-angle (get-asteroids-sorted-by-angle x y input)]
    (recursive-vaporize asteroids-sorted-by-angle)))
