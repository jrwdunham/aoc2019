(ns dec06.core
  (:require [clojure.string :as str]))

(def orbits-file-path "resources/input-dec06.txt")

(defn get-orbiters-of
  "Return the seq of satellites in `orbits` that orbit `orbitee`.
  Given
    :o-COM {:o-A :o-COM :o-X :o-COM :o-B :o-A}
  return
    (:o-A :o-X)."
  [orbitee orbits]
  (->> orbits
       (filter (fn [[_ other-orbitee]] (= orbitee other-orbitee)))
       (map first)))

(defn keywordize-satellite
  [satellite]
  (->> satellite (str "o-") keyword))

(defn analyze-orbits
  "Given a string of orbit lines, return a map from orbiters to the single
  other satellite that they immediately orbit.
  Given:
    COM)A
    COM)X
    B)A
  return:
    {:o-A :o-COM :o-X :o-COM :o-B :o-A ...}"
  [orbits-string]
  (->> (-> orbits-string str/split-lines)
       (map (fn [row]
              (->> (str/split row #"\)")
                   (map keywordize-satellite)
                   reverse
                   (apply hash-map))))
       (apply merge)))

(defn count-orbits
  "Count the total number of orbits encoded in the tree structure represented by
  map `orbits`. Start at the root node 'COM' which is at depth 1. Recursive.
  Example orbits: {:o-A :o-COM :o-X :o-COM :o-B :o-A ...} can be visualized
  as:
    COM----A--C
     |     |
     X--Y  B
     |
     Z
  The above contains 10 orbits: A and X orbit COM, Y orbits X and COM, etc."
  ([orbits] (count-orbits orbits 1 :o-COM))
  ([orbits depth node]
   (let [orbiters (get-orbiters-of node orbits)]  ;; (:o-A :o-X)
     (if (seq orbiters)
       (+ (* depth (count orbiters))
          (->> orbiters
               (map (fn [orbiter] (count-orbits orbits (inc depth) orbiter)))
               (reduce +)))
       0))))

(defn get-all-orbitees
  "Return an ordered sequence of all of the satellites that `orbiter` orbits
  according to the map of orbits `orbits`. For the Part Two example, the return
  value for YOU would be (K J E D C B COM)."
  [orbiter orbits]
  (if-let [orbitee (orbiter orbits)]
    (concat [orbitee] (get-all-orbitees orbitee orbits))
    []))

(defn get-orbitees-not-in
  "Return the prefix of `orbitees` where no elements of that prefix are in
  `not-in`."
  [orbitees not-in]
  (take-while (fn [orbitee] (not (some #{orbitee} not-in))) orbitees))

(defn get-minimum-orbital-transfers-between
  "Return the minimum number of orbital transfers required to move from satellite
  `s1` to satellite `s2` along the orbits defined in the map `orbits`."
  [s1 s2 orbits]
  (let [orbitees-1 (get-all-orbitees s1 orbits)
        orbitees-2 (get-all-orbitees s2 orbits)]
    (->> [(get-orbitees-not-in orbitees-1 orbitees-2)
          (get-orbitees-not-in orbitees-2 orbitees-1)]
         (map count)
         (reduce +))))
