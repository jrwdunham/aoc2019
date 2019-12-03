(ns dec03.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def wires-path "resources/input-dec03.txt")

(defn parse-wires-file
  [path]
  (-> path
      slurp
      str/split-lines))

(def puzzles
  [{:wire-1 "R8,U5,L5,D3"
    :wire-2 "U7,R6,D4,L4"
    :distance 6
    :steps 30}
   {:wire-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    :wire-2 "U62,R66,U55,R34,D71,R55,D58,R83"
    :distance 159
    :steps 610}
   {:wire-1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    :wire-2 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    :distance 135
    :steps 410}])

(defn get-all-points
  "Return all points, [x y] vectors, entailed by `path`, which is a
  comma-delimited set of movements, e.g., U12,L4,D2,R77. Note that this can be
  quite inefficient: for my input there were 153,052 point coordinates for the
  first path."
  [path]
  (->> (-> path (str/split #","))
       (map (fn [[direction & distance]]
              [(-> direction str/lower-case keyword)
               (->> distance (apply str) read-string)]))
       (reduce (fn [acc [direction distance]]
                 (let [[start-x start-y] (last acc)
                       new-coords
                       (apply map
                              vector
                              (case direction
                                :u [(repeat start-x)
                                    (range (inc start-y) (+ start-y distance 1))]
                                :d [(repeat start-x)
                                    (reverse (range (- start-y distance) start-y))]
                                :r [(range (inc start-x) (+ start-x distance 1))
                                    (repeat start-y)]
                                :l [(reverse (range (- start-x distance) start-x))
                                    (repeat start-y)]))]
                   (concat acc new-coords)))
               [[0 0]])))

(defn get-full-path-intersections
  "Return all intersections between al full paths in `paths`. A full path is a
  sequence of coordinates (i.e., 2-ary vectors)."
  [& paths]
  (->> paths
       (map set)
       (apply set/intersection)
       (filter #(not= % [0 0]))))

(defn get-path-intersections
  "Return the set of coordinates (2-ary vectors) where the paths in `paths`
  intersect. The paths are strings containing comma-delimited movement
  directions, e.g., U8,L3, which would mean 'start at [0 0], move up 8, then move
  left 3."
  [& paths]
  #_(->> paths
       (map (comp set get-all-points))
       (apply set/intersection))
  (apply get-full-path-intersections (map get-all-points paths)))

(defn abs
  [x]
  (if (< x 0) (* -1 x) x))

(defn get-closest-intersection
  "Calculate the Manattan distance from [0 0] for each intersection of `paths`
  and return the shortest distance (not including the 0 distance)."
  [& paths]
  (->> (apply get-path-intersections paths)
       (map #(apply + (map abs %)))
       sort
       first))

(defn get-targets-to-steps-required
  "Return a map from each target vec (2-vec coordinate) in `targets` to the
  number of steps along `path` that are required in order to reach that target.

  Note: this implementation accumulates an unnecessary :history value. The
  purpose of this was to allow for the removal of loops within a wire (i.e., a
  path). I was mistakenly under the impression that such loops needed to be
  removed because of this sentence in the instructions:

      'If a wire visits a position on the grid multiple times, use the steps
      value from the first time it visits that position when calculating the
      total value of a specific intersection.'

  While I understand that my interpretation was mistaken, I have left this overly
  complex implementation intact in case this strategy is needed in future steps."
  [path targets]
  (reduce (fn [{:keys [step-count targets #_history] :as acc} step]
            (let [new-step-count step-count
                  #_#_new-step-count (get history step step-count)
                  new-acc (-> acc
                              (assoc :step-count (inc new-step-count))
                              (assoc-in [:history step] new-step-count))
                  {new-targets :targets result :targets->steps :as new-acc}
                  (if (some #{step} targets)
                    (-> new-acc
                        (assoc-in [:targets->steps step] new-step-count)
                        (assoc :targets (filter #(not= % step) targets)))
                    new-acc)]
              (if (seq new-targets)
                new-acc
                (reduced result))))
          {:targets targets
           :targets->steps {}
           :history {}
           :step-count 0}
          path))

(defn get-closest-intersection-by-steps
  "Return the shortest combined path from [0 0] to an intersection between two
  wires following paths `path-1` and `path-2`."
  [path-1 path-2]
  (let [full-path-1 (get-all-points path-1)
        full-path-2 (get-all-points path-2)
        intersections (get-full-path-intersections full-path-1 full-path-2)
        path-1-targets->step-counts
        (get-targets-to-steps-required full-path-1 intersections)
        path-2-targets->step-counts
        (get-targets-to-steps-required full-path-2 intersections)]
    (->> intersections
         (map (fn [intersection]
                (+ (path-1-targets->step-counts intersection)
                   (path-2-targets->step-counts intersection))))
         sort
         first)))
