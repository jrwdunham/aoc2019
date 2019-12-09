(ns dec08.core
  (:require [clojure.string :as str]))

(def image-file-path "resources/input-dec08.txt")

(defn get-image
  "Return image as a string of digits (0-2)."
  []
  (-> image-file-path slurp str/trim))

(defn verify-image-data
  "Given a string of digits `image` encoding multiple image layers of dimensions
  `width` and `height`, return the product of the count of 1 and 2 digits in the
  layer with the fewest 0 digits."
  [image width height]
  (->> [\1 \2]
       (map (fn [k]
              (get (->> (partition (* width height) image)
                        (map frequencies)
                        (sort-by #(get % \0))
                        first)
                   k)))
       (apply *)))

(defn reduce-layers
  "Given a string of digits `image` encoding multiple image layers of dimensions
  `width` and `height`, return a single layer, a seq of characters whose length
  is the product of `width` and `height`."
  [image width height]
  (->> (partition (* width height) image)
       (apply map (fn [& pixels] (->> pixels
                                      (filter #(not= \2 %))
                                      first)))))

(defn draw-image
  "Reduce the layers encoded in `image` and write it to stdout so it can be
  viewed."
  [image width height]
  (doseq [row (->> (reduce-layers image width height)
                   (map #(case % \0 \space %))
                   (partition width))]
    (println (apply str row))))
