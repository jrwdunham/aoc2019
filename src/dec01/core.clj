(ns dec01.core
  (:require [clojure.string :as str]))

(def module-masses-path "resources/input-dec01.txt")

(defn get-module-masses
  [module-masses-path]
  (->> module-masses-path
       slurp
       str/split-lines
       (map read-string)))

(defn neg->zero
  [x]
  (if (< x 0) 0 x))

(defn calculate-fuel-needed
  [mass]
  (-> mass
      (quot 3)
      (- 2)
      neg->zero))

(defn calculate-total-fuel-needed-for-module
  [mass]
  (let [fuel-needed (calculate-fuel-needed mass)]
    (if (> fuel-needed 0)
      (+ fuel-needed
         (calculate-total-fuel-needed-for-module fuel-needed))
      fuel-needed)))

(defn calculate-total-fuel-needed-for-modules
  [module-masses]
  (->> module-masses
       (map calculate-total-fuel-needed-for-module)
       (reduce +)))

