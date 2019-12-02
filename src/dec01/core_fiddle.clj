(ns dec01.core-fiddle
  (:require [dec01.core :as sut]))

(comment

  (sut/calculate-fuel-needed 12)

  (sut/calculate-fuel-needed 14)

  (sut/calculate-fuel-needed 1969)

  (sut/calculate-fuel-needed 100756)

  (sut/get-module-masses sut/module-masses-path)

  (-> sut/module-masses-path
      sut/get-module-masses
      sut/calculate-fuel-needed-for-modules)

  (-> [12 14 1969]
      sut/calculate-fuel-needed-for-modules)

  (sut/calculate-total-fuel-needed-for-modules [14])

  (sut/calculate-total-fuel-needed-for-modules [1969])

  (sut/calculate-total-fuel-needed-for-modules [100756])

  (-> sut/module-masses-path
      sut/get-module-masses
      sut/calculate-fuel-needed-for-modules)  ;; 3481005

  (+ (sut/calculate-total-fuel-needed-for-module 14)
     (sut/calculate-total-fuel-needed-for-module 1969)
     (sut/calculate-total-fuel-needed-for-module 100756))  ;; 51314 CORRECT

  (sut/calculate-total-fuel-needed-for-modules [14 1969 100756])  ;; 51314

  (-> sut/module-masses-path
      sut/get-module-masses
      sut/calculate-total-fuel-needed-for-modules)  ;; 5218616 CORRECT

)

