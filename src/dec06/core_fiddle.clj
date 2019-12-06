(ns dec06.core-fiddle
  (:require [dec06.core :as sut]
            [clojure.string :as str]))

(comment

  ;; Verify Part One solution:
  (= 333679
     (-> sut/orbits-file-path
         slurp
         sut/analyze-orbits
         sut/count-orbits))

  ;; Verify Part Two solution:
  (= 370
     (->> sut/orbits-file-path
          slurp
          sut/analyze-orbits
          (sut/get-minimum-orbital-transfers-between :o-YOU :o-SAN)))

  (= 42
     (->
      (str
       "COM)B\n"
       "B)C\n"
       "C)D\n"
       "D)E\n"
       "E)F\n"
       "B)G\n"
       "G)H\n"
       "D)I\n"
       "E)J\n"
       "J)K\n"
       "K)L\n")
      sut/analyze-orbits
      sut/count-orbits))

  (= 10
     (sut/count-orbits
      {:o-A :o-COM
       :o-X :o-COM
       :o-Y :o-X
       :o-Z :o-X
       :o-C :o-A
       :o-B :o-A}))

  (-> sut/orbits-file-path
      slurp
      sut/analyze-orbits)

  (->
   (str
    "COM)B\n"
    "B)C\n"
    "C)D\n"
    "D)E\n"
    "E)F\n"
    "B)G\n"
    "G)H\n"
    "D)I\n"
    "E)J\n"
    "J)K\n"
    "K)L\n"
    "K)YOU\n"
    "I)SAN\n")
   sut/analyze-orbits)

  (let [orbits
        (-> (str
             "COM)B\n"
             "B)C\n"
             "C)D\n"
             "D)E\n"
             "E)F\n"
             "B)G\n"
             "G)H\n"
             "D)I\n"
             "E)J\n"
             "J)K\n"
             "K)L\n"
             "K)YOU\n"
             "I)SAN\n")
            sut/analyze-orbits)]
    #_(sut/get-minimum-orbital-transfers-between :o-YOU :o-SAN orbits)
    (sut/get-all-orbitees :o-YOU orbits)
    )

)
