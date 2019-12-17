(ns dec10.core-fiddle
  (:require [dec10.core :as sut]))

(def test-input-1
  (str ".#..#\n"
       ".....\n"
       "#####\n"
       "....#\n"
       "...##\n"))

(def test-input-2
  (str
   "......#.#.\n"
   "#..#.#....\n"
   "..#######.\n"
   ".#.#.###..\n"
   ".#..#.....\n"
   "..#....#.#\n"
   "#..#....#.\n"
   ".##.#..###\n"
   "##...#..#.\n"
   ".#....####\n"))

(def test-input-3
  (str
   "#.#...#.#.\n"
   ".###....#.\n"
   ".#....#...\n"
   "##.#.#.#.#\n"
   "....#.#.#.\n"
   ".##..###.#\n"
   "..#...##..\n"
   "..##....##\n"
   "......#...\n"
   ".####.###.\n"))

(def test-input-4
  (str
   ".#..#..###\n"
   "####.###.#\n"
   "....###.#.\n"
   "..###.##.#\n"
   "##.##.#.#.\n"
   "....###..#\n"
   "..#.#..#.#\n"
   "#..#.#.###\n"
   ".##...##.#\n"
   ".....#.#..\n"))

(def test-input-5
  (str
   ".#..##.###...#######\n"
   "##.############..##.\n"
   ".#.######.########.#\n"
   ".###.#######.####.#.\n"
   "#####.##.#.##.###.##\n"
   "..#####..#.#########\n"
   "####################\n"
   "#.####....###.#.#.##\n"
   "##.#################\n"
   "#####.##.###..####..\n"
   "..######..##.#######\n"
   "####.##.####...##..#\n"
   ".#####..#.######.###\n"
   "##...#.##########...\n"
   "#.##########.#######\n"
   ".####.#.###.###.#.##\n"
   "....##.##.###..#####\n"
   ".#.#.###########.###\n"
   "#.#.#.#####.####.###\n"
   "###.##.####.##.#..##\n"))

(def test-input-6
  (str
   "..........#.#.......\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "....................\n"
   "..........###.......\n"
   "..........###.......\n"
   "..........###.......\n"
   "....................\n"
   "....................\n"))

(def test-input-7
  (str
   ".#....#####...#..\n"
   "##...##.#####..##\n"
   "##...#...#.#####.\n"
   "..#.....#...###..\n"
   "..#.#.....#....##\n"))

(comment

  (sut/get-asteroids-lines-of-sight test-input-1)

  (sut/get-best-base test-input-1)

  (= [3 4 8] (sut/get-best-base test-input-1))

  (= [5 8 33] (sut/get-best-base test-input-2))

  (= [1 2 35] (sut/get-best-base test-input-3))

  (= [6 3 41] (sut/get-best-base test-input-4))

  (= [11 13 210] (sut/get-best-base test-input-5))

  ;; Verify correct answer to part one
  (= 247
     (-> sut/input-path
         slurp
         sut/get-best-base
         last))

  (nth (list 1 2 33 44) 3)

  (let [vaporization-order (sut/vaporize test-input-5)
        expectations
        [[[11 12] 1]
         [[12 1] 2]
         [[12 2] 3]
         [[12 8] 10]
         [[16 0] 20]
         [[16 9] 50]
         ;; [[9 6] 199]  ;; failing
         ;; [[8 2] 200]  ;; failing
         ;; [[10 9] 201]  ;; failing
         [[11 1] 299]
         ]]
    (->> expectations
         (map (fn [[coord index]]
                (= coord
                   (-> vaporization-order (nth (dec index)) :coord))))
         (every? identity)))

  (sut/vaporize test-input-6)

  (sut/vaporize test-input-7)

  (sut/get-distance 11 13 9 15)

  (Math/atan2 0 -1)

  (Math/atan2 1 0)

  (Math/atan2 -1 -1)

  ;;  2.4    1.5    0.8
  ;;  3.1    0      0.0
  ;; -2.4   -1.5   -0.8

  ;;  1 -1   1  0   1 1
  ;;  0 -1   0  0   0 1
  ;; -1 -1  -1  0  -1 1




  ;; -1  1   0  1   1  1
  ;; -1  0   0  0   1  0
  ;; -1 -1   0 -1   1 -1

  ;;  2.3     1.5    0.8
  ;;  3.4    0       0.0
  ;; -2.4    -1.5   -0.8

  (let [offset (Math/atan2 0 -1)]
    (->> (for [x (range -1 2)
               y (range -1 2)
               :when (not= [x y] [0 0])]
           [[x y]
            #_(let [tmp (+ offset (Math/atan2 y x))]
                (if (>= tmp offset) tmp (+ tmp (* 2 offset))))
            (Math/atan2 y x)
            ])
         (sort-by second)))

  ;;       10,0
  ;;    10,12 11,12 12,12
  ;;    10,13 11,13 12,13
  ;;    10,14 11,14 12,14

  ;;    -1,1   0,1   1,1
  ;;    -1,0   0,0   1,0
  ;;    -1,-1  0,-1  1,-1

  ;;  2.3     1.5    0.8
  ;;  3.4    0       0.0
  ;; -2.4    -1.5   -0.8

  (Math/atan2 1 0)

  (let [[cx cy] [11 13]
        lowest (Math/atan2 0 -1)
        primum (Math/atan2 1 0)]
    (->>
     (concat [[10 0] [12 0]]
             (for [x (range 10 13)
                   y (range 12 15)
                   :when (not= [x y] [cx cy])]
               [x y]))
     (map (fn [[x y]]
            (let [rx (- x cx)
                  ry (- cy y)
                  angle (Math/atan2 ry rx)
                  angle-lowest (- angle primum)]
              [[x y]
               #_angle-lowest
               #_angle
               (if (> angle-lowest 0)
                 (- (* -2 lowest) (- primum angle-lowest))
                 angle-lowest)])))
     (sort-by second)
     #_reverse))

  #_([[10 14] -2.356194490192345]
   [[11 14] -1.5707963267948966]
   [[12 14] -0.7853981633974483]
   [[12 13] 0.0]
   [[12 12] 0.7853981633974483]
   [[11 12] 1.5707963267948966]
   [[10 12] 2.356194490192345]
   [[10 13] 3.141592653589793])

  (let [offset (Math/atan2 0 -1)]
    (->> (for [x (range -1 2)
               y (range -1 2)
               :when (not= [x y] [0 0])]
           [[x y] (let [tmp (+ offset (Math/atan2 y x))]
                    (if (>= tmp offset) tmp (+ tmp (* 2 offset))))])
         (sort-by second)))

  (Math/atan2 -0.001 -1)

  (Math/atan2 0 -1)

  (Math/atan2 1 0)

  ;; Relative coord of [11 13] is [0 1]
  ;; Unmodified angle of [11 13] is 1.5707963267948966
  ;; Modified angle of [11 13] is 4.71238898038469
  ;; Ret for [11 13] is 4.71238898038469
  ;; Relative coord of [11 13] is [1 0]
  ;; Unmodified angle of [11 13] is 0.0
  ;; Modified angle of [11 13] is 3.141592653589793
  ;; Ret for [11 13] is 3.141592653589793

  (rem 8 7)

  (let [offset (Math/atan2 1 0)
        x [[:first (Math/atan2 1 0)]
           [:second (Math/atan2 0 1)]
           [:third (Math/atan2 -1 0)]
           [:fourth (Math/atan2 0 -1)]]]
    [x (mapv (fn [[ord v]] [ord (* -1 (- v offset))]) x)])

)

