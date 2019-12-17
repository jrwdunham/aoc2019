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

  ;; Should return true to show test for part two passes
  (let [vaporization-order (sut/vaporize test-input-5)
        expectations
        [[[11 12] 1]
         [[12 1] 2]
         [[12 2] 3]
         [[12 8] 10]
         [[16 0] 20]
         [[16 9] 50]
         [[9 6] 199]
         [[8 2] 200]
         [[10 9] 201]
         [[11 1] 299]]]
    (->> expectations
         (map (fn [[coord index]]
                (= coord
                   (-> vaporization-order (nth (dec index)) :coord))))
         (every? identity)))

  ;; Verify correct answer to part two
  (let [vaporization-audit
        (-> sut/input-path
            slurp
            sut/vaporize)
        [hundreds ones]
        (-> vaporization-audit (nth 199) :coord)]
    (= 1919 (+ (* hundreds 100) ones)))

)

