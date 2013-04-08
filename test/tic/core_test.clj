(ns tic.core-test
  (:use clojure.test
        clojure.set
        tic.tic))

(def gm1 [["X" "O" "X"]
          ["_" "O" "_"]
          ["_" "_" "_"]])

(def gm3 [["X" "X" "_"]
          ["_" "O" "_"]
          ["O" "_" "_"]])

(def gm4 [["X" "_" "O"]
          ["_" "O" "_"]
          ["X" "_" "_"]])

(def gm6 [["O" "_" "X"]
          ["O" "_" "_"]
          ["X" "_" "_"]])

(def gm7 [["O" "_" "X"]
          ["_" "_" "_"]
          ["X" "_" "O"]])

(def gm8 [["O" "_" "_"]
          ["X" "O" "X"]
          ["_" "_" "_"]])

(def tm1 [["_" "_" "_"]
          ["_" "_" "_"]
          ["_" "_" "_"]])

(deftest test-board-matched
  (is (= (get-board-two-matches gm4)
         '({:tile "X", :loc [[0 0] [2 0]]}))))

; our first move is corner
(deftest test-first-move-1
  (is (= (first-move {:board tm1 :my-tile "X" :my-turn "First"})
         '[["X" "_" "_"] ["_" "_" "_"] ["_" "_" "_"]])))

; test that when out turn is second, our first move is corner
; if op takes center
(deftest test-first-move-2
  (let [board (assoc-in tm1 [1 1] "X")]
    (is (= (first-move {:board board :my-tile "O" :my-turn "Second"})
           '[["O" "_" "_"] ["_" "X" "_"] ["_" "_" "_"]]))))

; if op takes corner, we take center
(deftest test-first-move-3
  (let [board (assoc-in tm1 [0 0] "X")]
    (is (= (first-move {:board board :my-tile "O" :my-turn "Second"})
           '[["X" "_" "_"] ["_" "O" "_"] ["_" "_" "_"]]))))

; test played cornes
(deftest test-played-corners-1
  (is (= (get-played-corners gm4 "X")
         '([0 0] [2 0]))))

(deftest test-played-corners-2
  (is (= (get-played-corners tm1 "X")
         '())))

(deftest test-played-corners-3
  (is (= (get-played-corners gm4 "O")
         '([0 2]))))

(deftest test-avail-corners1
  (is (= (get-available-corners gm4)
         '([2 2]))))

(def tm2 [["X" "_" "_"]
          ["_" "O" "_"]
          ["_" "_" "_"]])

(deftest test-am-i-main-diag?
  (is (= (first (clojure.set/intersection (into #{} (get-played-corners tm2 "X"))
                                          (into #{} (main-diagonal tm2 3))))
         [0 0])))

(deftest test-center1
  (is (= "O" (get-center gm4))))

; test available sides
(deftest test-avail-sides1
  (is (= (get-available-sides gm6)
         '([0 1] [1 2] [2 1]))))

(deftest test-avail-sides2
  (is (= (get-available-sides gm4)
         '([0 1] [1 0] [1 2] [2 1]))))

(def gm2 [["X" "_" "O"]
          ["_" "_" "_"]
          ["_" "_" "_"]])

(def tm3 [["X" "_" "_"]
          ["_" "O" "_"]
          ["_" "_" "_"]])

(def tm4 [["X" "_" "_"]
          ["_" "O" "_"]
          ["_" "_" "X"]])

(deftest test-next-move1
  (is (= (choose-next-move {:board gm2 :my-tile "X" :my-turn "First"})
         [2 2])))

(deftest test-next-move2
  (is (= (choose-next-move {:board gm2 :my-tile "O" :my-turn "First"})
         [2 0])))

; pick opposite corner
(deftest test-next-move3
  (is (= (choose-next-move {:board tm3 :my-tile "X" :my-turn "First"})
         [2 2])))

; make sure we block the fork
(deftest test-next-move4
  (is (= (choose-next-move {:board tm4 :my-tile "O" :my-turn "Second"})
         [0 1])))

; X has 2 ways to win, same as O
(def gm5 [["X" "X" "_"]
          ["X" "_" "O"]
          ["_" "O" "O"]])

; test metadata
(deftest test-match-metadata1
  (is (= (map #(meta %) ((group-player-moves 
                           (get-board-two-matches gm5)) "X"))
         '({:Colm true} {:Row true}))))

(def gm9 [["O" "_" "X"]
          ["X" "O" "X"]
          ["X" "_" "_"]])

(deftest test-match-metadata2
  (is (= (map #(meta %) ((group-player-moves 
                           (get-diag-two-matches gm9)) "O"))
         '({:Main true}))))

(deftest test-match-metadata3
  (is (= (concat (map #(meta %) ((group-player-moves 
                           (get-diag-two-matches gm9)) "O"))
                  (map #(meta %) ((group-player-moves 
                           (get-board-two-matches gm5)) "X")))
         '({:Main true} {:Colm true} {:Row true}))))

