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

(def tm1 [["_" "_" "_"]
          ["_" "_" "_"]
          ["_" "_" "_"]])

(def tm7 [["O" "O" "O"]
          ["O" "X" "X"]
          ["O" "X" "O"]])

(deftest test-board-matched
  (is (= (get-board-two-matches gm4)
         '({:tile "X", :loc [[0 0] [2 0]]}))))

; our first move is corner
(deftest test-first-move-1
  (is (= (first-move {:board tm1 :my-tile "X" :my-turn "First"})
         '{:board [["X" "_" "_"] ["_" "_" "_"] ["_" "_" "_"]], :my-tile "X", :my-turn "First"})))

; test that when out turn is second, our first move is corner
; if op takes center
(deftest test-first-move-2
  (let [board (assoc-in tm1 [1 1] "X")]
    (is (= (first-move {:board board :my-tile "O" :my-turn "Second"})
         '{:board [["O" "_" "_"] ["_" "X" "_"] ["_" "_" "_"]], :my-tile "O", :my-turn "Second"}))))

; if op takes corner, we take center
(deftest test-first-move-3
  (let [board (assoc-in tm1 [0 0] "X")]
    (is (= (first-move {:board board :my-tile "O" :my-turn "Second"})
          '{:board [["X" "_" "_"] ["_" "O" "_"] ["_" "_" "_"]], :my-tile "O", :my-turn "Second"}))))

(def tm11 [["X" "_" "_"]
           ["_" "_" "_"]
           ["_" "_" "_"]])

(deftest test-next-move-4
  (let [board (assoc-in tm11 [2 2] "O")]
    (is (= (next-move {:board board :my-tile "X" :my-turn "First"})
          '{:board [["X" "_" "X"] ["_" "_" "_"] ["_" "_" "O"]], :my-tile "X", :my-turn "First"}))))

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

(def tm6 [["X" "O" "O"]
          ["O" "X" "X"]
          ["O" "X" "O"]])

; what move when board is full ?
(deftest test-next-move5
  (is (= (choose-next-move {:board tm6 :my-tile "O" :my-turn "Second"})
         nil)))

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

; test rows
(deftest test-win1
  (is (= (next-move {:board gm3 :my-tile "X" :my-turn "First"})
         {:board [["X" "X" "X"] ["_" "O" "_"] ["O" "_" "_"]]
          :my-tile "X", :my-turn "First"})))

; test columns
(deftest test-win2
  (is (= (next-move {:board gm4 :my-tile "X" :my-turn "First"})
         {:board [["X" "_" "O"] ["X" "O" "_"] ["X" "_" "_"]]
          :my-tile "X", :my-turn "First"})))

; test major diag
(deftest test-win3
  (is (= (next-move {:board gm9 :my-tile "O" :my-turn "First"})
         {:board [["O" "_" "X"] ["X" "O" "X"] ["X" "_" "O"]]
          :my-tile "O", :my-turn "First"})))

; test minor diag
(deftest test-win4
  (is (= (next-move {:board gm3 :my-tile "O" :my-turn "First"})
         {:board [["X" "X" "O"] ["_" "O" "_"] ["O" "_" "_"]]
          :my-tile "O", :my-turn "First"})))

(def gm7 [["O" "O" "_"]
          ["X" "_" "_"]
          ["X" "_" "_"]])

; test blocking rows
(deftest test-block1
  (is (= (next-move {:board gm7 :my-tile "X" :my-turn "First"})
         {:board [["O" "O" "X"] ["X" "_" "_"] ["X" "_" "_"]]
          :my-tile "X", :my-turn "First"})))

; test columns
(deftest test-block2
  (is (= (next-move {:board gm4 :my-tile "O" :my-turn "First"})
         {:board [["X" "_" "O"] ["O" "O" "_"] ["X" "_" "_"]]
          :my-tile "O", :my-turn "First"})))

(def gm8 [["O" "_" "_"]
          ["X" "O" "X"]
          ["_" "_" "_"]])

; test major diag
(deftest test-block3
  (is (= (next-move {:board gm8 :my-tile "X" :my-turn "First"})
         {:board [["O" "_" "_"] ["X" "O" "X"] ["_" "_" "X"]]
          :my-tile "X", :my-turn "First"})))

(def tm5 [["X" "_" "O"]
          ["_" "O" "_"]
          ["_" "_" "X"]])

; test minor diag
(deftest test-block4
  (is (= (next-move {:board tm5 :my-tile "X" :my-turn "First"})
         {:board [["X" "_" "O"] ["_" "O" "_"] ["X" "_" "X"]]
          :my-tile "X", :my-turn "First"})))
