(use 'clojure.test)
(use 'tic)


(def gm1 [["X" "O" "X"]
          ["_" "O" "_"]
          ["_" "_" "_"]])

(def gm2 [["X" "_" "O"]
          ["_" "_" "_"]
          ["_" "_" "_"]])

(def gm3 [["X" "X" "_"]
          ["_" "O" "_"]
          ["O" "_" "_"]])

(def gm4 [["X" "_" "O"]
          ["_" "O" "_"]
          ["X" "_" "_"]])

(def gm5 [["X" "_" "O"]
          ["_" "_" "O"]
          ["_" "_" "X"]])

(def gm6 [["O" "_" "X"]
          ["O" "_" "_"]
          ["X" "_" "_"]])

(def gm7 [["O" "_" "_"]
          ["_" "X" "_"]
          ["X" "_" "_"]])

(def gm8 [["O" "_" "_"]
          ["X" "O" "X"]
          ["_" "_" "_"]])

(def gm9 [["O" "_" "X"]
          ["X" "O" "X"]
          ["X" "_" "_"]])

(def tm1 [["X" "_" "_"]
          ["_" "O" "_"]
          ["_" "_" "_"]])

(deftest test-board-matched [board]
  (is (= (get-board-two-matches board)
         ({:tile "X", :loc [[0 0] [2 0]], :descr "Non Consec Colm"}))))

