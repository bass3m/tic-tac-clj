(ns tic-tac-toe)

(defn create-board [size]
  (let [my-turn (rand-int 2)]
    {:board (vec (repeat size (vec (repeat size (identity "_")))))
     :tile-values ["_" "X" "O"]
     :size size
     :my-tile (if (zero? my-turn) "X" "O")
     :my-turn (if (zero? my-turn) "First" "Second")}))

(defn get-columns [board]
  (vec (apply map vector board)))

(defn main-diagonal [board size]
  (for [x (range size) y (range size) :when (= x y)] [x y]))

(defn main-diag-values [board]
  (for [[x y] (main-diagonal board (count board))] [((board x) y)]))

(defn minor-diagonal [board size]
  (for [x (range size) y (range size) :when (= (- size 1) (+ x y))] [x y]))

(defn minor-diag-values [board]
  (for [[x y] (minor-diagonal board (count board))] [((board x) y)]))

(defn tile-value-from-position [board x y]
  ((board x) y))

; returns true/false
; reduce returns nil is not all elements in row are equal
(defn three-in-a-row [row]
  (not (nil? (reduce #(if (= %1 %2) %1 nil) row))))

(defn check-board-matches [board]
  (cond
    ; check for 3-in-a-row columns match
    (not (empty? (filter three-in-a-row (get-columns board)))) true
    (not (empty? (filter three-in-a-row board))) true
    (not (empty? (filter three-in-a-row (main-diag-values board)))) true
    (not (empty? (filter three-in-a-row (minor-diag-values board)))) true
    :else false))

(defn win? [{board :board}]
  (check-board-matches board))

; ugh, or
; (assoc-in gm [0 1] "x")
; [[0 "x" 2] [3 4 5] [6 7 8]]

(defn mark-move [board tile x y]
  (map-indexed (fn [i row]
                 (if (= i x)
                   (vec (map-indexed (fn [k el] (if (= k y) tile el)) row))
                   row))
               board))

; modified from Joy of Clojure to include diagonals
(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1] 
                         [1 1] [-1 -1] [-1 1] [1 -1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %)) deltas))))

;(map #(get-in gm %) (neighbors 3 [0 0]))
;=> (3 1 4)

; get the two i a row neighbors for this location
(defn get-two-in-a-row [board x y]
  (let [current-tile ((board x) y)
        my-neighbors (neighbors (count board) [x y])]
    (filter #(if (= current-tile (get-in board %)) %) my-neighbors)))

(defn two-in-a-row? [board x y]
  (not (empty? (get-two-in-a-row board x y))))

; XXX this assumes a size 3 board, but what do you do for larger ?
(defn get-center [board]
  ((board 1) 1))

(defn get-corners [board]
  (for [x [0 (- (count board) 1)] y [0 (- (count board) 1)]] [x y]))
    
(defn get-played-corners [board tile]
  (filter #(if (= tile (get-in board %)) %) (get-corners board)))

(defn first-move [{:keys [board my-tile my-turn]}]
  (if (= my-turn "First")
    ; this is the first move, the optimal move is to choose a corner
    ;(conj (rest board) (assoc (board 0) 0 my-tile))
    (assoc-in board [0 0] my-tile)
    ; i go second, 3 options:
    (cond
      ; if other player played center, then play corner
      (= "X" (get-center board)) (assoc-in board [0 0] my-tile)
      ;(= "X" (get-center board)) (conj (rest board) (assoc (board 0) 0 my-tile))
      ; if other player played a corner, then play center
      (not (empty? (get-played-corners board "X"))) (assoc-in board [1 1] my-tile)
      ; else other player played edge, then just play center
      :else (assoc-in board [1 1] my-tile))))


; this is not a first move
(defn next-move [board]

