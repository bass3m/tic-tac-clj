(ns tic-tac-toe)

(defn create-board [size]
  (let [my-turn (rand-int 2)]
    {:board (vec (repeat size (vec (repeat size (identity "_")))))
     :tile-values ["_" "X" "O"]
     :size size
     :my-moves []
     :op-moves []
     :my-tile (if (zero? my-turn) "X" "O")
     :my-turn (if (zero? my-turn) "First" "Second")}))

(defn get-op-tile [my-tile]
  (if (= my-tile "X") "O" "X"))

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
;(defn get-two-in-a-row [board x y]
  ;(let [current-tile ((board x) y)
        ;my-neighbors (neighbors (count board) [x y])]
    ;(filter #(if (= current-tile (get-in board %)) %) my-neighbors)))

; pass in the values to check for non-consective matches
(defn non-consec-two-in-a-row? [[x y z]]
  (and (= x z) (= y "_")))

(defn get-non-consec-twos [board]
  ; check row matches
  (if-let [row-matches (filter non-consec-two-in-a-row? board)]
    row-matches
    ; now check columns
    (if-let [colmn-matches
             (filter non-consec-two-in-a-row? (get-columns board))]
      colmn-matches
      ; check main diagonal
      (if-let [main-diag-matches
               (filter non-consec-two-in-a-row? (main-diag-values board))]
        main-diag-matches
        ; check minor diagonal
        (if-let [minor-diag-matches
                 (filter non-consec-two-in-a-row? (minor-diag-values board))]
          minor-diag-matches
          nil)))))

(defn get-two-in-a-row [board x y]
   (let [current-tile ((board x) y)
         my-neighbors (neighbors (count board) [x y])]
     (filter #(= current-tile (get-in board %)) my-neighbors)))

; needs fixing XXX
;(defn get-two-in-a-row
  ;([board x y] (get-two-in-a-row board x y ((board x) y)))
  ;([board x y current-tile]
   ;(let [my-neighbors (neighbors (count board) [x y])]
     ;(filter #(= current-tile (get-in board %)) my-neighbors))))

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
  ; i go first ?
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

; XXX i'm sure there is a clojure ftn that does this better,
; not really proud of this
(defn group-player-moves
  ([moves] (group-player-moves moves {}))
  ([moves player-moves] (if (empty? moves)
                          player-moves
                          (let [move (first moves)
                                loc (:loc move)
                                tile (:tile move)]
                            (group-player-moves (rest moves)
                              (update-in player-moves [tile] (partial cons loc)))))))

; return all the two-in-a-rows on the board
; XXX needs to also handle the case with the empty gap in middle
(defn get-board-two-in-a-rows [board]
  (for [x (range (count board)) y (range (count board))
        :when (not (empty? (get-two-in-a-row board x y)))]
    {:tile ((board x) y) :loc [x y]}))

; do the next move after opening moves are done
(defn next-move [{:keys [board my-tile my-turn] :as game}]
  (let [board-2-in-a-rows (get-board-two-in-a-rows board)
        my-2-in-a-rows (board-2-in-a-rows my-tile)
        op-2-in-a-rows (board-2-in-a-rows (get-op-tile my-tile))]
    (cond
      ; do i have a 2-in-a-row ?, if so then win it
      (not (empty? my-2-in-a-rows)) (win-game game my-2-in-a-rows)
      ; does other player have 2-in-a-row ? block that
      (not (empty? op-2-in-a-rows)) (block-2-in-a-row game op-2-in-a-rows)
      ; fork works if i started first and opp played a corner in response
      ; to my corner, leaving center empty. I can then fork by playing
      ; another corner which will lead to me winning
