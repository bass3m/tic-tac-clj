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

; pass in the values to check for non-consective matches
(defn non-consec-two-in-a-row? 
  ([[x y z]] (and (= x z) (= y "_")))
  ([tile [x y z]] (and (= x z tile) (= y "_"))))

; same as above but also return location
(defn idx-non-consec-two-in-a-row?
  ([i [x y z]] (if (and (= x z) (= y "_")) {:tile x :loc [[i 0] [i 2]]}))
  ([tile i [x y z]] (and (= x z tile) (= y "_")){:tile tile :loc [[i 0] [i 2]]}))

(defn idx-non-consec-colmn-two-in-a-row?
  ([i [x y z]] (if (and (= x z) (= y "_")) {:tile x :loc [[0 i] [2 i]]})))

(defn get-non-consec-idx [board]
  (keep-indexed idx-non-consec-two-in-a-row? board))

(defn get-row-two-matches [i [x y z]]
  (cond
    (and (= x z) (or (= x "X") (= x "O")) (= y "_")) 
      {:tile x :loc [[i 0] [i 2]] :descr "Non Consec Row"}
    (and (= x y) (or (= x "X") (= x "O")) (= z "_")) 
      {:tile x :loc [[i 0] [i 1]] :descr "Consec Row"}
    (and (= y z) (or (= y "X") (= y "O")) (= x "_")) 
      {:tile y :loc [[i 1] [i 2]] :descr "Consec Row"}
    :else nil))

(defn get-colm-two-matches [i [x y z]]
  (cond
    (and (= x z) (or (= x "X") (= x "O")) (= y "_")) 
      {:tile x :loc [[0 i] [2 i]] :descr "Non Consec Colm"}
    (and (= x y) (or (= x "X") (= x "O")) (= z "_")) 
      {:tile x :loc [[0 i] [1 i]] :descr "Consec Colm"}
    (and (= y z) (or (= y "X") (= y "O")) (= x "_")) 
      {:tile y :loc [[1 i] [2 i]] :descr "Consec Colm"}
    :else nil))

(defn get-board-two-matches [board]
  (flatten (cons (keep-indexed get-row-two-matches board)
                 (keep-indexed get-colm-two-matches (get-columns board)))))

(defn get-non-consec-colmn-idx [board]
  (keep-indexed idx-non-consec-colmn-two-in-a-row? (get-columns board)))

(defn get-non-consec-twos [board]
  ; check row matches
  (let [row-matches (get-non-consec-idx board)]
    (if (not (empty? row-matches))
      row-matches
      ; now check columns
      (let [colmn-matches (get-non-consec-colmn-idx board)]
        (if (not (empty? colmn-matches))
          colmn-matches
          ; check main diagonal
          ; diagonals are much easier since we only have 2
          (if (non-consec-two-in-a-row? (flatten (main-diag-values board)))
            (list {:tile (get-in board [0 0]) :loc [[0 0] [2 2]]})
            ; check minor diagonal
            (if (non-consec-two-in-a-row? (flatten (minor-diag-values board)))
              (list {:tile (get-in board [0 2]) :loc [[0 2] [2 0]]})
              nil)))))))

(defn is-non-consec-twos? [board tile]
  (some #(= tile %) (first (get-non-consec-twos board))))

; get the two i a row neighbors for this location
(defn get-two-in-a-row [board x y]
   (let [current-tile ((board x) y)
         my-neighbors (neighbors (count board) [x y])]
     (filter #(and (= current-tile (get-in board %)) 
                   (or (= current-tile "X") 
                       (= current-tile "O"))) my-neighbors)))

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

;XXX can i use pattern matching on the results somehow ?
; return all the two-in-a-rows on the board
(defn get-board-two-in-a-rows [board]
  (for [x (range (count board)) y (range (count board))
        :when (not (empty? (get-two-in-a-row board x y)))]
    {:tile ((board x) y) :loc [x y]}))

(defn win-game [game matches]
  (println "You WIN!" matches))

(defn block-2-in-a-row [game matches]
  (println "Block"))

(defn can-i-win? [{:keys [board my-tile] :as game}]
  (let [board-2-in-a-rows (get-board-two-in-a-rows board)
        my-2-in-a-rows ((group-player-moves board-2-in-a-rows) my-tile)
        non-consec-matches (get-non-consec-twos board)
        my-2-non-consecs ((group-player-moves non-consec-matches) my-tile)]
    ; do i have a 2-in-a-row ?
    (or (not (empty? my-2-in-a-rows))
        ; or do i have 2 non consec plays ? if so we can win
        (not (empty? my-2-non-consecs)))))
    
; do the next move after opening moves are done
(defn next-move [{:keys [board my-tile my-turn] :as game}]
  (let [board-2-in-a-rows (get-board-two-in-a-rows board)
        my-2-in-a-rows ((group-player-moves board-2-in-a-rows) my-tile)
        op-2-in-a-rows ((group-player-moves board-2-in-a-rows) (get-op-tile my-tile))
        non-consec-matches (get-non-consec-twos board)]
    (cond
      (can-i-win? game) (win-game game my-2-in-a-rows)
      ; do i have a 2-in-a-row ?, if so then win it
      (not (empty? my-2-in-a-rows)) (win-game game my-2-in-a-rows)
      ; do i have 2 non consec plays ? if so we can win
      (and (not (empty? non-consec-matches))
           (= (:tile non-consec-matches) my-tile)) (win-game game non-consec-matches)
      ; does other player have 2-in-a-row ? block that
      (not (empty? op-2-in-a-rows)) (block-2-in-a-row game op-2-in-a-rows)
      ; block opponent if they have 2 non-consec matches
      (and (not (empty? non-consec-matches))
           (= (:tile non-consec-matches) (get-op-tile my-tile)))
           (block-2-in-a-row game non-consec-matches)
      ; fork works if i started first and opp played a corner in response
      ; to my corner, leaving center empty. I can then fork by playing
      ; another corner which will lead to me winning
      :else (println "Else:" my-tile))))
