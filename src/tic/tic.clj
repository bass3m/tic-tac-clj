(ns tic.tic)

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

(defn get-row-two-matches [i [x y z]]
  (cond
    (and (= x z) (or (= x "X") (= x "O")) (= y "_")) 
      ^:Row {:tile x :loc [[i 0] [i 2]]}
    (and (= x y) (or (= x "X") (= x "O")) (= z "_")) 
      ^:Row {:tile x :loc [[i 0] [i 1]]}
    (and (= y z) (or (= y "X") (= y "O")) (= x "_")) 
      ^:Row {:tile y :loc [[i 1] [i 2]]}
    :else nil))

(defn get-colm-two-matches [i [x y z]]
  (cond
    (and (= x z) (or (= x "X") (= x "O")) (= y "_")) 
      ^:Colm {:tile x :loc [[0 i] [2 i]]}
    (and (= x y) (or (= x "X") (= x "O")) (= z "_")) 
      ^:Colm {:tile x :loc [[0 i] [1 i]]}
    (and (= y z) (or (= y "X") (= y "O")) (= x "_")) 
      ^:Colm {:tile y :loc [[1 i] [2 i]]}
    :else nil))

(defn get-diag-two-matches [board]
  (let [[x y z] (flatten (main-diag-values board))
        [a b c] (flatten (minor-diag-values board))]
    (cond
      (and (= x z) (or (= x "X") (= x "O")) (= y "_")) 
        (list ^:Main {:tile x :loc [[0 0] [2 2]]})
      (and (= x y) (or (= x "X") (= x "O")) (= z "_")) 
        (list ^:Main {:tile x :loc [[0 0] [1 1]]})
      (and (= y z) (or (= y "X") (= y "O")) (= x "_")) 
        (list ^:Main {:tile y :loc [[1 1] [2 2]] })
      (and (= a c) (or (= a "X") (= a "O")) (= b "_")) 
        (list ^:Minor {:tile a :loc [[0 2] [2 0]]})
      (and (= a b) (or (= a "X") (= a "O")) (= c "_")) 
        (list ^:Minor {:tile a :loc [[0 2] [1 1]]})
      (and (= b c) (or (= b "X") (= b "O")) (= a "_")) 
        (list ^:Minor {:tile b :loc [[1 1] [2 0]]})
      :else nil)))

(defn get-board-two-matches [board]
  (flatten (cons (keep-indexed get-row-two-matches board)
                 (keep-indexed get-colm-two-matches (get-columns board)))))

; get the two i a row neighbors for this location
(defn get-two-in-a-row [board x y]
   (let [current-tile ((board x) y)
         my-neighbors (neighbors (count board) [x y])]
     (filter #(and (= current-tile (get-in board %)) 
                   (or (= current-tile "X") 
                       (= current-tile "O"))) my-neighbors)))

; XXX this assumes a size 3 board, but what do you do for larger ?
(defn get-center [board]
  ((board 1) 1))

(defn get-corners [board]
  (for [x [0 (- (count board) 1)] y [0 (- (count board) 1)]] [x y]))

(defn get-sides [board]
  (let [size (count board)]
    (for [x (range size) y (range size) :when (or (= -1 (- x y))(= 1 (- x y)))] [x y])))

(defn get-available-sides [board]
  (filter #(if (= "_" (get-in board %)) %) (get-sides board)))

(defn get-played-corners [board tile]
  (filter #(if (= tile (get-in board %)) %) (get-corners board)))

(defn get-available-corners [board]
  (filter #(if (= "_" (get-in board %)) %) (get-corners board)))

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
                              (update-in player-moves [tile] 
                                         (partial cons 
                                                  (with-meta loc (meta move)))))))))

;XXX can i use pattern matching on the results somehow ?
; return all the two-in-a-rows on the board
(defn get-board-two-in-a-rows [board]
  (for [x (range (count board)) y (range (count board))
        :when (not (empty? (get-two-in-a-row board x y)))]
    {:tile ((board x) y) :loc [x y]}))

; i actually, don't need to find the missing one, i just blindly fill the whole
; row/column/diag etc... XXX
; use the peepcode for example of multi-player /output etc..
(defn win-game [game matches diag-matches]
  ; win-game should only be called if we either have row/column matches
  ; else we have diagonal matches. we'll use the metadata to make life easier
  (if (or (not (nil? matches))(not (nil? diag-matches)))
    (case (first (keys (meta (first (concat matches diag-matches)))))
      (:Row) (println "Row")
      (:Colm) (println "Column")
      (:Main) (println "Main Diag")
      (:Minor) (println "Minor Diag")
      "default" (println "Error, can't win!"))
    (println "Error both are nil")))
    ; we can win using row matches. let fill it
    ; otherwise, must be using a diag
  ;(println "You WIN!" matches)
  ;(println "Meta" (meta (first matches)))
  ;(println "You WIN! Diags" diag-matches)
  ;(println "Meta" (meta (first diag-matches))))

(defn block-2-in-a-row [game matches diag-matches]
  (println "You Block" matches)
  (println "Meta" (meta matches))
  (println "You Block Diags" diag-matches)
  (println "Meta" (meta diag-matches)))

(defn first-move [{:keys [board my-tile my-turn]}]
  ; i go first ?
  (if (= my-turn "First")
    ; this is the first move, the optimal move is to choose a corner
    (assoc-in board [0 0] my-tile)
    ; i go second, 3 options:
    (cond
      ; if other player played center, then play corner
      (= "X" (get-center board)) (assoc-in board [0 0] my-tile)
      ; if other player played a corner, then play center
      (not (empty? (get-played-corners board "X"))) (assoc-in board [1 1] my-tile)
      ; else other player played edge, then just play center
      :else (assoc-in board [1 1] my-tile))))

(defn choose-next-move [{:keys [board my-tile my-turn] :as game}]
  (let [avail-corners (get-available-corners board)
        avail-sides (get-available-sides board)
        my-played-corners (get-played-corners board my-tile)
        main-diag (main-diagonal board (count board))
        minor-diag (minor-diagonal board (count board))]
    ; if first always go for corners
    (if (= my-turn "First")
      (cond
        ; prefer opposite corner if available
        (not (empty? (clojure.set/intersection (into #{} my-played-corners)
                                               (into #{} main-diag))))
          ; i'm on main diagonal, let's see if there's any available spots
          (first (clojure.set/intersection (into #{} avail-corners) 
                                           (into #{} main-diag)))
        ; otherwise must be minor diagonal
        (not (empty? (clojure.set/intersection (into #{} my-played-corners)
                                               (into #{} minor-diag))))
          ; i'm on minor diagonal, let's see if there's any available spots
          (first (clojure.set/intersection (into #{} avail-corners) 
                                           (into #{} minor-diag)))
        ; else choose an availble corner
        (not (empty? avail-corners)) (first avail-corners)
        ; else choose center
        (= "_" (get-center board)) [1 1]
        ; else side
        (not (empty? avail-sides)) (first avail-sides)
        :else nil)
      ; else turn = Second case, defending is somewhat easier
      (cond
        ; we're going second, pick center first
        (= "_" (get-center board)) [1 1]
        ; defend against forks by picking a side
        (not (empty? avail-sides)) (first avail-sides)
        ; else choose an availble corner
        (not (empty? avail-corners)) (first avail-corners)
        :else nil))))

; do the next move after opening moves are done
(defn next-move [{:keys [board my-tile my-turn] :as game}]
  (let [board-2-in-a-rows (get-board-two-matches board)
        my-2-in-a-rows ((group-player-moves board-2-in-a-rows) my-tile)
        diag-matches (get-diag-two-matches board)
        my-diag-matches ((group-player-moves diag-matches) my-tile)
        op-2-in-a-rows ((group-player-moves board-2-in-a-rows) (get-op-tile my-tile))
        op-diag-matches ((group-player-moves diag-matches) (get-op-tile my-tile))]
    (cond
      ; do i have a 2-in-a-row ?, if so then win it
      (or (not (empty? my-2-in-a-rows)) 
          (not (empty? my-diag-matches)))
        (win-game game my-2-in-a-rows my-diag-matches)
      ; does other player have 2-in-a-row ? block that
      (or (not (empty? op-2-in-a-rows)) 
          (not (empty? op-diag-matches)))
        (block-2-in-a-row game op-2-in-a-rows op-diag-matches)
      ; fork works if i started first and opp played a corner in response
      ; to my corner, leaving center empty. I can then fork by playing
      ; another corner which will lead to me winning
      :else (choose-next-move game))))
