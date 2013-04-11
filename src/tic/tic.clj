(ns tic.tic
  (:require clojure.edn)
  (:use [clojure.set :as set :only [intersection]]))

(defn create-game
  ([] (create-game 3))
  ([size] (let [my-turn (rand-int 2)]
            {:board (vec (repeat size (vec (repeat size (identity "_")))))
             :tile-values ["_" "X" "O"]
             :game-result ""
             :size size
             :my-moves [] ; for future to keep track of history
             :op-moves []
             :my-tile (if (zero? my-turn) "X" "O")
             :my-turn (if (zero? my-turn) "First" "Second")})))

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
  (not (nil? (reduce #(if (and (not (= %1 "_")) (= %1 %2)) %1 nil) row))))

(defn check-board-matches 
  "Check for 3 in a row matches in rows/columns/diagonals"
  [board]
  (cond
    (not (every? empty? (filter three-in-a-row (get-columns board)))) true
    (not (every? empty? (filter three-in-a-row board))) true
    ; there is only 1 major diagonal and 1 minor diagonal
    (three-in-a-row (vec (flatten (main-diag-values board)))) true
    (three-in-a-row (vec (flatten (minor-diag-values board)))) true
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

; return all the two-in-a-rows on the board
(defn get-board-two-in-a-rows [board]
  (for [x (range (count board)) y (range (count board))
        :when (not (empty? (get-two-in-a-row board x y)))]
    {:tile ((board x) y) :loc [x y]}))

(defn win-game
  "Win game. We'll use the metadata to make life easier. Returns filled board."
  [{:keys [board my-tile] :as game} matches diag-matches]
  (if (or (not (nil? matches))(not (nil? diag-matches)))
    (let [which-match (first (concat matches diag-matches))]
      (case (first (keys (meta which-match)))
        (:Row) (reduce #(assoc-in %1 %2 my-tile) board
                          (for [y (range (count board))]
                            [(first (first which-match)) y]))
        (:Colm) (reduce #(assoc-in %1 %2 my-tile) board
                        (for [x (range (count board))]
                          [x (second (first which-match))]))
        (:Main) (reduce #(assoc-in %1 %2 my-tile) board 
                        (main-diagonal board (count board)))
        (:Minor) (reduce #(assoc-in %1 %2 my-tile) board 
                         (minor-diagonal board (count board)))
        "default" (println "Error, can't win!")))
    (println "Error both are nil")))

(defn block 
  "Block opponent's 3 in a row, returns board."
  [{:keys [board my-tile] :as game} matches diag-matches]
  (if (or (not (nil? matches))(not (nil? diag-matches)))
    (let [which-match (first (concat matches diag-matches))]
      (case (first (keys (meta which-match)))
        ; find the empty slot in the row and fill it with our tile
        (:Row) (reduce #(if (= "_" (get-in %1 %2)) (assoc-in %1 %2 my-tile) %1)
                       board (for [y (range (count board))]
                               [(first (first which-match)) y]))
        ; find the empty slot in the column and fill it with our tile
        (:Colm) (reduce #(if (= "_" (get-in %1 %2)) (assoc-in %1 %2 my-tile) %1)
                        board (for [x (range (count board))]
                                [x (second (first which-match))]))
        (:Main) (reduce #(if (= "_" (get-in %1 %2)) (assoc-in %1 %2 my-tile) %1)
                        board (main-diagonal board (count board)))
        (:Minor) (reduce #(if (= "_" (get-in %1 %2)) (assoc-in %1 %2 my-tile) %1)
                        board (minor-diagonal board (count board)))
        "default" (println "Error, can't block!")))
    (println "Error both are nil")))

; change comments to docstring
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

(defn choose-next-move
  "Returns vector with location of next move.
  If nil is returned then it's a tied game."
  [{:keys [board my-tile my-turn] :as game}]
  (let [avail-corners (get-available-corners board)
        avail-sides (get-available-sides board)
        my-played-corners (get-played-corners board my-tile)
        main-diag (main-diagonal board (count board))
        minor-diag (minor-diagonal board (count board))]
    ; if first always go for corners
    (if (= my-turn "First")
      (cond
        ; prefer opposite corner if available
        (not (empty? (set/intersection (into #{} my-played-corners)
                                       (into #{} main-diag))))
          ; i'm on main diagonal, let's see if there's any available spots
          (first (set/intersection (into #{} avail-corners)
                                   (into #{} main-diag)))
        ; otherwise must be minor diagonal
        (not (empty? (set/intersection (into #{} my-played-corners)
                                       (into #{} minor-diag))))
          ; i'm on minor diagonal, let's see if there's any available spots
          (first (set/intersection (into #{} avail-corners)
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
        (block game op-2-in-a-rows op-diag-matches)
      ; fork works if i started first and opp played a corner in response
      ; to my corner, leaving center empty. I can then fork by playing
      ; another corner which will lead to me winning
      :else {:board (assoc-in board (choose-next-move game) my-tile)
             :my-tile my-tile :my-turn my-turn})))
      ;:else (choose-next-move game))))

(defn do-player-move [{:keys [board my-tile my-turn] :as game} move]
  ; check if it's a legal move, i.e. whether the coords are valid
    (println "test" move (get-in board move) my-tile my-turn)
  (if (= "_" (get-in board move)) ; slot is empty ?
    ; fill in opponent play then do ours after that play
    (let [outcome (next-move {:board (assoc-in board move 
                                               (get-op-tile my-tile)) 
                              :my-tile my-tile :my-turn my-turn})]
      (println (:board outcome))
      ; need to find out if there was a tie or a win etc..
      (if (win? outcome)
        (println "Sorry you lost. Me the Winnar!")
        (do (println outcome) outcome)))
    (println "Invalid board location: " (get-in board move) board move)))

(defn print-help []
  (do
    (println "Commands: help, move, new (create new game), quit")
    (println "new: start a new game")
    (println "help: prints this super helpful message")
    (println "move: make a move. Pick coordinates [x y] of position to play")
    (println "quit: self explanatory, me thinks")))

(defn print-board [game]
  (println game))

(defn player-move [game arg]
  (if (nil? game)
    (println "Non game started ?")
    (let [move (clojure.edn/read-string (clojure.string/join " " arg))]
      ; make sure player picked a proper spot on our board
      (if (and (vector? move)
               (= 2 (count move)) ; spot is 2 dimensional
               (#(< -1 % 3) (apply max move))) ; check range
                  (do-player-move game move)
        (do (println "No good" move game arg) move)))))

(defn start-new-game [& args]
  (println "Got a start")
  (create-game))

(def game-commands {"quit" (fn [game] (println "quitting"))
                    "help" print-help
                    "new" start-new-game
                    "move" (fn [game & args] (player-move game args))})

(defn execute [game player-input]
  (try (let [[command & args] (.split player-input " ")]
         (do (println "Game: " game " inp:" player-input " args:" args)
         (apply (game-commands command) game args)))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))

