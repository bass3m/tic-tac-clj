(ns tic-tac-toe)

(defn create-board [size]
  {:board (vec (repeat size (vec (repeat size (identity "_")))))
   :tile-values ["_" "X" "O"]
   :size size
   :my-tile "X"})

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

; this is the first move, the optimal move is to choose a corner
(defn first-move [{board :board tile :my-tile}]
  (conj (rest board) (assoc (board 0) 0 tile)))

(defn make-move [board tile x y]
  (loop [row  (first board), rest-vec (next board) , x x]
    (if (empty? row)
      rest-vec ; this shouldn't happen
      (if (neg? x)
        (conj rest-vec (assoc row y tile))
        (recur (first (next rest-vec)) (conj rest-vec row) (dec x))))))

(defn make-move []
  ; change board back to a vector
  (map (partial into []) (partition size size (flatten 
; (vec (partition 3 3 (make-move gm "x" 2 1)))
; [(0 1 2) (3 4 5) (6 "x" 8)]
(defn make-move [board tile x y]
  (loop [row  (vec (first board)), rest-vec (vec (next board))
         prev-vec [] , x x]
    (if (empty? row)
      prev-vec ; this shouldn't happen
      (if (zero? x)
        (conj prev-vec (conj [(assoc row y tile)] rest-vec))
        (recur (first rest-vec) (next rest-vec) (conj prev-vec row) (dec x))))))
        ;(recur (vec (first rest-vec)) (vec (next rest-vec)) (conj prev-vec row) (dec x))))))
        ;(conj prev-vec (vec (drop-while empty? (conj [(assoc row y tile)] rest-vec))))
        ;(flatten (conj prev-vec (vec (drop-while empty? (conj [(assoc row y tile)] rest-vec)))))

; returns a sequential vector with the passed in index removed
; XXX simpler with iterate perhaps
(defn rem-idx-from-vec [size i]
  (vec (flatten (for [x (range size) :when (not= x i)]
                  [(conj (vector-of :int) x)]))))
; [0 2]
;(replace (conj (replace gm [0 2]) (assoc (gm 1) 2 "x")) [0 2 1])
; [[0 1 2] [3 4 "x"] [6 7 8]]
; changing 1 2
(defn make-move [board tile x y]
  (let [idx-vec (rem-idx-from-vec (count board) x)]
    (replace (conj (replace board idx-vec) 
                   (assoc (board x) y tile)) (conj idx-vec x))))

; ugly
(defn make-move [board tile x y]
  (loop [row  (vec (first board))
         rest-vec (vec (next board))
         new-board [] , x x]
    (if (empty? row) ; we shouldn't have empty rows
      new-board ; this shouldn't happen
      (if (empty? rest-vec) ; we reached the end
        new-board ; return the new board with the move made
        (if (zero? x) ; is this the row that we need to make the move in ?
            (recur (vec (first rest-vec)) 
                   (vec (next rest-vec)) 
                   (vec (conj new-board [(assoc row y tile)]))
                   (dec x))
            (recur (vec (first rest-vec)) (vec (next rest-vec)) (conj new-board row) (dec x)))))))
