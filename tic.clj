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

; XXX not very proud of this at all, needs cleanup
(defn mark-move [board tile x y]
  (loop [row  (vec (first board)), rest-vec (vec (next board))
         prev-vec [] , x x]
    (if (empty? row)
      prev-vec ; this shouldn't happen
      (if (zero? x)
        (conj prev-vec (conj [(assoc row y tile)] rest-vec))
        (recur (first rest-vec) (next rest-vec) (conj prev-vec row) (dec x))))))

(defn make-move [board size tile x y]
  ; change board back to a vector
  (map (partial into []) (partition size size (flatten (mark-move board tile x y)))))

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
