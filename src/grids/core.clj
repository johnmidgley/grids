(ns grids.core)

(defn board [rows cols]
  {:rows rows
   :cols cols
   :pieces (vec (repeat rows (vec (repeat cols nil))))})

(defn valid-pos? [board pos]
  (let [{:keys [rows cols]} board
        [row col] pos]
    (and (>= row 0)
         (< row rows)
         (>= col 0)
         (< col cols))))

(defn place [board pos val]
  (if (valid-pos? board pos)
    (let [[row col] pos]
      (assoc-in board [:pieces row col] val))))

(defn get-at [board pos]
  (get-in (:pieces board) pos))

(defn positions [board]
  (let [{:keys [rows cols]} board]
    (for [row (range rows)
          col (range cols)]
      [row col])))

(defn occupied-positions [board]
  (filter #(get-at board %) (positions board)))

(def steps
  {:up [-1 0]
   :down [1 0]
   :left [0 -1]
   :right [0 1]})

(def directions
  (keys steps))

(defn step [dir pos]
  (mapv + (steps dir) pos))

(defn goal? [board]
  (let [pieces (:pieces board)]
    (= 1 (->> pieces
              flatten
              (filter some?)
              count))))

; Using specter would make this more elegant.
(defn seek [board pos dir]
  (when (valid-pos? board pos)
    (if (get-at board pos)
      pos
      (seek board (step dir pos) dir))))

(defn valid-move? [board pos dir]
  (and (get-at board pos)
       (let [adj (step dir pos)]
         (and (not (get-at board adj))
              (seek board (step dir adj) dir)))))

(defn valid-moves [board]
  (for [pos (occupied-positions board)
        dir directions
        :when (valid-move? board pos dir)]
    [pos dir]))


