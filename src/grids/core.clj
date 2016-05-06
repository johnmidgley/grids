(ns grids.core)

(defn board [rows cols]
  {:rows rows
   :cols cols
   :pieces (vec (repeat rows (vec (repeat cols nil))))})

(defn piece [board pos]
  (let [[row col] pos]
    (get-in board [:pieces row col])))

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


(def directions
  {:up [-1 0]
   :down [1 0]
   :left [0 -1]
   :right [0 1]})

(defn step [dir pos]
  (mapv + (directions dir) pos))

(defn goal? [board]
  (let [pieces (:pieces board)]
    (= 1 (->> pieces
              flatten
              (filter some?)
              count))))

; Using specter would make this more elegant.
(defn seek [board pos dir]
  (when (valid-pos? board pos)
    (if (piece board pos)
      pos
      (seek board (step dir pos) dir))))

(defn valid-move? [board pos dir]
  (let [{:keys [rows cols pieces]} board]
    (and (get-in pieces pos))))

(defn valid-moves [board]
  (let [{:keys [rows cols pieces]} board]))


