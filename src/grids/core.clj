(ns grids.core)

(defn board [rows cols]
  {:rows rows
   :cols cols
   :pieces (vec (repeat rows (vec (repeat cols nil))))})

(defn position [row col]
  {:row row
   :col col})

(defn row [pos]
  (:row pos))

(defn col [pos]
  (:col pos))

(defn valid-pos? [board pos]
  (let [{:keys [rows cols]} board
        {:keys [row col]} pos]
    (and (>= row 0)
         (< row rows)
         (>= col 0)
         (< col cols))))

(defn place [board pos val]
  (if (valid-pos? board pos)
    (let [{:keys [row col]} pos]
      (assoc-in board [:pieces row col] val))))


(def directions
  {:up [-1 0]
   :down [1 0]
   :left [0 -1]
   :right [0 1]})


(defn goal? [board]
  (let [pieces (:pieces board)]
    (= 1 (->> pieces
              flatten
              (filter some?)
              count))))

; Using specter would make this more elegant.
(defn seek [board pos dir])

(defn valid-move? [board pos dir]
  (let [{:keys [rows cols pieces]} board
        {:keys [row col]} pos]
    (and (get-in pieces [row col]))))

(defn valid-moves [board]
  (let [{:keys [rows cols pieces]} board]
    ))


