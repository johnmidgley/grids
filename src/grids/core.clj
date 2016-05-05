(ns grids.core)


(def directions
  {:up [-1 0]
   :down [1 0]
   :left [0 -1]
   :right [0 1]})

(defn valid-pos? [board row col]
  (let [{:keys [rows cols]} board]
    (and (>= row 0)
         (< row rows)
         (>= col 0)
         (< col cols))))

(defn place [board row col val]
  (if (valid-pos? board row col)
    (assoc-in board [:pieces row col] val)))

(defn goal? [board]
  (let [pieces (:pieces board)]
    (= 1 (->> pieces
              flatten
              (filter some?)
              count))))

; Using specter would make this more elegant.
(defn seek [board pos dir])

(defn valid-move? [board row col move]
  (let [{:keys [rows cols pieces]} board]
    (and (get-in pieces [row col]))))

(defn valid-moves [board]
  (let [{:keys [rows cols pieces]} board]
    ))

;; Passing in directions here enforces referential transparancy. Using the "global" seems
;; bad form.
(defn board [directions rows cols]
  {:rows rows
   :cols cols
   :directions directions
   :pieces (vec (repeat rows (vec (repeat cols nil))))})
