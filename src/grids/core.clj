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

(defn set-at [board pos val]
  (if (valid-pos? board pos)
    (let [[row col] pos]
      (assoc-in board [:pieces row col] val))))

(defn get-at [board pos]
  (get-in (:pieces board) pos))

(defn remove-at [board pos]
  [(get-at board pos)
   (set-at board pos nil)])

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

(defn step-forward [dir pos]
  (when (and dir pos)
    (mapv + pos (steps dir))))

(defn step-backward [dir pos]
  (when (and dir pos)
    (mapv - pos (steps dir))))

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
      (seek board (step-forward dir pos) dir))))

(defn valid-move? [board pos dir]
  (and (get-at board pos)
       (let [adj (step-forward dir pos)]
         (and (not (get-at board adj))
              (seek board (step-forward dir adj) dir)))))

(defn valid-moves [board]
  (when board
    (for [pos (occupied-positions board)
          dir directions
          :when (valid-move? board pos dir)]
      [pos dir])))

(defn move [board pos dir]
  (if (valid-move? board pos dir)
    (loop [b board
           p pos]
      (let [[v b] (remove-at b p)
            t (seek b (step-forward dir p) dir)
            dst (step-backward dir t)]
        (if dst
          (recur (set-at b dst v) t)
          b)))))

(defn play [board]
  (let [moves (valid-moves board)]
    (cond (goal? board) []
          (empty? moves) nil
          :else
          (->> (for [m moves
                     :let [s (play (apply move board m))]
                     :when s]
                 (conj s m))
               (filter some?)
               not-empty))))
(def b
  (-> (board 8 7)
      (set-at [0 3] 1)
      (set-at [0 6] 1)
      (set-at [3 4] 1)))

#_(def b
  (-> (board 8 7)
      (set-at [0 0] 1)
      (set-at [0 6] 1)
      (set-at [2 2] 2)))

(def b
  (-> (board 8 7)
      (set-at [0 3] 1)
      (set-at [1 1] 1)
      (set-at [1 3] 1)
      (set-at [2 0] 1)
      (set-at [2 2] 1)
      (set-at [2 6] 1)
      (set-at [3 0] 1)
      (set-at [3 1] 1)
      (set-at [5 4] 1)))
