(ns battleship.core)

(def board-size 10)

(def ship
  {:position [3 7]
   :hit? false})

(def ships
  [{:position [3 7] :hit? false}
   {:position [1 2] :hit? false}
   {:position [8 5] :hit? false}])

(defn -main [& _args]
  (println "Морской бой")
  (println "Размер поля:" board-size "x" board-size))

(defn valid-coordinate? [[x y]]
  (and (<= 0 x (dec board-size))
       (<= 0 y (dec board-size))))

(defn hit? [{:keys [position]} shot]
  (= position shot))

(defn shoot [ship shot]
  (if (hit? ship shot)
    (assoc ship :hit? true)
    ship))

(defn shoot-ships [ships shot]
  (mapv #(shoot % shot) ships))

(defn all-sunk? [ships]
  (every? :hit? ships))

(defn hit-any? [ships shot]
  (boolean
    (some #(hit? % shot) ships)))

(defn shot-result [ships shot]
  (if (hit-any? ships shot)
    :hit
    :miss))

(comment
  (valid-coordinate? [0 0])
  (valid-coordinate? [9 9])
  (valid-coordinate? [10 5])

  (hit? ship [3 7])

  (shoot ship [3 7])
  (shoot-ships ships [1 2])

  (all-sunk? ships)

  (hit-any? ships [1 2])
  (shot-result ships [1 2]))
