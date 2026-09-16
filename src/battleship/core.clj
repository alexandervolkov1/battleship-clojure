(ns battleship.core)

;;===========================================================
;; Constants
;;===========================================================

(def board-size 10)

(def fleet-spec
  [4 3 3 2 2 2 1 1 1 1])

;;===========================================================
;; Board geometry
;;===========================================================

(defn valid-coordinate? [[x y]]
  (and (<= 0 x (dec board-size))
       (<= 0 y (dec board-size))))

(defn ship-cells [[x y] length orientation]
  (set
    (map (fn [offset]
           (case orientation
             :horizontal [(+ x offset) y]
             :vertical   [x (+ y offset)]))
         (range length))))

(defn valid-ship? [cells]
  (every? valid-coordinate? cells))

(defn neighbor-cells [[x y]]
  (set
   (for [dx [-1 0 1]
         dy [-1 0 1]]
     [(+ x dx) (+ y dy)])))

(defn ship-neighborhood [cells]
  (set
   (mapcat neighbor-cells cells)))

;;============================================================
;; Fleet
;;===============================================================

(defn fleet-cells [fleet]
  (set (mapcat :cells fleet)))

(defn can-place-ship? [new-cells fleet]
  (let [occupied (fleet-cells fleet)
        forbidden (ship-neighborhood occupied)]
    (and
     (valid-ship? new-cells)
     (not-any? #(contains? forbidden %) new-cells))))

(defn place-ship [fleet start length orientation]
  (let [new-ship (ship-cells start length orientation)]
    (if (can-place-ship? new-ship fleet)
      (conj fleet {:cells new-ship})
      nil)))

;;===============================================================
;; Shooting
;;===============================================================

(defn ship-at [fleet coordinate]
  (some
   #(when (contains? (:cells %) coordinate) %)
   fleet))

(defn hit? [fleet coordinate]
  (some? (ship-at fleet coordinate)))

(defn sunk? [ship shots]
  (every? #(contains? shots %) (:cells ship)))

(defn all-sunk? [fleet shots]
  (every? #(sunk? % shots) fleet))

(defn shot-result [fleet shots coordinate]
  (let [new-shots (conj shots coordinate)]
    (if

;;===============================================================
;; tests
;;===============================================================
  
(comment

  (def sample-fleet
    [{:cells #{[0 0] [1 0] [2 0] [3 0]}}
     {:cells #{[9 0] [9 1] [9 2]}}
     {:cells #{[4 9] [5 9]}}
     {:cells #{[9 9]}}])

  ;; valid edge cases
  (valid-ship? (ship-cells [0 0] 4 :horizontal))
  (valid-ship? (ship-cells [9 7] 3 :vertical))
  (valid-ship? (ship-cells [7 9] 3 :horizontal))

  ;; outside board
  (valid-ship? (ship-cells [8 5] 3 :horizontal))
  (valid-ship? (ship-cells [4 9] 2 :vertical))

  ;; fleet placement
  (can-place-ship?
   (ship-cells [5 4] 3 :horizontal)
   sample-fleet)

  ;; overlap
  (can-place-ship?
   (ship-cells [2 0] 3 :vertical)
   sample-fleet)

  ;; side contact
  (can-place-ship?
   (ship-cells [0 1] 2 :horizontal)
   sample-fleet)

  ;; diagonal contact
  (can-place-ship?
   #{[4 1]}
   sample-fleet)

  ;; add new ship
  (place-ship sample-fleet [5 5] 2 :vertical))
