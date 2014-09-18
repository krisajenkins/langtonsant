(ns lankton.core
  (:require [clojure.pprint :refer :all]))

(defn flip-current
  [state]
  (let [[x y] (:position state)]
    (update-in state
               [:board y x]
               (fn [color]
                 (if (= color :X)
                   :_
                   :X)))))

(defn move-1-step
  [state]
  (let [direction (:direction state)
        [x y] (:position state)
        [new-x new-y :as new-position] (case direction
                                         :east  [(inc x) y]
                                         :west  [(dec x) y]
                                         :north [x (dec y)]
                                         :south [x (inc y)])]

    (assert (<= 0 new-x) (format "Fallen off the west of the world.: %s" new-position))
    (assert (<= 0 new-y) "Fallen off the north of the world.")
    (assert (< new-y (count (:board state))) "Fallen off the south of the world.")
    (assert (< new-x (count (first (:board state)))) "Fallen off the east of the world.")

    (assoc state :position new-position)))

(defn current-color
  [state]
  (let [[x y] (:position state)]
    (get-in state [:board y x])))

(defn change-direction
  [state]
  (let [current-direction (:direction state)]
    (assoc state :direction
           (if (= (current-color state) :X)
             (case current-direction
               :east :north
               :north :west
               :west :south
               :south :east)
             (case current-direction
               :north :east
               :west :north
               :south :west
               :east :south)))))

(defn step
  [state]
  (move-1-step (change-direction (flip-current state))))

(defn row-to-string
  [row]
  (apply str (map name row)))

(defn board-to-string
  [board]
  (clojure.string/join "\n"
                       (map row-to-string board)))

(defn -main
  []
  (binding [*print-right-margin* 400]
    (let [initial-state {:direction :east
                         :position [50 50]
                         :board (vec (repeat 100
                                             (vec (repeat 100 :_))))}]
      (println (board-to-string (:board (nth (iterate step initial-state)
                                             10000)))))))
