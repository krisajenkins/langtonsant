(ns lankton.core
  (:require [quil.core :as q]))

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

(def quil-state (atom nil))

(defn setup []
  (q/smooth)
  (q/frame-rate 100)
  (q/background 200)
  (reset! quil-state {:direction :east
                      :position [50 50]
                      :board (vec (repeat 100
                                          (vec (repeat 100 :_))))}) )

(defn draw []
  (let [scale 8
        new-state (swap! quil-state step)]
    (doall (map-indexed (fn [rownum row]
                          (doall (map-indexed (fn [colnum cell]
                                                (if (= cell :_)
                                                  (q/fill 10)
                                                  (q/fill 255))
                                                (q/rect (* rownum scale)
                                                        (* colnum scale)
                                                        scale
                                                        scale
                                                        ))
                                              row)))
                        (:board new-state)))))

(defn -main
  []
  (q/defsketch lankton
    :title "such lankton"
    :setup setup
    :draw draw
    :size [800 800]))

