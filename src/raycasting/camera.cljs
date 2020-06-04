(ns raycasting.camera
  (:require-macros [raycasting.macros :refer [three-decimal] :as m]))

(def camera (atom {:x 0.0 :y 0.0 :degree 0.0}))
(def radian (/ Math/PI 180))

(defn set-position
  "Set position of the `camera` to a certain `x`, `y`, and `dergree`."
  ([camera] (set-position camera 0 0 0))
  ([camera x y] (set-position camera x y 0.0))
  ([camera x y degree] (assoc camera :x x :y y :degree (mod degree 360.0))))

(defn move-point
  "Calculate new coordinates for [x y] point by moving it given `amount`
  respecting `degree`."
  [[x y] degree amount]
  (let [angle (* degree radian)]
    [(+ x (three-decimal (* (Math/sin angle) amount)))
     (+ y (three-decimal (* (Math/cos angle) amount)))]))

(defn move-forward
  "Move `camera` forward given `amount` respecting the `degree`."
  ([camera] (move-forward camera 1))
  ([camera amount]
   (let [{x :x
          y :y
          degree :degree} camera
         [x y] (move-point [x y] degree amount)]
     (assoc camera :x x :y y))))

(defn rotate
  "Rotate `camera` by an `degree`."
  ([camera] (rotate camera 0.5))
  ([camera degree]
   (assoc camera :degree (mod (+ (:degree camera) degree) 360.0))))


(defn strafe
  "Move `camera` to the side given `amount` respecting the `degree`."
  ([camera] (move-forward camera 1))
  ([camera amount]
   (let [{x :x
          y :y
          degree :degree} camera
         [x y] (move-point [x y] (mod (+ degree 90) 360.0) amount)]
     (assoc camera :x x :y y))))
