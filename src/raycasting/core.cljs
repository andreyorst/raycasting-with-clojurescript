(ns raycasting.core
  (:require [raycasting.camera :as cam])
  (:require [raycasting.input :as input])
  (:require [raycasting.stage :as stage])
  (:require-macros [raycasting.macros
                    :refer [three-decimal binding*]
                    :as m]))

(defonce ^:dynamic *canvas* nil)
(defonce ^:dynamic *ctx* nil)
(defonce ^:dynamic *ray-count* 42)
(defonce ^:dynamic *fov* 60)
(defonce ^:dynamic *compensate-fisheye* true)

(def max-fov 360)
(def max-compensated-fov 180)
(def infinity 100000)
(def stage stage/stage)

(defn distance
  "Compute distance between two points."
  [[x1 y1] [x2 y2]]
  (three-decimal
   (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                 (Math/pow (- y1 y2) 2)))))

(defn on-segment?
  "Checks if point `[ax ay]` lies on segment defined by two points `[b
  c]`, where each point is defined as a vector `[x y]`."
  [[ax ay] [[bx by] [cx cy]]]
  (and (<= ax (max bx cx))
       (>= ax (min bx cx))
       (<= ay (max by cy))
       (<= ay (min by cy))))

(defn orientation
  "Checks orientation of a point relatively to two other points."
  [[ax ay] [bx by] [cx cy]]
  (let [orientation (- (* (- by ay) (- cx bx))
                       (* (- bx ax) (- cy by)))]
    (cond (> orientation 0) :cw
          (< orientation 0) :cc
          :else :colinear)))

(defn intersect?
  "Checks if two line segments intersect. Line segments are defined as
  vector of two points `[a1 a2]`, and each point is a vector of two
  coordinates `[x y]`."
  [[a1 a2] [b1 b2]]
  (let [o1 (orientation a1 a2 b1)
        o2 (orientation a1 a2 b2)
        o3 (orientation b1 b2 a1)
        o4 (orientation b1 b2 a2)]
    (or
     (and (not (= o1 o2))
          (not (= o3 o4)))
     (and (= o1 :colinear)
          (on-segment? b1 [a1 a2]))
     (and (= o2 :colinear)
          (on-segment? b2 [a1 a2]))
     (and (= o3 :colinear)
          (on-segment? a1 [b1 b2]))
     (and (= o4 :colinear)
          (on-segment? a2 [b1 b2])))))

(defn intersection
  "Compute intersection point between two line segments."
  [[[x1 y1] [x2 y2] :as ray] [[x3 y3] [x4 y4] :as wall]]
  (let [x (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4)) (* (- x1 x2) (- (* x3 y4) (* y3 x4))))
             (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
        y (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4)) (* (- y1 y2) (- (* x3 y4) (* y3 x4))))
             (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))]
    [x y]))

(defn draw-line
  "Draw line in context of canvas specified by `*ctx*` binding."
  ([[x1 y1] [x2 y2]]
   (draw-line [x1 y1] [x2 y2] "#000000"))
  ([[x1 y1] [x2 y2] color]
   (set! (. *ctx* -strokeStyle) color)
   (doto *ctx*
     (.beginPath)
     (.moveTo x1 y1)
     (.lineTo x2 y2)
     (.stroke))))

(defn find-intersections
  "Find all intersection points with all walls that presented within `stage`.
  Returns a map, which has distance as a key, and a map with endpoint
  and color."
  [stage [origin end :as ray]]
  (->> stage
       (map (fn [[_ _ color :as wall]]
              (if (intersect? ray wall)
                (let [new (intersection ray wall)]
                  [(distance origin new) {:end new :color (or color color "#eeeeee")}])
                [infinity {:end end :color "#000000"}])))
       (into {})))

(defn projection-distance
  "Calculate projection distance between player and projection plane."
  []
  (three-decimal
   (/ (/ *ray-count* 2) (Math/atan (* (/ *fov* 2) cam/radian)))))

(defn dim
  "Dims two digit hex encoded color by some `amount`."
  [color amount]
  (let [dim-factor (Math/pow 1.1 (/ (inc amount) 7))
        color (int (/ color dim-factor))]
    (if (< color 25)
      25
      color)))

(defn dim-color
  "Parses six digit hex encoded color string (#000000) and returns a
  darker rgb color string."
  [color distance]
  (let [red (js/parseInt (str "0x" (subs color 1 3)))
        green (js/parseInt (str "0x" (subs color 3 5)))
        blue (js/parseInt (str "0x" (subs color 5)))]
    (str "rgb("
         (dim red distance)
         ", "
         (dim green distance)
         ", "
         (dim blue distance)
         ")")))

(defn inside-rectangle
  "Check if point is inside rectangle defined by four points."
  [camera [[ax ay] [bx by] [cx cy] [dx dy]]]
  (let [walls [[[ax ay] [bx by]]
               [[bx by] [cx cy]]
               [[cx cy] [dx dy]]
               [[dx dy] [ax ay]]]
        {x :x y :y} camera
        {x' :x y' :y} (cam/move-forward {:x x :y y :degree 90} 1000)]
    (->> walls
         (map (fn [wall]
                (if (intersect? [[x y] [x' y']] wall) 1 0)))
         (reduce + 0)
         odd?)))

(defn draw-3d-stage
  "Draws pseudo 3d stage on `*canvas*`."
  [rays]
  (let [ray-width (/ (/ (. *canvas* -width) 2) *ray-count*)
        height (. *canvas* -height)
        canvas-width (/ (. *canvas* -width) 2)
        projection-distance (projection-distance)
        gradient (. *ctx* createLinearGradient 0 0 0 height)]
    (doto gradient
      (.addColorStop 0 "#424242")
      (.addColorStop 0.5 "#111111")
      (.addColorStop 1 "#cccccc"))
    (set! (. *ctx* -fillStyle) gradient)
    (. *ctx* fillRect canvas-width 0 canvas-width height)
    (loop [rays (sort (fn [{l1 :length} {l2 :length}] (> l1 l2)) rays)]
      (when-let [ray (first rays)]
        (let [{length :length ray-degree :angle n :n color :color} ray]
          (when (< length infinity) ;; skip drawing infinity rays
            (let [angle-step (/ *fov* *ray-count*)
                  distance (if *compensate-fisheye*
                             (* length (three-decimal (Math/cos ray-degree)))
                             length)
                  wall-height (* (/ (/ height *ray-count*) distance) projection-distance (/ *fov* 4))
                  wall-height (if (> wall-height height)
                                height
                                wall-height)
                  color (dim-color color distance)]
              (set! (. *ctx* -fillStyle) color)
              (. *ctx*
                 fillRect
                 (+ canvas-width (* n ray-width))
                 (- (/ height 2) (/ wall-height 2))
                 (+ ray-width 1) ;; extra pixel for border
                 wall-height))))
        (recur (rest rays))))))

(defn angle-between
  "Computes angle between two line segments."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [theta1 (Math/atan2 (- y1 y2) (- x1 x2))
        theta2 (Math/atan2 (- y3 y4) (- x3 x4))
        diff (Math/abs (- theta1 theta2))]
    (min diff (Math/abs (- 180 diff)))))

(defn cast-rays
  "Cast `*ray-count*` amount of rays from `camera` on the `stage` and
  returns list of shortest rays."
  [camera stage]
  (let [step (/ *fov* *ray-count*)
        ray-start [(:x camera) (:y camera)]
        central-ray [ray-start (cam/move-point ray-start (:degree camera) 100)]]
    (loop [n (dec *ray-count*)
           degree (:degree (cam/rotate camera (- (/ *fov* 2.0))))
           rays '()]
      (if (>= n 0)
        (do (let [ray-end (cam/move-point ray-start degree infinity)
                  intersections (find-intersections stage [ray-start ray-end])
                  shortest-ray (apply min (keys intersections))
                  {ray-end :end color :color} (intersections shortest-ray)
                  ray-angle (if *compensate-fisheye*
                              (angle-between [ray-start ray-end] central-ray)
                              0)]
              (recur (dec n)
                     (+ degree step)
                     (conj rays {:end ray-end :length shortest-ray :angle ray-angle :n n :color color}))))
        rays))))

(defn draw-stage
  "Draw 2D representation of `stage` on `*canvas*`."
  [stage]
  (doseq [line stage]
    (apply draw-line line)))

(defn collides?
  "Check if `path` intersects with any `stage` walls."
  [stage path]
  (loop [stage stage]
    (when-let [wall (first stage)]
      (or (intersect? wall path)
          (recur (rest stage))))))

(defn move-camera!
  "Handles `camera` movement within `stage`. Accepts `camera` and
  `key-states` atoms."
  [camera key-states stage]
  (let [{x :x y :y :as current-pos} @camera
        states @key-states
        step-size 0.8
        rotate-angle 1.5
        extra-step (+ step-size 1.5)]

    (when (states "Escape")
      (input/release-focus)
      (reset! key-states {}))

    (when (states "ArrowRight")
      (if (states "Shift")
        (let [{x' :x y' :y} (cam/strafe current-pos (- extra-step))]
          (when (not (collides? stage [[x y] [x' y']]))
            (swap! camera cam/strafe (- step-size))))
        (swap! camera cam/rotate (- rotate-angle))))

    (when (states "ArrowLeft")
      (if (states "Shift")
        (let [{x' :x y' :y} (cam/strafe current-pos extra-step)]
          (when (not (collides? stage [[x y] [x' y']]))
            (swap! camera cam/strafe step-size)))
        (swap! camera cam/rotate rotate-angle)))

    (when (states "ArrowUp")
      (let [{x' :x y' :y} (cam/move-forward current-pos extra-step)]
        (when (not (collides? stage [[x y] [x' y']]))
          (swap! camera cam/move-forward step-size))))

    (when (states "ArrowDown")
      (let [{x' :x y' :y} (cam/move-forward current-pos (- extra-step))]
        (when (not (collides? stage [[x y] [x' y']]))
          (swap! cam/camera cam/move-forward (- step-size)))))))

(defn draw-camera
  "Draws `camera` on canvas, specified by `*ctx*`."
  [camera]
  (let [[x y :as pos] [(:x camera) (:y camera)]
        pointer (cam/move-point pos (:degree camera) 7)
        width 5
        color "#000000"]
    (draw-line pos pointer color)
    (set! (. *ctx* -fillStyle) color)
    (. *ctx* fillRect (- x (/ width 2)) (- y (/ width 2)) width width)))

(defn draw-rays
  "Draws `rays` from `camera` on canvas, specified by `*ctx*`."
  [camera rays]
  (let [pos [(:x camera) (:y camera)]]
    (doseq [{ray :end} rays]
      (draw-line pos ray "#ffe680"))))

(defn render
  "Main game loop."
  []
  (move-camera! cam/camera input/key-states stage)
  (when (inside-rectangle @cam/camera stage/goal)
    (swap! cam/camera cam/set-position 12 195 180))
  (let [height (. *canvas* -height)
        width (. *canvas* -width)
        camera @cam/camera
        rays (cast-rays camera stage)]
    (. *ctx* clearRect 0 0 width height)
    (draw-rays camera rays)
    (draw-camera camera)
    (draw-stage stage)
    (draw-3d-stage rays)
    (. js/window requestAnimationFrame render)))

(defn update-ray-count
  "Called when user interaccts with ray count input on the page."
  [event]
  (let [value (.. event -target -value)]
    (set! *ray-count* (js/parseInt value))
    (when-let [output (. js/document getElementById "rayCountOutput")]
      (set! (. output -innerHTML) value))))

(defn update-fov
  "Called when user interaccts with fov input on the page."
  [event]
  (let [value (.. event -target -value)]
    (set! *fov* (js/parseInt value))
    (when-let [output (. js/document getElementById "fovSliderOutput")]
      (set! (. output -innerHTML) value))))

(defn update-fish-eye-compensation
  "Called when user interaccts with fisheye correction input on the page."
  [event]
  (let [value (.. event -target -checked)]
    (set! *compensate-fisheye* value)
    (when-let [fov-slider (. js/document getElementById "fovSlider")]
      (set! (. fov-slider -max) (if value max-compensated-fov max-fov))
      (when (and value
                 (> *fov* max-compensated-fov))
        (set! *fov* max-compensated-fov)
        (set! (. fov-slider -value) max-compensated-fov)
        (when-let [fov-output (. js/document getElementById "fovSliderOutput")]
          (set! (. fov-output -innerHTML) *fov*))))))

(defn init-inputs
  "Set up input handlers and default values."
  []
  (set! (. js/window -onkeyup) input/on-key-release)
  (set! (. js/window -onkeydown) input/on-key-press)

  (when-let [ray-count-slider (. js/document getElementById "rayCountSlider")]
    (set! (. ray-count-slider -value) *ray-count*)
    (. ray-count-slider addEventListener "input" update-ray-count))
  (when-let [ray-count-output (. js/document getElementById "rayCountOutput")]
    (set! (. ray-count-output -innerHTML) *ray-count*))

  (when-let [fov-slider (. js/document getElementById "fovSlider")]
    (. fov-slider addEventListener "input" update-fov)
    (set! (. fov-slider -max) (if *compensate-fisheye* max-compensated-fov max-fov))
    (set! (. fov-slider -value) *fov*))
  (when-let [ray-count-output (. js/document getElementById "fovSliderOutput")]
    (set! (. ray-count-output -innerHTML) *fov*))

  (when-let [compensate (. js/document getElementById "fishEyeCompensation")]
    (. compensate addEventListener "click" update-fish-eye-compensation)
    (set! (. compensate -checked) *compensate-fisheye*)
    (set! (. compensate -value) *compensate-fisheye*)))

(defn ^:export init []
  (init-inputs)
  (when-let [canvas (. js/document getElementById "raycaster")]
    (set! *canvas* canvas)
    (. *canvas* addEventListener "mousedown" input/on-click)
    (swap! cam/camera cam/set-position 12 195 180)
    (when (. *canvas* -getContext)
      (set! *ctx* (. *canvas* getContext "2d"))
      (render))))
