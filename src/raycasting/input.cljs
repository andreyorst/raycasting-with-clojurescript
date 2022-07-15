(ns raycasting.input)

(defonce key-states (atom {}))
(defonce focus false)

(defn valid-key?
  "Checks if key string is one of keys that we're using."
  [key]
  (or (= key "Escape")
      (= key "Shift")
      (= key "ArrowLeft")
      (= key "ArrowUp")
      (= key "ArrowRight")
      (= key "ArrowDown")))

(defn on-key-press
  "Updates key state when key is pressed."
  [event]
  (when focus
    (let [event (if event event (.-event js/window))
          key (.-key event)]
      (when (valid-key? key)
        (.preventDefault event)
        (swap! key-states assoc key true)))))

(defn on-key-release
  "Updates key state when key is released."
  [event]
  (when focus
    (let [event (if event event (.-event js/window))
          key (.-key event)]
      (when (valid-key? key)
        (swap! key-states assoc key false)
        (.preventDefault event)))))

;; Hanling focus

(defn on-click [_event]
  (set! focus true))

(defn release-focus []
  (set! focus false))
