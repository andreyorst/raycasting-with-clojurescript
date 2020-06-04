(ns raycasting.macros)

(defmacro three-decimal
  "Truncate `double` to three decimal places."
  [num]
  `(/ (int (* 1000 ~num)) 1000.0))

(defmacro binding*
  "Sequential binding."
  [bindings & body]
  (reduce (fn [acc [x y]]
            `(binding [~x ~y] ~acc))
          `(do ~@body)
          (reverse (partition 2 bindings))))
