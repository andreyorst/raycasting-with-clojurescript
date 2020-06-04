(ns raycasting.stage)

(def stage [[[67.5 157.5] [67.5 90] "#7372a8"]
            [[45 67.5] [45 0] "#7372a8"]
            [[22.5 157.5] [67.5 157.5] "#7372a8"]
            [[22.5 90] [67.5 90] "#7372a8"]
            [[0 135] [45 135] "#7372a8"]
            [[22.5 90] [22.5 22.5] "#7372ff"]
            [[67.5 22.5] [67.5 67.5] "#7372a8"]
            [[157.5 22.5] [67.5 22.5] "#7372a8"]
            [[90 157.5] [135 157.5] "#7372a8"]
            [[135 112.5] [135 157.5] "#7372a8"]
            [[90 112.5] [135 112.5] "#7372a8"]
            [[90 22.5] [90 112.5] "#7372a8"]
            [[45 180] [157.5 180] "#7372a8"]
            [[157.5 45] [157.5 180] "#7372a8"]
            [[112.5 45] [157.5 45] "#73f2a8"]
            [[112.5 90] [157.5 90] "#7372a8"]
            [[112.5 67.5] [112.5 90] "#7372a8"]
            [[135 67.5] [112.5 67.5] "#737fa8"]
            [[67.5 135] [112.5 135] "#7372a8"]
            [[45 135] [45 112.5] "#ff72a8"]
            [[0 112.5] [22.5 112.5] "#7372a8"]
            [[22.5 157.5] [22.5 202.5] "#73f0a8"]
            [[45 180] [45 202.5] "#7372a8"]
            [[183 22.5] [183 202.5] "#7372a8"]
            [[0 0] [0 202.5] "#7372a8"]
            [[183 202.5] [0 202.5] "#7372a8"]
            [[0 0] [157.5 0] "#7372a8"]

            [[157.5 0] [183 0] "#ffde00"]
            [[183 22.5] [183 0] "#ffde00"]
            [[157.5 0] [157.5 22.5] "#ffde00"]])

(def goal [[157.5 0] [183 0] [183 22.5] [157.5 22.5]])