(use ../support)

(defn parse [input]
    (def h (length input))
    (def w (length (input 0)))
    (def obs @{})
    (var start nil)
    (eachp [y line] input
        (eachp [x char] line
            (case char
                (chr "#") (put obs [x y] true)
                (chr "^") (set start [x y]))))
    (struct
        :w w
        :h h
        :obs obs
        :guard-pos start
        :guard-dir [0 -1]
        :seen @{start true}))

(defn turn [dir]
    (case dir
        [ 0 -1] [ 1  0]   # north -> east
        [ 1  0] [ 0  1]   # east -> south
        [ 0  1] [-1  0]   # south -> west
        [-1  0] [ 0 -1])) # west -> north

(defn move [pos dir]
    [(+ (pos 0) (dir 0)) (+ (pos 1) (dir 1))])

(defn within-bounds [state pos]
    (def [x y] pos)
    (and
        (<= 0 x) (< x (state :w))
        (<= 0 y) (< y (state :h))))

(defn update-seen [state pos]
    (when (within-bounds state pos)
        (put (state :seen) pos true)))

(defn tick [state]
    (def next-pos (move (state :guard-pos) (state :guard-dir)))
    (if (gett state :obs next-pos)
        (struct/update-with state :guard-dir turn)
        (do
            (update-seen state next-pos)
            (struct/update state :guard-pos next-pos))))

(defn play [state]
    (-> state
        (update-state
            |(tick $)
            |(within-bounds $0 ($0 :guard-pos)))
        (get :seen)
        (length)))

(defn dir-symbol [dir]
    (case dir
        [ 0 -1] "^"
        [ 1  0] ">"
        [ 0  1] "v"
        [-1  0] "<"))

(defn print-grid [state]
    (def buf @"")
    (for y 0 (state :h)
        (for x 0 (state :w)
            (def pos [x y])
            (def char
                (cond
                    (= pos (state :guard-pos)) (dir-symbol (state :guard-dir))
                    (gett state :obs pos) "#"
                    "."))
            (buffer/push-string buf char))
        (buffer/push-string buf "\n"))
    buf)

(defn debug-tick-n [state n]
    (var n n)
    (-> state
        (update-state
            |(tick $)
            |(> (-- n) 0))
        (print-grid)
        (print)))
