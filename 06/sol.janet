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
    [{:w w :h h :obs obs} [start [0 -1]]])

(defn turn [dir]
    (case dir
        [ 0 -1] [ 1  0]   # north -> east
        [ 1  0] [ 0  1]   # east -> south
        [ 0  1] [-1  0]   # south -> west
        [-1  0] [ 0 -1])) # west -> north

(defn move [pos dir]
    [(+ (pos 0) (dir 0)) (+ (pos 1) (dir 1))])

(defn tick [grid guard]
    (def [pos dir] guard)
    (def next (move pos dir))
    (if (gett grid :obs next)
        [grid [pos (turn dir)]]
        [grid [next dir]]))
