(defn input [text]
    (string/split "\n" text))

(defmacro gett [ds & keys]
    (reduce (fn [t key] (tuple 'get t key)) ds keys))

(defn struct/update [ds & pairs]
    (def t (struct/to-table ds))
    (forv i 0 (length pairs)
        (def k (pairs i))
        (++ i)
        (def v (pairs i))
        (put t k v))
    (table/to-struct t))

(defn struct/update-with [ds key fn]
    (def t (struct/to-table ds))
    (update t key fn)
    (table/to-struct t))

(defn update-state [initial update continue]
    (var state initial)
    (while (continue state)
        (->> state
            (update)
            (set state)))
    state)

