(defn input [text]
    (string/split "\n" text))

(defmacro gett [ds & keys]
    (reduce (fn [t key] (tuple 'get t key)) ds keys))
