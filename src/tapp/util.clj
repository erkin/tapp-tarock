(ns tapp.util)

(defn toss-coin?
  "Return `true` 50% of the time, `false` 50% of the time.
  Probably."
  []
  (zero? (Math/round (rand))))

(defn best
  "Find the element that satisfies `pred?` over everything else in `coll`."
  [pred? coll]
  (reduce
   (fn [best current]
     (if (pred? best current)
       best
       current))
   coll))

(defn listed>?
  "Use `list` as a reference (assumed to be higher to lower)
  to find the greater of `a` and `b`."
  [a b list]
  (loop [l (reverse list)]
    (cond
      (= b (first l)) false
      (= a (first l)) true
      :else (recur (rest l)))))

(defn remove-from
  "Drop elements in `coll` that are equal to `element`."
  [coll element]
  (remove (partial = element) coll))
