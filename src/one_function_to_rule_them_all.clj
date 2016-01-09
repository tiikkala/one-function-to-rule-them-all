(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [i v] (str i " " v)) a-seq)))

(defn my-interpose [x a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq) 0)
  (let [counter (fn [count e]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce #(conj %1 %2) (list) a-seq))

(defn min-max-element [a-seq]
  (let [helper
        (fn [[mi ma] v]
          [(min mi v) (max ma v)])]
    (reduce helper [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (loop [l nil
         r []
         s sorted-seq]
    (cond
      (empty? s) (conj r n)
      (and (nil? l) (<= n (first s))) (concat (conj r n) s)
      (and (not (nil? l)) (<= l n (first s))) (concat (conj r n) s)
      :else (recur (first s) (conj r (first s)) (rest s)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [helper (fn [s v]
                 (if (contains? s v) (disj s v) (conj s v)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& vars]
  (count vars))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and [& more]
  (fn [x]
    ((fn [x preds]
    (if (empty? preds)
      true
      (if ((first preds) x)
        (recur x (rest preds))
        false)))
     x more)))

(defn my-map1 [f a-seq]
  (reduce (fn [a b] (conj a (f b))) [] a-seq)
  )


(defn reducen [f initial seqs]
  (if (empty? (first seqs))
    initial
    (recur f
           (conj initial (apply f (my-map1 first seqs)))
           (my-map1 rest seqs))))

(defn my-map [f & seqs]
  (reducen f [] seqs))
