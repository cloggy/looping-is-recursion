(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [prod base exp]
                 (if (zero? exp)
                   prod
                   (recur (* prod base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [cur a-seq]
                 (if-not (seq a-seq)
                   cur
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [s1 (first seq1)
        s2 (first seq2)]
    (if (and (empty? seq1) (empty? seq2))
      true
      (if (or (empty? seq1) (empty? seq2))
        false
        (if (not= s1 s2)
          false
          (recur (rest seq1) (rest seq2)))))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) idx
     :else (recur (inc idx) (rest s)))))

(defn avg [a-seq]
  (loop [total 0
         count 0
         s a-seq]
    (if-not (seq s)
      (if (<= count 0)
        nil
        (/ total count))
      (recur (+ total (first s)) (inc count) (rest s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [odds #{}
           s a-seq]
      (if (seq s)
        (recur (toggle odds (first s)) (rest s))
        odds))))

(defn fast-fibo [n]
  (loop [m-1 0
         m 1
         n n]
    (if (zero? n)
      m-1
     (recur m (+ m-1 m) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [s #{}
         se a-seq
         acc []]
    (if (contains? s (first se))
      acc
      (if (seq se)
        (recur (conj s (first se)) (rest se) (conj acc (first se)))
        acc))))

