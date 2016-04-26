(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                  (if (zero? k)
                    acc
                    (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last coll]
                  (if (empty? coll)
                    last
                    (recur (first coll) (rest coll))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                    (cond
                      (and (empty? a-seq) (empty? b-seq))
                        true
                      (or (empty? a-seq) (empty? b-seq))
                        false
                      (= (first a-seq) (first b-seq))
                        (recur (rest a-seq) (rest b-seq))
                      :else
                        false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         predicate pred
         seq a-seq]
    (cond
      (empty? seq)
        nil
      (pred (first seq))
        index
      :else
        (recur (inc index) predicate (rest seq)))))

(defn avg [a-seq]
  (loop [seq a-seq
         n 0
         sum 0]
    (if (empty? seq)
      (/ sum n)
      (recur (rest seq) (inc n) (+ sum (first seq))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                  (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))]
    (loop [seq a-seq
           set #{}]
      (if (empty? seq)
        set
        (recur (rest seq) (toggle set (first seq)))))))

(defn fast-fibo [n]
  (loop [f-n-1 0
         f-n 1
         k 1]
    (cond
      (== n 0)
        0
      (== k n)
        f-n
      :else
        (recur f-n (+ f-n-1 f-n) (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [set #{}
         keep-seq []
         seq a-seq]
    (cond
      (empty? seq)
        keep-seq
      (contains? set (first seq))
        keep-seq
      :else
        (recur (conj set (first seq))
               (conj keep-seq (first seq))
               (rest seq)))))
