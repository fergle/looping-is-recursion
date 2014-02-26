(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   :else (and (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq-copy a-seq]
    (cond
     (empty? seq-copy) nil
     (pred (first seq-copy)) index
     :else (recur (inc index) (rest seq-copy))
     )))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [index 0
           sum 0
           seq-copy a-seq]
      (cond
       (empty? seq-copy) (/ sum index)
       :else (recur (inc index) (+ sum (first seq-copy)) (rest seq-copy))))))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem))
  )

(defn parity [a-seq]
  (loop [even-set (set a-seq)
         odd-set #{}
         seq-copy a-seq]
    (cond
     (empty? seq-copy) odd-set
     :else (recur (toggle even-set (first seq-copy)) (toggle odd-set (first seq-copy)) (rest seq-copy))
     )))

(defn fast-fibo [n]
  (cond
   (zero? n) 0
   (= n 1) 1
   :else (loop [f-last 1
                f-prev 1
                index (- n 2)]
           (if (zero? index)
             f-last
             (recur (+ f-last f-prev) f-last (dec index))))))


(defn cut-at-repetition [a-seq]
  (loop [processed-set #{}
         return-seq []
         copy-seq a-seq]
    (cond
     (or (empty? copy-seq) (contains? processed-set (first copy-seq))) return-seq
     :else (recur (conj processed-set (first copy-seq)) (conj return-seq (first copy-seq)) (rest copy-seq)))))

