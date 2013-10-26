(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [res orig n]
                 (if (>= 1 n)
                     res
                     (recur (* res orig ) orig (dec n))))]
    (cond
     (zero? base) 0
     (zero? exp) 1
     :else (helper base base exp))
    ))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (= 1 (count a-seq))
                   (first a-seq)
                   (recur (rest a-seq))
                   ))]
    (if (empty? a-seq)
    nil
    (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                  (and (empty? seq1)
                       (empty? seq2)) true
                  (= seq1 seq2)
                   (recur (rest seq1)
                          (rest seq2))
                  :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         sek a-seq]
    (cond
     (empty? sek) nil
     (pred (first sek)) index
     :else (recur (inc index)
                  (rest sek)))))

(defn avg [a-seq]
  (loop [cnt 0                                             ;=> GOOD THING THIS ISN'T TESTED
         sum 0
         sek a-seq]
    (if (empty? sek)
      (/ sum cnt)
      (recur (inc cnt)
             (+ sum (first sek))
             (rest sek)))))

(defn parity [a-seq]
  (loop [finalseq []
         sek a-seq]
    (if (empty? sek)
      finalseq
      (recur (if (some #(= (first sek) %) finalseq)
               (remove #(= (first sek) %) finalseq)
               (conj finalseq (first sek))) (rest sek))
        )))

(defn fast-fibo [n]
  (loop [first 0
         second 1
         cnt 0]
    (if (= cnt n)
      first
      (recur second (+ first second) (inc cnt)))))

(defn cut-at-repetition [a-seq]
  (loop [finalseq []
         sek a-seq]
    (if (or (some #(= (first sek) %) finalseq) (empty? sek))
      finalseq
      (recur (conj finalseq
                   (first sek))
             (rest sek)))))

