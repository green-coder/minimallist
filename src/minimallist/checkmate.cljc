(ns minimallist.checkmate
  (:require [minimallist.helper :as h])
  (:import [java.util Random]))

(defn random-generator
  ([] (Random.))
  ([seed] (Random. seed)))

(defn derive-random-generator! [rg]
  (random-generator (.nextLong rg)))

(comment
  (let [r1 (random-generator 0)
        r2 (derive-random-generator! r1)]
    (prn (nextInt r1))
    (prn (nextInt r1))
    (prn (nextInt r1))
    (prn (nextInt r2))
    (prn (nextInt r2))
    (prn (nextInt r2))))

(defn nextBoolean [rg]
  (.nextBoolean rg))

(defn nextInt
  ([rg]
   (.nextInt rg))
  ([rg sup]
   (.nextInt rg sup))
  ([rg min sup]
   (+ min (.nextInt rg (- sup min)))))

(defn nextDouble [rg]
  (.nextDouble rg))

(defn abs [x]
  (max x (- x)))

(defn nextPosGaussianDouble
  "Positive double number from a normal distribution of mean 0 and standard deviation of 1."
  ([rgen] (abs (.nextGaussian rgen))))

(defn nextPosGaussianInt
  ([rgen sup]
   (let [max (dec sup)]
     (-> (nextPosGaussianDouble rgen)
         (/ 3.0) ;; values are rarely over 3.0
         (* max)
         (int)
         (clojure.core/min max))))
  ([rgen min sup]
   (-> (nextPosGaussianInt rgen (- sup min))
       (+ min))))

(comment
  (sort > (repeatedly 20 #(nextPosGaussianInt (random-generator) 100))))

(defn n-split [rgen sum n-parts]
  (case n-parts
    0 []
    1 [sum]
    (let [parts (repeatedly n-parts #(nextPosGaussianDouble rgen))
          factor (/ sum (reduce + parts))]
      (into [] (map (fn [x]
                      (-> x (* factor) (int))))
            parts))))

;; Note:
;; - This distribution is more unbalanced,
;; - big numbers are more likely to be in front.
(defn n-split-weird [rgen sum n-parts]
  (if (zero? n-parts)
    []
    (loop [n n-parts
           sum sum
           result []]
      (if (= n 1)
        (conj result sum)
        (let [part (nextPosGaussianInt rgen sum)]
          (recur (dec n)
                 (- sum part)
                 (conj result part)))))))

(comment
  (n-split (random-generator) 100 10)
  (n-split-weird (random-generator) 100 10))



(comment
  (defn bind [gen gen-fn shrinks-fn])

  (defn my-shrinks [output input smaller-input]
    (if (< smaller-input (dec input))
      '()
      (let [size (count output)]
        (map (fn [index]
               (into (subvec output 0 index)
                     (subvec output (inc index) size)))
             (range size)))))

  (my-shrinks [1 2 3] 3 2)

  (gen/bind small-int
            (fn [width]
              (gen/vector gen/nat width))
            (fn [output input smaller-input]
              (if (< small-input (dec input))
                '()
                (let [size (count output)]
                  (map (fn [index]
                         (into (subvec output 0 index)
                               (subvec output (inc index) size)))
                       (range size)))))))

(defn generate [rgen options context model]
  (if-let [generator (:gen model)]
    (generator rgen options)
    (case (:type model)
      ; :fn should have its own generator

      :enum
      (let [index (nextInt rgen 0 (count (:values model)))
            values (seq (:values model))]
        (nth values index))

      :sequence-of
      (let [n-elements (if (contains? model :count-model)
                         (generate (derive-random-generator! rgen)
                                   options
                                   context
                                   (:count-model model))
                         (nextInt rgen 0 (:size options 10)))]
        (into []
              (map (fn [rgen]
                     (generate rgen options context (:elements-model model))))
              (repeatedly n-elements #(derive-random-generator! rgen)))))))

(comment
  (repeatedly 20 #(generate (random-generator) {} {}
                            (h/vector-of (h/enum #{:a 33 "b" [1 2] true}))))

  (repeatedly 20 #(generate (random-generator) {} {}
                            (h/sequence-of (h/fn {:gen (fn [rgen options]
                                                         (nextInt rgen 0 100))}
                                                 int?)))))


(defn row [parent-rgen width]
  (into []
        (map (fn [rgen]
               (nextInt rgen 0 100)))
        (repeatedly width #(derive-random-generator! parent-rgen))))

(row (random-generator) 10)

; shrink width, shrink elements
; problem:

; shrink-bool: the other boolean value
; shrink-int: smaller ints
; shrink-text: texts with some parts removed
; shrink-coll: collections with parts removed
;   - vector: remove 1 element, starting from the end
;   - list: remove 1 element, starting from the beginning
;   - set: remove 1 element, no specific order
;   -

(defn boolean-shrinks [b]
  (when b '(false)))

(defn int-shrinks [n]
  (cond
    (= n 0) '()
    (or (= n 1) (= n -1)) '(0)
    :else (let [half (quot n 2)]
            (list* 0 half
                   (->> (range (dec (abs n)) 0 -1)
                        (map (if (> n 0) identity #(- %)))
                        (remove #{half}))))))

(comment
  (int-shrinks 6)
  (int-shrinks -6))

(defn string-shrinks [s]
  (let [c (count s)]
    (mapcat (fn [n]
              (if (zero? n)
                [""]
                [(subs s 0 n)
                 (subs s (- c n))]))
            (range (dec c) -1 -1))))

(comment
  (string-shrinks "1234"))

(defn vector-shrinks [v]
  (let [c (count v)]
   (mapcat (fn [n]
             (if (zero? n)
               [[]]
               [(subvec v 0 n)
                (subvec v (- c n))]))
           (range (dec c) -1 -1))))

(comment
  (vector-shrinks [1 2 3 4]))


(defn for-all [gen predicate n-tries rgen])
