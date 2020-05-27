(ns minimallist.generator
  (:require [minimallist.core :refer [valid?]]
            [clojure.test.check.generators :as gen]))


;; Temporally move this here, will be used later
(comment

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

  (sort > (repeatedly 20 #(nextPosGaussianInt (random-generator) 100)))

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

  (n-split (random-generator) 100 10)
  (n-split-weird (random-generator) 100 10))





;; TODO: What if ... conditions could only exist as something else's :condition-model?
;;       conditions would then only be used for validity testing, not generation.
;; TODO: add :condition-model to :fn nodes and maybe others (all of them?)

(declare generator)

(defn- sequence-generator
  "Returns a generator of a sequence."
  [context model]
  (if (and (#{:alt :cat :repeat :let :ref} (:type model))
           (:inlined model true))
    (or (:test.check/generator model)
        (case (:type model)
          :alt (gen/let [entry (gen/elements (:entries model))]
                 (sequence-generator context (:model entry)))
          :cat (->> (apply gen/tuple (mapv (fn [entry]
                                             (sequence-generator context (:model entry)))
                                           (:entries model)))
                    (gen/fmap (fn [xs] (into [] cat xs))))
          :repeat (gen/let [n-repeat (gen/choose (:min model) (:max model))
                            sequences (gen/vector (sequence-generator context (:elements-model model))
                                                  n-repeat)]
                    (into [] cat sequences))
          :let (sequence-generator (merge context (:bindings model)) (:body model))
          :ref (sequence-generator context (get context (:key model)))))
    (gen/fmap vector (generator context model))))

(defn generator
  "Returns a generator derived from the model."
  ([model]
   (generator {} model))
  ([context model]
   (or (:test.check/generator model)
       (case (:type model)

         :fn nil ;; a generator is supposed to be provided for those nodes

         :enum (gen/elements (:values model))

         (:and :or) nil ;; a generator is supposed to be provided for those nodes

         :alt (let [entries (:entries model)]
                (gen/let [index (gen/choose 0 (dec (count entries)))]
                  (generator context (:model (entries index)))))

         (:set-of :set) (let [element-generator (if (contains? model :elements-model)
                                                  (generator context (:elements-model model))
                                                  gen/any)]
                          (cond->> (if (contains? model :count-model)
                                     (gen/bind (generator context (:count-model model))
                                               (fn [num-elements]
                                                 (gen/set element-generator {:num-elements num-elements})))
                                     (gen/set element-generator))
                            (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model)))))

         (:map-of :map) (cond->> (if (contains? model :entries)
                                   (gen/bind (gen/vector gen/boolean (count (:entries model)))
                                             (fn [random-bools]
                                               (->> (map (fn [entry included?]
                                                           (when (or (not (:optional entry)) included?)
                                                             [(:key entry) (generator context (:model entry))]))
                                                         (:entries model) random-bools)
                                                    (filter some?)
                                                    (apply concat)
                                                    (apply gen/hash-map))))
                                   ;; Maybe consider supporting :count-model for generators
                                   (gen/map (generator context (-> model :keys :model))
                                            (generator context (-> model :values :model))))
                          (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))

         (:sequence-of :sequence) (cond->> (gen/bind gen/boolean
                                                     (fn [random-bool]
                                                       (let [gen (if (contains? model :entries)
                                                                   (apply gen/tuple (map (fn [entry]
                                                                                           (generator context (:model entry)))
                                                                                         (:entries model)))
                                                                   (let [elements-gen (generator context (:elements-model model))]
                                                                     (if (contains? model :count-model)
                                                                       (gen/bind (generator context (:count-model model))
                                                                                 (fn [count] (gen/vector elements-gen count)))
                                                                       (gen/vector elements-gen))))]
                                                         (let [inside-list? (case (:coll-type model)
                                                                              :list true
                                                                              :vector false
                                                                              random-bool)]
                                                           (cond->> gen
                                                             inside-list? (gen/fmap (partial apply list)))))))
                                    (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))

         ;; TODO: enforce :count-model when specified
         (:cat :repeat) (cond->> (gen/bind gen/boolean
                                           (fn [random-bool]
                                             (let [gen (sequence-generator context model)]
                                               (let [inside-list? (case (:coll-type model)
                                                                    :list true
                                                                    :vector false
                                                                    random-bool)]
                                                 (cond->> gen
                                                   inside-list? (gen/fmap (partial apply list)))))))
                                 (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))

         :let (generator (merge context (:bindings model)) (:body model))

         ;; scary possible infinite recursion problems
         :ref (generator context (get context (:key model)))))))

