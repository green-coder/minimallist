(ns minimallist.generator
  (:require [minimallist.core :refer [valid?]]
            [minimallist.util :refer [reduce-update reduce-update-in reduce-mapv]]
            [clojure.test.check.generators :as gen]))


(defn- find-stack-index [stack key]
  (loop [index (dec (count stack))
         elements (rseq stack)]
    (when elements
      (let [elm (first elements)]
        (if (contains? (:bindings elm) key)
          index
          (recur (dec index) (next elements)))))))

;; TODO: walk on :count-model and :condition-model nodes
(defn postwalk [model visitor]
  (let [walk (fn walk [[stack walked-bindings] model path]
               (let [[[stack walked-bindings] model]
                     (case (:type model)
                       (:fn :enum) [[stack walked-bindings] model]
                       (:set-of :sequence-of
                        :repeat) (-> [[stack walked-bindings] model]
                                     (reduce-update :elements-model walk (conj path :elements-model)))
                       :map-of (-> [[stack walked-bindings] model]
                                   (reduce-update-in [:keys :model] walk (conj path :keys :model))
                                   (reduce-update-in [:values :model] walk (conj path :values :model)))
                       (:and :or
                        :map :sequence
                        :alt :cat) (cond-> [[stack walked-bindings] model]
                                     (contains? model :entries)
                                     (reduce-update :entries (fn [[stack walked-bindings] entries]
                                                               (reduce-mapv (fn [[stack walked-bindings] [index entry]]
                                                                              (reduce-update [[stack walked-bindings] entry] :model
                                                                                             walk (conj path :entries index :model)))
                                                                            [stack walked-bindings]
                                                                            (map-indexed vector entries)))))
                       :let (let [[[stack' walked-bindings'] walked-body] (walk [(conj stack {:bindings (:bindings model)
                                                                                              :path (conj path :bindings)})
                                                                                 walked-bindings]
                                                                                (:body model)
                                                                                (conj path :body))]
                              [[(pop stack') walked-bindings'] (assoc model
                                                                 :bindings (:bindings (peek stack'))
                                                                 :body walked-body)])
                       :ref (let [key (:key model)
                                  index (find-stack-index stack key)
                                  binding-path (conj (get-in stack [index :path]) key)]
                              (if (contains? walked-bindings binding-path)
                                [[stack walked-bindings] model]
                                (let [[[stack' walked-bindings'] walked-ref-model] (walk [(subvec stack 0 (inc index))
                                                                                          (conj walked-bindings binding-path)]
                                                                                         (get-in stack [index :bindings key])
                                                                                         binding-path)]
                                  [[(-> stack'
                                        (assoc-in [index :bindings key] walked-ref-model)
                                        (into (subvec stack (inc index)))) walked-bindings'] model]))))]
                 [[stack walked-bindings] (visitor model stack path)]))]
    (second (walk [[] #{}] model []))))


(defn- assoc-leaf-distance-disjunction [model distances]
  (let [non-nil-distances (filter some? distances)]
    (cond-> model
      (seq non-nil-distances)
      (assoc :leaf-distance (inc (reduce min non-nil-distances))))))

(defn- assoc-leaf-distance-conjunction [model distances]
  (cond-> model
    (every? some? distances)
    (assoc :leaf-distance (inc (reduce max 0 distances)))))

(defn assoc-leaf-distance-visitor [model stack path]
  ;(prn path)
  (case (:type model)
    (:fn :enum) (assoc model :leaf-distance 0)
    :map-of (assoc-leaf-distance-conjunction model
                                             [(-> model :keys :model :leaf-distance)
                                              (-> model :values :model :leaf-distance)])
    (:set-of
     :sequence-of
     :repeat) (assoc-leaf-distance-conjunction model
                                               [(-> model :elements-model :leaf-distance)])
    (:or
     :alt) (assoc-leaf-distance-disjunction model
                                            (mapv (comp :leaf-distance :model)
                                                  (:entries model)))
    (:and
     :map
     :sequence
     :cat) (assoc-leaf-distance-conjunction model
                                            (->> (:entries model)
                                                 (remove :optional)
                                                 (mapv (comp :leaf-distance :model))))
    :let (let [body-distance (:leaf-distance (:body model))]
           (cond-> model
             (some? body-distance) (assoc :leaf-distance (inc body-distance))))
    :ref (let [key (:key model)
               index (find-stack-index stack key)
               binding-distance (get-in stack [index :bindings key :leaf-distance])]
           (cond-> model
             (some? binding-distance) (assoc :leaf-distance (inc binding-distance))))))



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
          ;; TODO: avoid choosing a model that cannot be generated.
          ;; TODO: choose specific ones when running out of budget.
          :alt (gen/let [entry (gen/elements (:entries model))]
                 (sequence-generator context (:model entry)))
          :cat (->> (apply gen/tuple (mapv (fn [entry]
                                             (sequence-generator context (:model entry)))
                                           (:entries model)))
                    (gen/fmap (fn [xs] (into [] cat xs))))
          ;; TODO: choose n-repeat according to the budget.
          :repeat (gen/let [n-repeat (gen/choose (:min model) (:max model))
                            sequences (gen/vector (sequence-generator context (:elements-model model))
                                                  n-repeat)]
                    (into [] cat sequences))
          :let (sequence-generator (merge context (:bindings model)) (:body model))
          :ref (sequence-generator context (get context (:key model)))))
    (gen/fmap vector (generator context model))))

(defn generator
  "Returns a test.check generator derived from the model."
  ([model]
   (generator {} model))
  ([context model]
   (or (:test.check/generator model)
       (case (:type model)

         :fn nil ;; a generator is supposed to be provided for those nodes

         :enum (gen/elements (:values model))

         (:and :or) nil ;; a generator is supposed to be provided for those nodes

         ;; TODO: avoid choosing a model that cannot be generated.
         ;; TODO: choose specific ones when running out of budget.
         :alt (let [entries (:entries model)]
                (gen/let [index (gen/choose 0 (dec (count entries)))]
                  (generator context (:model (entries index)))))

         ;; TODO: choose a count according to the budget when :count-model is not specified.
         ;; Problem: we don't know how to pick a small value when :count-model is specified.
         ;;          we could handle the simple case where :count-model is just an enum.
         :set-of (let [element-generator (if (contains? model :elements-model)
                                           (generator context (:elements-model model))
                                           gen/any)]
                   (cond->> (if (contains? model :count-model)
                              (gen/bind (generator context (:count-model model))
                                        (fn [num-elements]
                                          (gen/set element-generator {:num-elements num-elements})))
                              (gen/set element-generator))
                     (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model)))))

         ;; TODO: avoid choosing optional keys that cannot be generated.
         ;; TODO: avoid optional entries when running out of budget.
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

         ;; TODO: solve the scary possible infinite recursion problems
         :ref (generator context (get context (:key model)))))))

