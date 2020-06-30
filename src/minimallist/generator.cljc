(ns minimallist.generator
  (:require [minimallist.core :refer [valid?]]
            [minimallist.util :refer [reduce-update reduce-update-in reduce-mapv] :as util]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :as random]))


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
                        :repeat) (cond-> [[stack walked-bindings] model]
                                   (contains? model :elements-model)
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


(defn- min-count-value [model]
  (let [count-model (:count-model model)]
    (if (nil? count-model)
      0
      (case (:type count-model)
        :enum (let [values (filter number? (:values count-model))]
                (when (seq values)
                  (apply min values)))
        :fn (:min-value count-model)
        nil))))

(defn assoc-leaf-distance-visitor [model stack path]
  (let [distance (case (:type model)
                   (:fn :enum) 0
                   :map-of (let [key-distance (-> model :keys :model ::leaf-distance)
                                 value-distance (-> model :values :model ::leaf-distance)]
                             (cond
                               (zero? (min-count-value model)) 0
                               (and key-distance value-distance) (inc (max key-distance value-distance))))
                   (:set-of
                    :sequence-of
                    :repeat) (if (or (not (contains? model :elements-model))
                                     (zero? (min-count-value model))
                                     (zero? (:min model)))
                               0
                               (some-> (-> model :elements-model ::leaf-distance) inc))
                   (:or
                    :alt) (let [distances (->> (:entries model)
                                               (map (comp ::leaf-distance :model))
                                               (remove nil?))]
                            (when (seq distances)
                              (inc (reduce min distances))))
                   (:and
                    :map
                    :sequence
                    :cat) (let [distances (->> (:entries model)
                                               (remove :optional)
                                               (map (comp ::leaf-distance :model)))]
                            (when (every? some? distances)
                              (inc (reduce max 0 distances))))
                   :let (some-> (-> model :body ::leaf-distance) inc)
                   :ref (let [key (:key model)
                              index (find-stack-index stack key)
                              binding-distance (get-in stack [index :bindings key ::leaf-distance])]
                          (some-> binding-distance inc)))]
    (cond-> model
      (some? distance) (assoc ::leaf-distance distance))))

(defn assoc-min-cost-visitor [model stack path]
  (let [type (:type model)
        min-cost (case type
                   (:fn :enum) (::min-cost model 1)
                   :map-of (let [container-cost 1
                                 min-count (min-count-value model)
                                 key-min-cost (-> model :keys :model ::min-cost)
                                 value-min-cost (-> model :values :model ::min-cost)
                                 content-cost (if (zero? min-count) 0
                                                (when (and min-count key-min-cost value-min-cost)
                                                  (* min-count (+ key-min-cost value-min-cost))))]
                             (some-> content-cost (+ container-cost)))
                   (:set-of
                    :sequence-of
                    :repeat) (let [container-cost (if (#{:set-of :sequence-of} type) 1 0)
                                   min-count (min-count-value model)
                                   elements-model (:elements-model model)
                                   elements-model-min-cost (if elements-model
                                                             (::min-cost elements-model)
                                                             1) ; the elements could be anything
                                   content-cost (if (zero? min-count) 0
                                                  (when (and min-count elements-model-min-cost)
                                                    (* elements-model-min-cost min-count)))]
                               (some-> content-cost (+ container-cost)))
                   (:or
                    :alt) (let [existing-vals (->> (:entries model)
                                                   (map (comp ::min-cost :model))
                                                   (filter some?))]
                            (when (seq existing-vals)
                              (reduce min existing-vals)))
                   :and (let [vals (map (comp ::min-cost :model) (:entries model))]
                          (when (and (seq vals) (every? some? vals))
                            (reduce max vals)))
                   (:map
                    :sequence
                    :cat) (let [container-cost (if (or (#{:map :sequence} type)
                                                       (:coll-type model)
                                                       (not (:inlined model true)))
                                                 1 0)
                                vals (->> (:entries model)
                                          (remove :optional)
                                          (map (comp ::min-cost :model)))
                                content-cost (when (every? some? vals) (reduce + vals))]
                            (some-> content-cost (+ container-cost)))
                   :let (::min-cost (:body model))
                   :ref (let [key (:key model)
                              index (find-stack-index stack key)]
                          (get-in stack [index :bindings key ::min-cost])))]
    (cond-> model
      (some? min-cost) (assoc ::min-cost min-cost))))


(defn- preferably-such-that
  "A generator that tries to generate values satisfying a given predicate,
   but won't throw an tantrum if it can't."
  ([pred gen]
   (preferably-such-that pred gen 10))
  ([pred gen max-tries]
   (#'gen/make-gen (fn [rng size]
                     (loop [tries-left max-tries
                            rng rng
                            size size]
                       (if (zero? tries-left)
                         (gen/call-gen gen rng size)
                         (let [[r1 r2] (random/split rng)
                               value (gen/call-gen gen r1 size)]
                           (if (pred (rose/root value))
                             (rose/filter pred value)
                             (recur (dec tries-left) r2 (inc size))))))))))

(defn- budget-split
  "Returns a generator which generates budget splits."
  [budget min-costs]
  (if (seq min-costs)
    (let [nb-elements (count min-costs)
          min-costs-sum (reduce + min-costs)
          budget-minus-min-costs (max 0 (- budget min-costs-sum))]
      (gen/fmap (fn [rates]
                  (let [budget-factor (/ budget-minus-min-costs (reduce + rates))]
                    (mapv (fn [min-cost rate]
                            (+ min-cost (* rate budget-factor)))
                          min-costs
                          rates)))
                (gen/vector (gen/choose 1 100) nb-elements)))
    (gen/return [])))


;; TODO: What if ... conditions could only exist as something else's :condition-model?
;;       conditions would then only be used for validity testing, not generation.
;; TODO: add :condition-model to :fn nodes and maybe others (all of them?)

(declare generator)

(defn- sequence-generator
  "Returns a generator of a sequence."
  [context model budget]
  (if (and (#{:alt :cat :repeat :let :ref} (:type model))
           (:inlined model true))
    (or (:test.check/generator model)
        (case (:type model)
          ;; TODO: avoid choosing a model that cannot be generated.
          ;; TODO: choose specific ones when running out of budget.
          :alt (gen/let [entry (gen/elements (:entries model))]
                 (sequence-generator context (:model entry) budget))
          :cat (->> (apply gen/tuple (mapv (fn [entry]
                                             (sequence-generator context (:model entry) budget))
                                           (:entries model)))
                    (gen/fmap (fn [xs] (into [] cat xs))))
          ;; TODO: choose n-repeat according to the budget.
          :repeat (gen/let [n-repeat (gen/choose (:min model) (:max model))
                            sequences (gen/vector (sequence-generator context (:elements-model model) budget)
                                                  n-repeat)]
                    (into [] cat sequences))
          :let (sequence-generator (merge context (:bindings model)) (:body model) budget)
          :ref (sequence-generator context (get context (:key model)) budget)))
    (gen/fmap vector (generator context model budget))))

(defn- generator
  "Returns a generator of a data structure."
  [context model budget]
  (or (:test.check/generator model)
      (case (:type model)

        :fn nil ;; a generator is supposed to be provided for those nodes

        :enum (gen/elements (:values model))

        (:and :or) nil ;; a generator is supposed to be provided for those nodes

        ;; TODO: avoid choosing a model that cannot be generated.
        ;; TODO: choose specific ones when running out of budget.
        :alt (let [entries (:entries model)]
               (gen/let [index (gen/choose 0 (dec (count entries)))]
                 (generator context (:model (entries index)) budget)))

        ;; TODO: choose a count according to the budget when :count-model is not specified.
        ;; Problem: we don't know how to pick a small value when :count-model is specified.
        ;;          we could handle the simple case where :count-model is just an enum.
        :set-of (let [element-generator (if (contains? model :elements-model)
                                          (generator context (:elements-model model) budget)
                                          gen/any)]
                  (cond->> (if (contains? model :count-model)
                             (gen/bind (generator context (:count-model model) budget)
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
                                                            [(:key entry) (generator context (:model entry) budget)]))
                                                        (:entries model) random-bools)
                                                   (filter some?)
                                                   (apply concat)
                                                   (apply gen/hash-map))))
                                  ;; Maybe consider supporting :count-model for generators
                                  (gen/map (generator context (-> model :keys :model) budget)
                                           (generator context (-> model :values :model) budget)))
                         (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))

        (:sequence-of :sequence) (cond->> (gen/bind gen/boolean
                                                    (fn [random-bool]
                                                      (let [gen (if (contains? model :entries)
                                                                  (apply gen/tuple (map (fn [entry]
                                                                                          (generator context (:model entry) budget))
                                                                                        (:entries model)))
                                                                  (let [elements-gen (generator context (:elements-model model) budget)]
                                                                    (if (contains? model :count-model)
                                                                      (gen/bind (generator context (:count-model model) budget)
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
                                            (let [gen (sequence-generator context model budget)]
                                              (let [inside-list? (case (:coll-type model)
                                                                   :list true
                                                                   :vector false
                                                                   random-bool)]
                                                (cond->> gen
                                                  inside-list? (gen/fmap (partial apply list)))))))
                                (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))

        :let (generator (merge context (:bindings model)) (:body model) budget)

        :ref (generator context (get context (:key model)) budget))))

(defn gen
  "Returns a test.check generator derived from the model."
  ([model]
   (gen model {}))
  ([model options]
   (let [visitor (fn [model stack path]
                   (-> model
                       (assoc-leaf-distance-visitor stack path)
                       (assoc-min-cost-visitor stack path)))
         walker (fn [model]
                  (postwalk model visitor))
         walked-model (util/iterate-while-different walker model 100)]
         ;options (into {:max-leaf-distance 20} options)]
     (if (contains? options :budget)
       (generator {} walked-model options)
       (gen/sized (fn [size]
                    (generator {} model (assoc options
                                          ; (* size 10) varies between 0 and 2000
                                          :budget (* size 10)))))))))
