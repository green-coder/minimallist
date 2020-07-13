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
  (if (= (:type model) :repeat)
    (:min model)
    (let [count-model (:count-model model)]
      (if (nil? count-model)
        0
        (case (:type count-model)
          :enum (let [values (filter number? (:values count-model))]
                  (when (seq values)
                    (apply min values)))
          :fn (:min-value count-model)
          nil)))))

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
                                     (zero? (min-count-value model)))
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

(defn- rec-coll-size-gen [max-size]
  (if (zero? max-size)
    (gen/return 0)
    (gen/fmap (fn [[x y]] (+ x y 1))
              (gen/tuple (gen/choose 0 (quot (dec max-size) 2))
                         (gen/choose 0 (quot max-size 2))))))

;; Statistics about the distribution
#_ (->> (gen/sample (rec-coll-size-gen 20) 10000)
        (frequencies)
        (sort-by first))

;; maybe, use rec-coll-size-gen to pick up a size at each iteration
(defn- decreasing-sizes-gen
  "Returns a generator of lazy sequence of decreasing sizes."
  [max-size]
  (#'gen/make-gen
    (fn [rng _]
      (let [f (fn f [rng max-size]
                (when-not (neg? max-size)
                  (lazy-seq
                    (let [[r1 r2] (random/split rng)
                          size (#'gen/rand-range r1 0 max-size)]
                      (cons size (f r2 (dec size)))))))]
        (rose/pure (f rng max-size))))))

#_(gen/sample (decreasing-sizes-gen 100) 1)

(defn- budget-split-gen
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
          :alt (let [possible-entries (filterv (comp ::leaf-distance :model)
                                               (:entries model))
                     affordable-entries (filterv (fn [entry] (<= (-> entry :model ::min-cost) budget))
                                                 possible-entries)]
                 (if (seq affordable-entries)
                   (gen/let [index (gen/choose 0 (dec (count affordable-entries)))]
                     (sequence-generator context (:model (affordable-entries index)) budget))
                   (let [chosen-entry (first (sort-by (comp ::min-cost :model) possible-entries))]
                     (sequence-generator context (:model chosen-entry) budget))))

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

        ;; TODO: there "might be" an problem a enumeration order from the set.
        :enum (gen/elements (:values model))

        (:and :or) nil ;; a generator is supposed to be provided for those nodes

        :alt (let [possible-entries (filterv (comp ::leaf-distance :model)
                                             (:entries model))
                   affordable-entries (filterv (fn [entry] (<= (-> entry :model ::min-cost) budget))
                                               possible-entries)]
               (if (seq affordable-entries)
                 (gen/let [index (gen/choose 0 (dec (count affordable-entries)))]
                   (generator context (:model (affordable-entries index)) budget))
                 (let [chosen-entry (first (sort-by (comp ::min-cost :model) possible-entries))]
                   (generator context (:model chosen-entry) budget))))

        :set-of (let [budget (max 0 (dec budget)) ; the collection itself costs 1
                      elements-model (:elements-model model)
                      count-model (:count-model model)
                      elm-min-cost (::min-cost elements-model 1)
                      coll-sizes-gen (if count-model
                                       (if (= (:type count-model) :enum)
                                         (gen/shuffle (sort (:values count-model)))
                                         (gen/vector-distinct (generator context count-model 0)
                                                              {:min-elements 1}))
                                       (decreasing-sizes-gen (int (/ budget elm-min-cost))))
                      set-gen (gen/bind coll-sizes-gen
                                        (fn [coll-sizes]
                                          (#'gen/make-gen
                                            (fn [rng gen-size]
                                              (loop [rng rng
                                                     coll-sizes coll-sizes]
                                                (when-not (seq coll-sizes)
                                                  (throw (ex-info "Couldn't generate a set." {:elements-model elements-model
                                                                                              :coll-sizes coll-sizes})))
                                                (let [[r1 r2 next-rng] (random/split-n rng 3)
                                                      coll-size (first coll-sizes)
                                                      elements-gen (if elements-model
                                                                     (let [min-costs (repeat coll-size elm-min-cost)
                                                                           budgets (-> (budget-split-gen budget min-costs)
                                                                                       (gen/call-gen r1 gen-size)
                                                                                       (rose/root))]
                                                                       (apply gen/tuple
                                                                              (mapv (partial generator context elements-model)
                                                                                    budgets)))
                                                                     (gen/vector gen/any coll-size))
                                                      elements (-> (gen/call-gen elements-gen r2 gen-size)
                                                                   (rose/root))
                                                      elements-in-set (into #{} elements)]
                                                  (if (= (count elements) (count elements-in-set))
                                                    (rose/pure elements-in-set)
                                                    (recur next-rng (rest coll-sizes)))))))))]
                  (cond->> set-gen
                           (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model)))))

        :map-of (cond->> (let [budget (max 0 (dec budget)) ; the collection itself costs 1
                               count-model (:count-model model)
                               keys-model (-> model :keys :model)
                               values-model (-> model :values :model)
                               entry-min-cost (+ (::min-cost keys-model 1) ;; TODO: is the default values needed?
                                                 (::min-cost values-model 1))
                               coll-max-size (int (/ budget entry-min-cost))
                               coll-size-gen (if count-model
                                               (generator context count-model 0)
                                               (rec-coll-size-gen coll-max-size))
                               budgets-gen (gen/bind coll-size-gen
                                                     (fn [coll-size]
                                                       (let [min-costs (repeat coll-size entry-min-cost)]
                                                         (budget-split-gen budget min-costs))))
                               entries-gen (gen/bind budgets-gen
                                                     (fn [entry-budgets]
                                                       (apply gen/tuple
                                                              (mapv (fn [entry-budget]
                                                                      (gen/tuple
                                                                        (generator context keys-model entry-budget)
                                                                        (generator context values-model entry-budget)))
                                                                    entry-budgets))))
                               map-gen (gen/fmap (fn [entries]
                                                   (into {} entries))
                                                 entries-gen)]
                           (cond->> map-gen
                             (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))))

        :map (let [budget (max 0 (dec budget)) ; the collection itself costs 1
                   possible-entries (filterv (comp ::min-cost :model) (:entries model))
                   {required-entries false, optional-entries true} (group-by (comp true? :optional) possible-entries)
                   required-min-cost (transduce (map (comp ::min-cost :model)) + required-entries)
                   map-gen (gen/let [coll-size (if (< required-min-cost budget)
                                                 (gen/choose (count required-entries) (count possible-entries))
                                                 (gen/return (count required-entries)))
                                     selected-entries (gen/fmap (fn [shuffled-optional-entries]
                                                                  (->> (concat possible-entries shuffled-optional-entries)
                                                                       (take coll-size)))
                                                                (gen/shuffle optional-entries))
                                     entry-budgets (let [min-costs (mapv (comp ::min-cost :model) selected-entries)]
                                                     (budget-split-gen budget min-costs))]
                             (apply gen/hash-map
                                    (mapcat (fn [entry budget]
                                              [(:key entry) (generator context (:model entry) budget)])
                                          selected-entries
                                          entry-budgets)))]
               (cond->> map-gen
                 (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model)))))

        (:sequence-of :sequence) (let [budget (max 0 (dec budget)) ; the collection itself costs 1
                                       entries (:entries model)
                                       coll-gen (if entries
                                                  ; :sequence ... count-model is not used
                                                  (gen/bind (budget-split-gen budget (mapv (fn [entry]
                                                                                             (::min-cost entry 1))
                                                                                           entries))
                                                            (fn [budgets]
                                                              (apply gen/tuple (mapv (fn [entry budget]
                                                                                       (generator context (:model entry) budget))
                                                                                     entries budgets))))
                                                  ; :sequence-of ... count-model and/or elements-model might be used
                                                  (let [count-model (:count-model model)
                                                        elements-model (:elements-model model)
                                                        elm-min-cost (::min-cost elements-model 1)
                                                        coll-max-size (int (/ budget elm-min-cost))
                                                        coll-size-gen (if count-model
                                                                        (generator context count-model 0)
                                                                        (rec-coll-size-gen coll-max-size))]
                                                    (if elements-model
                                                      (let [budgets-gen (gen/bind coll-size-gen
                                                                                  (fn [coll-size]
                                                                                    (let [min-costs (repeat coll-size elm-min-cost)]
                                                                                      (budget-split-gen budget min-costs))))]
                                                        (gen/bind budgets-gen
                                                                  (fn [budgets]
                                                                    (apply gen/tuple
                                                                           (mapv (partial generator context elements-model)
                                                                                 budgets)))))
                                                      (gen/bind coll-size-gen
                                                                (fn [coll-size]
                                                                  (gen/vector gen/any coll-size))))))
                                       inside-list?-gen (case (:coll-type model)
                                                          :list (gen/return true)
                                                          :vector (gen/return false)
                                                          (gen/no-shrink gen/boolean))
                                       seq-gen (gen/fmap (fn [[coll inside-list?]]
                                                           (cond->> coll
                                                             inside-list? (apply list)))
                                                         (gen/tuple coll-gen inside-list?-gen))]
                                   (cond->> seq-gen
                                     (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model)))))

        (:cat :repeat) (cond->> (gen/bind gen/boolean
                                          (fn [random-bool]
                                            (let [budget (max 0 (dec budget)) ; the collection itself costs 1
                                                  gen (sequence-generator context model budget)
                                                  inside-list? (case (:coll-type model)
                                                                 :list true
                                                                 :vector false
                                                                 random-bool)]
                                              (cond->> gen
                                                inside-list? (gen/fmap (partial apply list))))))
                                (contains? model :condition-model) (gen/such-that (partial valid? context (:condition-model model))))

        :let (generator (merge context (:bindings model)) (:body model) budget)

        :ref (generator context (get context (:key model)) budget))))

(defn decorate-model
  "Analyzes and decorates the model with information regarding 'distance to leaf' and 'minimal cost'."
  [model]
  (let [visitor (fn [model stack path]
                  (-> model
                      (assoc-leaf-distance-visitor stack path)
                      (assoc-min-cost-visitor stack path)))
        walker (fn [model]
                 (postwalk model visitor))]
    (util/iterate-while-different walker model 100)))

(defn gen
  "Returns a test.check generator derived from the model."
  ([model]
   (gen model nil))
  ([model budget]
   (let [decorated-model (decorate-model model)]
     (when-not (::min-cost decorated-model)
       (throw (ex-info "The model cannot be generated as it contains infinite structures that cannot be avoided during generation."
                       {:model model
                        :decorated-model decorated-model})))
     (if budget
       (generator {} decorated-model budget)
       (gen/sized (fn [size] ; size varies between 0 and 200
                    (generator {} decorated-model size)))))))
