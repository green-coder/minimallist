(ns minimallist.generator-test
  (:require [clojure.test :refer [deftest testing is are]]
            [minimallist.core :refer [valid?]]
            [minimallist.helper :as h]
            [minimallist.generator :refer [generator] :as g]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]))

(comment
  ;; Nothing replaces occasional hand testing

  (def fn-int? (-> (h/fn int?)
                   (h/with-test-check-gen gen/nat)))

  (def fn-string? (-> (h/fn string?)
                      (h/with-test-check-gen gen/string-alphanumeric)))

  (gen/sample (generator (-> (h/set)
                             (h/with-count (h/enum #{1 2 3 10}))
                             (h/with-condition (h/fn (comp #{1 2 3} count))))))

  (gen/sample (generator (h/map-of fn-int? fn-string?)))

  (gen/sample (generator (-> (h/map [:a fn-int?])
                             (h/with-optional-entries [:b fn-string?]))))

  (gen/sample (generator (h/sequence-of fn-int?)))

  (gen/sample (generator (h/cat fn-int? fn-string?)))

  (gen/sample (generator (h/repeat 2 3 fn-int?)))

  (gen/sample (generator (h/repeat 2 3 (h/cat fn-int? fn-string?))))

  (gen/sample (generator (h/let ['int? fn-int?
                                 'string? fn-string?
                                 'int-string? (h/cat (h/ref 'int?) (h/ref 'string?))]
                                (h/repeat 2 3 (h/ref 'int-string?))))))

(deftest generator-test

  (let [fn-int? (-> (h/fn int?) (h/with-test-check-gen gen/nat))
        fn-string? (-> (h/fn string?) (h/with-test-check-gen gen/string-alphanumeric))]

    (let [model fn-string?]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/enum #{:1 2 "3"})]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (-> (h/set-of fn-int?)
                    (h/with-condition (h/fn (partial some odd?))))]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (-> (h/set)
                    (h/with-count (h/enum #{1 2 3 10}))
                    (h/with-condition (h/fn (comp #{1 2 3} count))))]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/map-of fn-int? fn-string?)]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (-> (h/map [:a fn-int?])
                    (h/with-optional-entries [:b fn-string?]))
          sample (gen/sample (generator model) 100)]
      (is (and (every? (partial valid? model) sample)
               (some (fn [element] (contains? element :b)) sample)
               (some (fn [element] (not (contains? element :b))) sample))))

    (let [model (h/sequence-of fn-int?)]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/tuple fn-int? fn-string?)
          sample (gen/sample (generator model) 100)]
      (is (and (every? (partial valid? model) sample)
               (some list? sample)
               (some vector? sample))))

    (let [model (h/vector fn-int? fn-string?)
          sample (gen/sample (generator model))]
      (is (and (every? (partial valid? model) sample)
               (every? vector? sample))))

    (let [model (h/list fn-int? fn-string?)
          sample (gen/sample (generator model))]
      (is (and (every? (partial valid? model) sample)
               (every? list? sample))))

    (let [model (h/alt fn-int? fn-string?)]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/cat fn-int? fn-string?)]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/repeat 2 3 fn-int?)]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/repeat 2 3 (h/cat fn-int? fn-string?))]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))

    (let [model (h/let ['int? fn-int?
                        'string? fn-string?
                        'int-string? (h/cat (h/ref 'int?) (h/ref 'string?))]
                  (h/repeat 2 3 (h/ref 'int-string?)))]
      (is (every? (partial valid? model)
                  (gen/sample (generator model)))))))

    ;;; TODO: limit the size of recursive models
    ;
    ;;; Budget-based limit on model choice.
    ;(let [model (h/let ['tree (h/alt [:leaf fn-int?]
    ;                                 [:branch (h/vector (h/ref 'tree)
    ;                                                    (h/ref 'tree))])]
    ;
    ;              (h/ref 'tree))]
    ;  (is (every? (partial valid? model)
    ;              (gen/sample (generator model)))))
    ;
    ;;; Budget-based limit on variable collection size.
    ;(let [model (h/let ['node (h/vector-of (h/ref 'node))]
    ;              (h/ref 'node))]
    ;  (is (every? (partial valid? model)
    ;              (gen/sample (generator model)))))))

(defn- find-stack-index [stack key]
  (loop [index (dec (count stack))
         elements (rseq stack)]
    (when elements
      (let [elm (first elements)]
        (if (contains? (:bindings elm) key)
          index
          (recur (dec index) (next elements)))))))

(defn reduce-update [[acc data] key f & args]
  (let [elm (get data key)
        [updated-acc updated-elm] (apply f acc elm args)
        updated-data (assoc data key updated-elm)]
    [updated-acc updated-data]))

(defn reduce-update-in [[acc data] path f & args]
  (let [elm (get-in data path)
        [updated-acc updated-elm] (apply f acc elm args)
        updated-data (assoc-in data path updated-elm)]
    [updated-acc updated-data]))

#_(let [m {:a {:x 1, :y 2}
           :b [3 4 5]}
        f (fn [acc elm]
            (let [elm10 (* elm 10)]
              [(conj acc elm10) elm10]))]
    (-> [[] m]
        (reduce-update-in [:a :x] f)
        (reduce-update-in [:b 2] f)))
; => [{:a {:x 10, :y 2}, :b [3 4 50]} [10 50]]

(defn reduce-mapv [f acc coll]
  (reduce (fn [[acc v] elm]
            (let [[updated-acc updated-elm] (f acc elm)]
              [updated-acc (conj v updated-elm)]))
          [acc []]
          coll))

#_(let [m {:a {:x 1, :y 2}
           :b [3 4 5]}
        f (fn [acc elm]
            (let [elm10 (* elm 10)]
              [(conj acc elm10) elm10]))]
    (reduce-update [[] m] :b (partial reduce-mapv f)))
;=> [{:a {:x 1, :y 2}, :b [30 40 50]} [30 40 50]]


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

(defn -with-visit-order []
  (let [counter (atom -1)]
    (fn [model stack path]
      (swap! counter inc)
      (prn path)
      (-> model (assoc :visit-order @counter)))))

#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/ref 'leaf)]
                   (h/ref 'tree))
            (-with-visit-order))

#_(postwalk (h/let ['root (h/let ['leaf (h/fn int?)
                                  'tree (h/ref 'leaf)]
                                 (h/ref 'tree))]
                   (h/ref 'root))
            (-with-visit-order))

#_(postwalk (h/let ['leaf (h/fn int?)
                    'root (h/let ['tree (h/ref 'leaf)]
                                 (h/ref 'tree))]
                   (h/ref 'root))
            (-with-visit-order))

; test of no visit more than once
#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/tuple (h/ref 'leaf) (h/ref 'leaf))]
                   (h/ref 'tree))
            (-with-visit-order))

; test of no visit more than once, infinite loop otherwise
#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/tuple (h/ref 'tree) (h/ref 'leaf))]
                   (h/ref 'tree))
            (-with-visit-order))

(defn- assoc-leaf-distance-disjunction [model distances]
  (let [non-nil-distances (filter some? distances)]
    (cond-> model
      (seq non-nil-distances)
      (assoc :leaf-distance (inc (reduce min non-nil-distances))))))

(defn- assoc-leaf-distance-conjunction [model distances]
  (cond-> model
    (every? some? distances)
    (assoc :leaf-distance (inc (reduce max 0 distances)))))


(defn -with-leaf-distance [model stack path]
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
                                            (mapv (comp :leaf-distance :model)
                                                  (:entries model)))
    :let (let [body-distance (:leaf-distance (:body model))]
           (cond-> model
             (some? body-distance) (assoc :leaf-distance (inc body-distance))))
    :ref (let [key (:key model)
               index (find-stack-index stack key)
               binding-distance (get-in stack [index :bindings key :leaf-distance])]
           (cond-> model
             (some? binding-distance) (assoc :leaf-distance (inc binding-distance))))))

; Recursive data-structure impossible to generate
#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/tuple (h/ref 'tree) (h/ref 'leaf))]
                   (h/ref 'tree))
            -with-leaf-distance)

; Recursive data-structure which can be generated
#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/alt (h/ref 'tree) (h/ref 'leaf))]
                   (h/ref 'tree))
            -with-leaf-distance)
