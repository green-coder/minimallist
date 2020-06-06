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

(defn postwalk [model visitor]
  (let [walk (fn walk [model stack walked-bindings path]
               (let [[model stack walked-bindings]
                     (case (:type model)
                       (:fn :enum
                        :set) [model stack walked-bindings]
                       (:set-of :sequence-of
                        :repeat) (let [[model' stack' walked-bindings'] (walk (:elements-model model)
                                                                              stack
                                                                              walked-bindings
                                                                              (conj path :elements-model))]
                                   [(assoc model :elements-model model') stack' walked-bindings'])
                       :map-of (let [[model' stack' walked-bindings'] (walk (-> model :keys :model)
                                                                            stack
                                                                            walked-bindings
                                                                            (conj path :keys :model))
                                     [model'' stack'' walked-bindings''] (walk (-> model' :values :model)
                                                                               stack'
                                                                               walked-bindings'
                                                                               (conj path :values :model))]
                                 [(-> model
                                      (assoc-in [:keys :model] model')
                                      (assoc-in [:values :model] model'')) stack'' walked-bindings''])
                       (:and :or
                        :map :sequence
                        :alt :cat) (if (contains? model :entries)
                                     (let [[walked-entries stack' walked-bindings'] (reduce (fn [[walked-entries stack walked-bindings] [index entry]]
                                                                                              (let [[walked-entry-model stack' walked-bindings'] (walk (:model entry)
                                                                                                                                                       stack
                                                                                                                                                       walked-bindings
                                                                                                                                                       (conj path :entries index :model))]
                                                                                                [(conj walked-entries (assoc entry :model walked-entry-model)) stack' walked-bindings']))
                                                                                            [[] stack walked-bindings]
                                                                                            (map-indexed vector (:entries model)))]
                                       [(assoc model :entries walked-entries) stack' walked-bindings'])
                                     [model stack walked-bindings])
                       :let (let [[walked-body stack' walked-bindings'] (walk (:body model)
                                                                              (conj stack {:bindings (:bindings model)
                                                                                            :path (conj path :bindings)})
                                                                              walked-bindings
                                                                              (conj path :body))]
                              [(assoc model
                                 :bindings (:bindings (peek stack'))
                                 :body walked-body) (pop stack') walked-bindings'])
                       :ref (let [key (:key model)
                                  index (find-stack-index stack key)
                                  binding-path (conj (get-in stack [index :path]) key)]
                              (if (contains? walked-bindings binding-path)
                                [model stack walked-bindings]
                                (let [[walked-ref-model stack' walked-bindings'] (walk (get-in stack [index :bindings key])
                                                                                       (subvec stack 0 (inc index))
                                                                                       (conj walked-bindings binding-path)
                                                                                       binding-path)]
                                  [model (-> stack'
                                             (assoc-in [index :bindings key] walked-ref-model)
                                             (into (subvec stack (inc index)))) walked-bindings']))))]
                 [(visitor model stack path) stack walked-bindings]))]
    (first (walk model [] #{} []))))

#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/ref 'leaf)]
                   (h/ref 'tree))
            (-with-leaf-distance))

#_(postwalk (h/let ['root (h/let ['leaf (h/fn int?)
                                  'tree (h/ref 'leaf)]
                                 (h/ref 'tree))]
                   (h/ref 'root))
            (-with-leaf-distance))

#_(postwalk (h/let ['leaf (h/fn int?)
                    'root (h/let ['tree (h/ref 'leaf)]
                                 (h/ref 'tree))]
                   (h/ref 'root))
            (-with-leaf-distance))

; test of no visit more than once
#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/tuple (h/ref 'leaf) (h/ref 'leaf))]
                   (h/ref 'tree))
            (-with-leaf-distance))

; test of no visit more than once, infinite loop otherwise
#_(postwalk (h/let ['leaf (h/fn int?)
                    'tree (h/tuple (h/ref 'tree) (h/ref 'leaf))]
                   (h/ref 'tree))
            (-with-leaf-distance))

(defn -with-leaf-distance []
  (let [counter (atom -1)]
    (fn [model stack path]
      (swap! counter inc)
      (prn path)
      ;(-> model (assoc :visited @counter)))))
      (case (:type model)
        (:fn :enum) (assoc model :leaf-distance 0)
        ;:set-of :set
        ;:map-of :map
        ;:sequence-of :repeat
        (:and :or
         :alt :cat
         :sequence) (let [distances (->> (:entries model)
                                         (mapv (comp :leaf-distance :model))
                                         (filter some?))]
                      (cond-> model
                        (seq distances) (assoc :leaf-distance (inc (apply min distances)))))
        :let (let [body-distance (:leaf-distance (:body model))]
               (cond-> model
                 (some? body-distance) (assoc :leaf-distance (inc body-distance))))
        :ref (let [key (:key model)
                   index (find-stack-index stack key)
                   binding-distance (get-in stack [index :bindings key :leaf-distance])]
               (cond-> model
                 (some? binding-distance) (assoc :leaf-distance (inc binding-distance))))))))
