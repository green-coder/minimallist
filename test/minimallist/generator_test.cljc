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


(defn post-walk [model visitor]
  (let [-walk (fn -walk [model context path]
                (case (:type model)
                  (:fn :enum
                   :set) {:bindings context
                          :model (visitor model context path)}
                  (:and :or) (visitor model context path) ;; TODO as :sequence
                  (:set-of
                   :sequence-of) (let [walked-model (update model :elements-model
                                                            -walk context (conj path :elements-model))]
                                   (visitor walked-model context path))
                  :map-of (let [walked-model (-> model
                                                 (update-in [:keys :model]
                                                            -walk context (conj path :keys :model))
                                                 (update-in [:values :model]
                                                            -walk context (conj path :values :model)))]
                            (visitor walked-model context path))
                  (:map
                   :sequence) (let [walked-model (cond-> model
                                                   (contains? model :entries)
                                                   (update :entries
                                                           (partial into []
                                                                    (map-indexed (fn [index entry]
                                                                                   (update entry :model
                                                                                           -walk context (conj path :entries index :model)))))))]
                                (visitor walked-model context path))
                  ;:alt
                  ;;:cat :repeat

                  ;; TODO: fix for multi-level :let
                  :let (let [{bindings :bindings
                              walked-body :model} (-walk (:body model) (:bindings model) (conj path :body))
                             walked-model (assoc model
                                            :bindings bindings
                                            :body walked-body)]
                         {:bindings context
                          :model (visitor walked-model context path)})
                  :ref (let [key (:key model)
                             {bindings :bindings
                              walked-model :model} (-walk (get context key) context (conj path key))
                             updated-context (assoc bindings key walked-model)]
                         {:bindings updated-context
                          :model (visitor model updated-context path)})))]
    (:model (-walk model (sorted-map) []))))

#_(post-walk (h/let ['leaf (h/fn int?)
                     'tree (h/ref 'leaf)]
                    (h/ref 'tree))
             (-with-leaf-distance))

(defn -with-leaf-distance []
  (let [counter (atom -1)]
    (fn [model context path]
      (swap! counter inc)
      (prn path)
      (-> model (assoc :visited @counter)))))
  ;(case (:type model)
  ;  (:fn :enum
  ;    :and :or) (assoc model :leaf-distance 0)
  ;  ;:set-of :set
  ;  ;:map-of :map
  ;  ;:sequence-of
  ;  :sequence (let [distances (->> (:entries model)
  ;                                 (mapv (comp :leaf-distance :model))
  ;                                 (filter some?))]
  ;              (cond-> model
  ;                (seq distances) (assoc :leaf-distance (inc (apply min distances)))))))
  ;  :alt
  ;  ;:cat :repeat
  ;  :let
  ;  :ref))
