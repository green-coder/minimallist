(ns minimallist.generator-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]
            [minimallist.core :refer [valid?]]
            [minimallist.helper :as h]
            [minimallist.util :as util]
            [minimallist.generator :refer [generator] :as g]))

(defn- path-test-visitor []
  ;; Testing using side effects.
  ;; A little ugly, but good enough for tests.
  (let [paths (atom [])]
    (fn
      ([] @paths)
      ([model stack path]
       (swap! paths conj path)
       model))))

(deftest postwalk-visit-order-test
  (are [model expected-paths]
    (let [visitor (path-test-visitor)]
      (#'g/postwalk model visitor)  ; Create side effects
      (= (visitor) expected-paths)) ; Collect and compare the side effects

    (h/let ['leaf (h/fn int?)
            'tree (h/ref 'leaf)]
           (h/ref 'tree))
    [[:bindings 'leaf]
     [:bindings 'tree]
     [:body]
     []]

    (h/let ['root (h/let ['leaf (h/fn int?)
                          'tree (h/ref 'leaf)]
                         (h/ref 'tree))]
           (h/ref 'root))
    [[:bindings 'root :bindings 'leaf]
     [:bindings 'root :bindings 'tree]
     [:bindings 'root :body]
     [:bindings 'root]
     [:body]
     []]

    (h/let ['leaf (h/fn int?)
            'root (h/let ['tree (h/ref 'leaf)]
                         (h/ref 'tree))]
           (h/ref 'root))
    [[:bindings 'leaf]
     [:bindings 'root :bindings 'tree]
     [:bindings 'root :body]
     [:bindings 'root]
     [:body]
     []]

    ; test of no visit more than once
    (h/let ['leaf (h/fn int?)
            'tree (h/tuple (h/ref 'leaf) (h/ref 'leaf))]
           (h/ref 'tree))
    [[:bindings 'leaf]
     [:bindings 'tree :entries 0 :model]
     [:bindings 'tree :entries 1 :model]
     [:bindings 'tree]
     [:body]
     []]

    ; test of no visit more than once, infinite loop otherwise
    (h/let ['leaf (h/fn int?)
            'tree (h/tuple (h/ref 'tree) (h/ref 'leaf))]
           (h/ref 'tree))
    [[:bindings 'tree :entries 0 :model]
     [:bindings 'leaf]
     [:bindings 'tree :entries 1 :model]
     [:bindings 'tree]
     [:body]
     []]

    #__))

(deftest assoc-leaf-distance-visitor-test
  (are [model expected-walked-model]
    (= (-> model
           (g/postwalk #'g/assoc-leaf-distance-visitor)
           (util/walk-map-dissoc :fn))
       expected-walked-model)

    ; Recursive data-structure impossible to generate
    ; This one is trying to bring the function generator in an infinite loop.
    (h/let ['loop (h/ref 'loop)]
           (h/ref 'loop))
    {:type :let
     :bindings {'loop {:type :ref
                       :key 'loop}}
     :body {:type :ref
            :key 'loop}}

    ; Recursive data-structure impossible to generate
    (h/let ['leaf (h/fn int?)
            'tree (h/tuple (h/ref 'tree) (h/ref 'leaf))]
           (h/ref 'tree))
    {:type :let
     :bindings {'leaf {:type :fn
                       :leaf-distance 0}
                'tree {:type :sequence
                       :entries [{:model {:type :ref
                                          :key 'tree}}
                                 {:model {:type :ref
                                          :key 'leaf
                                          :leaf-distance 1}}]}}
     :body {:type :ref
            :key 'tree}}

    ; Recursive data-structure impossible to generate
    (h/let ['rec-map (h/map [:a (h/fn int?)]
                            [:b (h/ref 'rec-map)])]
           (h/ref 'rec-map))
    {:type :let
     :bindings {'rec-map {:type :map
                          :entries [{:key :a
                                     :model {:type :fn
                                             :leaf-distance 0}}
                                    {:key :b
                                     :model {:type :ref
                                             :key 'rec-map}}]}}
     :body {:type :ref
            :key 'rec-map}}

    ; Recursive data-structure which can be generated
    (h/let ['leaf (h/fn int?)
            'tree (h/alt (h/ref 'tree) (h/ref 'leaf))]
           (h/ref 'tree))
    {:type :let
     :bindings {'leaf {:type :fn
                       :leaf-distance 0}
                'tree {:type :alt
                       :entries [{:model {:type :ref
                                          :key 'tree}}
                                 {:model {:type :ref
                                          :key 'leaf
                                          :leaf-distance 1}}]
                       :leaf-distance 2}}
     :body {:type :ref
            :key 'tree
            :leaf-distance 3}
     :leaf-distance 4}

    (h/let ['rec-map (h/map [:a (h/fn int?)]
                            [:b {:optional true} (h/ref 'rec-map)])]
           (h/ref 'rec-map))
    {:type :let
     :bindings {'rec-map {:type :map
                          :entries [{:key :a
                                     :model {:type :fn
                                             :leaf-distance 0}}
                                    {:key :b
                                     :optional true
                                     :model {:type :ref
                                             :key 'rec-map}}]
                          :leaf-distance 1}}
     :body {:type :ref
            :key 'rec-map
            :leaf-distance 2}
     :leaf-distance 3}

    #__))



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
