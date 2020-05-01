(ns minimallist.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [minimallist.core :refer [valid? explain describe undescribe]]))

(deftest valid?-test
  (let [test-data [;; fn
                   {:type :fn
                    :fn #(= 1 %)}
                   [1]
                   [2]

                   ;; enum
                   {:type :enum
                    :values #{1 "2" :3}}
                   [1 "2" :3]
                   [[1] 2 true false nil]

                   {:type :enum
                    :values #{nil false}}
                   [nil false]
                   [true '()]

                   ;; and
                   {:type :and
                    :entries [{:model {:type :fn
                                       :fn pos-int?}}
                              {:model {:type :fn
                                       :fn even?}}]}
                   [2 4 6]
                   [0 :a -1 1 3]

                   ;; or
                   {:type :or
                    :entries [{:model {:type :fn
                                       :fn pos-int?}}
                              {:model {:type :fn
                                       :fn even?}}]}
                   [-2 0 1 2 3]
                   [-3 -1]

                   ;; set
                   {:type :set
                    :count-model {:type :enum
                                  :values #{2 3}}
                    :elements-model {:type :fn
                                     :fn int?}}
                   [#{1 2} #{1 2 3}]
                   [#{1 :a} [1 2 3] '(1 2) `(1 ~2) #{1} #{1 2 3 4}]

                   ;; map, entries
                   {:type :map
                    :entries [{:key :a
                               :model {:type :fn
                                       :fn int?}}
                              {:key :b
                               :model {:type :fn
                                       :fn string?}}
                              {:key (list 1 2 3)
                               :model {:type :fn
                                       :fn string?}}]}
                   [{:a 1, :b "foo", (list 1 2 3) "you can count on me like ..."}
                    {:a 10, :b "bar", [1 2 3] "soleil !"}]
                   [{:a 1, :b "foo"}
                    {:a 1, :b "foo", #{1 2 3} "bar"}]

                   ;; map, keys and values
                   {:type :map
                    :keys {:model {:type :fn
                                   :fn keyword?}}
                    :values {:model {:type :fn
                                     :fn int?}}}
                   [{} {:a 1, :b 2}]
                   [{:a 1, :b "2"} [[:a 1] [:b 2]] {true 1, false 2}]

                   ;; sequence, no collection type specified
                   {:type :sequence
                    :elements-model {:type :fn
                                     :fn int?}}
                   ['(1 2 3) [1 2 3] `(1 2 ~3)]
                   ['(1 :a) #{1 2 3} {:a 1, :b 2, :c 3}]

                   ;; sequence as a list
                   {:type :sequence
                    :coll-type :list
                    :elements-model {:type :fn
                                     :fn int?}}
                   ['(1 2 3)]
                   ['(1 :a) [1 2 3] #{1 2 3}
                    #_`(1 2 ~3)] ; this is not a list in cljs]

                   ;; sequence as a vector
                   {:type :sequence
                    :coll-type :vector
                    :elements-model {:type :fn
                                     :fn int?}}
                   [[1 2 3]]
                   [[1 :a] '(1 2 3) #{1 2 3} `(1 2 ~3)]

                   ;; sequence with fixed size
                   {:type :sequence
                    :count-model {:type :enum
                                  :values #{2 3}}}
                   ['(1 2) [1 "2"] `(1 ~"2") [1 "2" :3]]
                   [#{1 "a"} [1 "2" :3 :4]]

                   ;; sequence with entries (fixed size is implied)
                   {:type :sequence
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :fn
                                       :fn string?}}]}
                   ['(1 "2") [1 "2"] `(1 ~"2")]
                   [#{1 "a"} [1 "2" :3]]

                   ;; alt
                   {:type :alt
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :cat
                                       :entries [{:model {:type :fn
                                                          :fn string?}}]}}]}
                   [1 ["1"]]
                   [[1] "1" :1 [:1]]

                   ;; alt - inside a cat
                   {:type :cat
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :alt
                                       :entries [{:model {:type :fn
                                                          :fn string?}}
                                                 {:model {:type :fn
                                                          :fn keyword?}}
                                                 {:model {:type :cat
                                                          :entries [{:model {:type :fn
                                                                             :fn string?}}
                                                                    {:model {:type :fn
                                                                             :fn keyword?}}]}}]}}
                              {:model {:type :fn
                                       :fn int?}}]}
                   [[1 "2" 3] [1 :2 3] [1 "a" :b 3]]
                   [[1 ["a" :b] 3]]

                   ;; alt - inside a cat, but with :inline false on its cat entry
                   {:type :cat
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :alt
                                       :entries [{:model {:type :fn
                                                          :fn string?}}
                                                 {:model {:type :fn
                                                          :fn keyword?}}
                                                 {:model {:type :cat
                                                          :inlined false
                                                          :entries [{:model {:type :fn
                                                                             :fn string?}}
                                                                    {:model {:type :fn
                                                                             :fn keyword?}}]}}]}}
                              {:model {:type :fn
                                       :fn int?}}]}
                   [[1 "2" 3] [1 :2 3] [1 ["a" :b] 3]]
                   [[1 "a" :b 3]]

                   ;; cat of cat, the inner cat is implicitly inlined
                   {:type :cat
                    :coll-type :vector
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :cat
                                       :entries [{:model {:type :fn
                                                          :fn int?}}]}}]}
                   [[1 2]]
                   [[1] [1 [2]] [1 2 3] '(1) '(1 2) '(1 (2)) '(1 2 3)]

                   ;; cat of cat, the inner cat is explicitly not inlined
                   {:type :cat
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :cat
                                       :inlined false
                                       :entries [{:model {:type :fn
                                                          :fn int?}}]}}]}
                   [[1 [2]] [1 '(2)]]
                   [[1] [1 2] [1 [2] 3]]

                   ;; repeat - no collection type specified
                   {:type :repeat
                    :min 0
                    :max 2
                    :elements-model {:type :fn
                                     :fn int?}}
                   [[] [1] [1 2] '() '(1) '(2 3)]
                   [[1 2 3] '(1 2 3)]

                   ;; repeat - inside a vector
                   {:type :repeat
                    :coll-type :vector
                    :min 0
                    :max 2
                    :elements-model {:type :fn
                                     :fn int?}}
                   [[] [1] [1 2]]
                   [[1 2 3] '() '(1) '(2 3) '(1 2 3)]

                   ;; repeat - inside a list
                   {:type :repeat
                    :coll-type :list
                    :min 0
                    :max 2
                    :elements-model {:type :fn
                                     :fn int?}}
                   ['() '(1) '(2 3)]
                   [[] [1] [1 2] [1 2 3] '(1 2 3)]

                   ;; repeat - min > 0
                   {:type :repeat
                    :min 2
                    :max 3
                    :elements-model {:type :fn
                                     :fn int?}}
                   [[1 2] [1 2 3]]
                   [[] [1] [1 2 3 4]]

                   ;; repeat - max = +Infinity
                   {:type :repeat
                    :min 2
                    :max ##Inf
                    :elements-model {:type :fn
                                     :fn int?}}
                   [[1 2] [1 2 3] [1 2 3 4]]
                   [[] [1]]

                   ;; repeat - of a cat
                   {:type :repeat
                    :min 1
                    :max 2
                    :elements-model {:type :cat
                                     :entries [{:model {:type :fn
                                                        :fn int?}}
                                               {:model {:type :fn
                                                        :fn string?}}]}}
                   [[1 "a"] [1 "a" 2 "b"]]
                   [[] [1] [1 2] [1 "a" 2 "b" 3 "c"]]

                   ;; repeat - of a cat with :inlined false
                   {:type :repeat
                    :min 1
                    :max 2
                    :elements-model {:type :cat
                                     :inlined false
                                     :entries [{:model {:type :fn
                                                        :fn int?}}
                                               {:model {:type :fn
                                                        :fn string?}}]}}
                   [[[1 "a"]] [[1 "a"] [2 "b"]] ['(1 "a") [2 "b"]]]
                   [[] [1] [1 2] [1 "a"] [1 "a" 2 "b"] [1 "a" 2 "b" 3 "c"]]

                   ;; let / ref
                   {:type :let
                    :bindings {'pos-even? {:type :and
                                           :entries [{:model {:type :fn
                                                              :fn pos-int?}}
                                                     {:model {:type :fn
                                                              :fn even?}}]}}
                    :body {:type :ref
                           :ref 'pos-even?}}
                   [2 4]
                   [-2 -1 0 1 3]]]

    (doseq [[model valid-coll invalid-coll] (partition 3 test-data)]
      (doseq [data valid-coll]
        (is (valid? model data)))
      (doseq [data invalid-coll]
        (is (not (valid? model data)))))))




(defn- cleanup-description [description keys-to-remove]
  (walk/postwalk
    (fn [node]
      (if (and (map? node)
               (set/subset? #{:context :model :data} (set (keys node))))
        (as-> node xxx
              (cond-> xxx (contains? node :valid?) (update :valid? boolean))
              (update xxx :model select-keys [:type])
              (apply dissoc xxx keys-to-remove))

        node))
    description))

(deftest describe-test
  (let [test-data [;; fn
                   {:type :fn
                    :fn #(= 1 %)}
                   #{:context :model :data}
                   [1 {:valid? true}
                    2 {:valid? false}]

                   ;; enum
                   {:type :enum
                    :values #{1 "2" false nil}}
                   #{:context :model :data}
                   [1 {:valid? true}
                    "2" {:valid? true}
                    false {:valid? true}
                    nil {:valid? true}
                    true {:valid? false}]

                   ;; and
                   {:type :and
                    :entries [{:model {:type :fn
                                       :fn pos-int?}}
                              {:model {:type :fn
                                       :fn even?}}]}
                   #{:context :model :data}
                   [0 {:valid? false}
                    1 {:valid? false}
                    2 {:valid? true}
                    3 {:valid? false}
                    4 {:valid? true}]

                   ;; or
                   {:type :or
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :fn
                                       :fn string?}}]}
                   #{:context :model :data}
                   [1 {:valid? true}
                    "a" {:valid? true}
                    :a {:valid? false}]

                   ;; set
                   {:type :set
                    :elements-model {:type :fn
                                     :fn int?}}
                   #{:context :model :data}
                   [#{1} {:entries #{{:valid? true}}
                          :valid? true}]

                   ;; map
                   {:type :map
                    :entries [{:key :a
                               :model {:type :fn
                                       :fn int?}}
                              {:key :b
                               :model {:type :or
                                       :entries [{:model {:type :fn
                                                          :fn int?}}
                                                 {:model {:type :fn
                                                          :fn string?}}]}}]}
                   #{:context :model :data}
                   [{:a 1, :b 2} {:entries {:a {:valid? true}
                                            :b {:valid? true}}
                                  :valid? true}
                    {:a 1, :b "foo"} {:entries {:a {:valid? true}
                                                :b {:valid? true}}
                                      :valid? true}
                    {:a 1, :b [1 2]} {:entries {:a {:valid? true}
                                                :b {:valid? false}}
                                      :valid? false}
                    ; missing entry
                    {:a 1} {:entries {:a {:valid? true}
                                      :b {:missing? true}}
                            :valid? false}
                    ; extra entry
                    {:a 1, :b 2, :c 3} {:entries {:a {:valid? true}
                                                  :b {:valid? true}}
                                        :valid? true}]

                   ;; map - :keys
                   {:type :map
                    :keys {:model {:type :fn
                                   :fn keyword?}}}
                   #{:context :model :data}
                   [{:a 1, :b 2} {:valid? true}
                    {"a" 1} {:valid? false}]

                   ;; map - :values
                   {:type :map
                    :values {:model {:type :fn
                                     :fn int?}}}
                   #{:context :model :data}
                   [{:a 1, "b" 2} {:valid? true}
                    {:a "1"} {:valid? false}]

                   ;; sequence - :elements-model
                   {:type :sequence
                    :elements-model {:type :fn
                                     :fn int?}}
                   #{:context :model :data}
                   [[1 2 3] {:entries [{:valid? true}
                                       {:valid? true}
                                       {:valid? true}]
                             :valid? true}
                    '(1 2 3) {:entries [{:valid? true}
                                        {:valid? true}
                                        {:valid? true}]
                              :valid? true}
                    [1 "2" 3] {:entries [{:valid? true}
                                         {:valid? false}
                                         {:valid? true}]
                               :valid? false}]

                   ;; sequence - :coll-type vector
                   {:type :sequence
                    :coll-type :vector}
                   #{:context :model :data}
                   [[1 2 3] {:valid? true}
                    '(1 2 3) {:valid? false}]

                   ;; sequence - :entries
                   {:type :sequence
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :fn
                                       :fn string?}}]}
                   #{:context :model :data}
                   [[1 "a"] {:entries [{:valid? true}
                                       {:valid? true}]
                             :valid? true}
                    [1 2] {:entries [{:valid? true}
                                     {:valid? false}]
                            :valid? false}
                    [1] {:entries [{:valid? true}]
                          :valid? false}]

                   ;; sequence - :count-model
                   {:type :sequence
                    :count-model {:type :enum
                                  :values #{3}}}
                   #{:context :model :data}
                   [[1 2] {:valid? false}
                    [1 2 3] {:valid? true}
                    [1 2 3 4] {:valid? false}]

                   ;;; alt
                   ;{:type :alt
                   ; :entries [{:key :number
                   ;            :model {:type :fn
                   ;                    :fn int?}}
                   ;           {:key :sequence
                   ;            :model {:type :cat
                   ;                    :entries [{:key :text
                   ;                               :model {:type :fn
                   ;                                       :fn string?}}]}}]}
                   ;#{:context :model :data}
                   ;[1 {:key :number
                   ;    :entry {:valid? true}
                   ;    :valid? true}
                   ; ;["1"] {:key :sequence
                   ; ;       :entry {:entries {:text {:valid? true}}
                   ; ;               :valid? true}
                   ; ;       :valid? true}
                   ; [1] {:valid? false}
                   ; "1" {:valid? false}]
                   ;
                   ;;; alt - inside a cat
                   ;{:type :cat
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :alt
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn string?}}
                   ;                              {:model {:type :fn
                   ;                                       :fn keyword?}}
                   ;                              {:model {:type :cat
                   ;                                       :entries [{:model {:type :fn
                   ;                                                          :fn string?}}
                   ;                                                 {:model {:type :fn
                   ;                                                          :fn keyword?}}]}}]}}
                   ;           {:model {:type :fn
                   ;                    :fn int?}}]}
                   ;#{:context :model :data}
                   ;[[1 "2" 3] {}
                   ; [1 :2 3] {}
                   ; [1 "a" :b 3] {}
                   ; [1 ["a" :b] 3] {}]
                   ;
                   ;;; alt - inside a cat, but with :inline false on its cat entry
                   ;{:type :cat
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :alt
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn string?}}
                   ;                              {:model {:type :fn
                   ;                                       :fn keyword?}}
                   ;                              {:model {:type :cat
                   ;                                       :inlined false
                   ;                                       :entries [{:model {:type :fn
                   ;                                                          :fn string?}}
                   ;                                                 {:model {:type :fn
                   ;                                                          :fn keyword?}}]}}]}}
                   ;           {:model {:type :fn
                   ;                    :fn int?}}]}
                   ;#{:context :model :data}
                   ;[[1 "2" 3] {}
                   ; [1 :2 3] {}
                   ; [1 ["a" :b] 3] {}
                   ; [1 "a" :b 3] {}]
                   ;
                   ;;; cat of cat, the inner cat is implicitly inlined
                   ;{:type :cat
                   ; :coll-type :vector
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :cat
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn int?}}]}}]}
                   ;#{:context :model :data}
                   ;[[1 2] {}
                   ; [1] {}
                   ; [1 [2]] {}
                   ; [1 2 3] {}
                   ; '(1) {}
                   ; '(1 2) {}
                   ; '(1 (2)) {}
                   ; '(1 2 3) {}]
                   ;
                   ;;; cat of cat, the inner cat is explicitly not inlined
                   ;{:type :cat
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :cat
                   ;                    :inlined false
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn int?}}]}}]}
                   ;#{:context :model :data}
                   ;[[1 [2]] {}
                   ; [1 '(2)] {}
                   ; [1] {}
                   ; [1 2] {}
                   ; [1 [2] 3] {}]

                   ;; repeat - no collection type specified
                   {:type :repeat
                    :min 0
                    :max 2
                    :elements-model {:type :fn
                                     :fn int?}}
                   #{:context :model :data}
                   [[] {:entries []
                        :valid? true}
                    [1] {:entries [{:valid? true}]
                         :valid? true}
                    [1 2] {:entries [{:valid? true}
                                     {:valid? true}]
                           :valid? true}
                    '() {:entries []
                         :valid? true}
                    '(1) {:entries [{:valid? true}]
                          :valid? true}
                    '(2 3) {:entries [{:valid? true}
                                      {:valid? true}]
                            :valid? true}
                    [1 2 3] {:valid? false}
                    '(1 2 3) {:valid? false}]

                   ;;; repeat - inside a vector
                   ;{:type :repeat
                   ; :coll-type :vector
                   ; :min 0
                   ; :max 2
                   ; :elements-model {:type :fn
                   ;                  :fn int?}}
                   ;#{:context :model :data}
                   ;[[] {}
                   ; [1] {}
                   ; [1 2] {}
                   ; [1 2 3] {}
                   ; '() {}
                   ; '(1) {}
                   ; '(2 3) {}
                   ; '(1 2 3) {}]
                   ;
                   ;;; repeat - inside a list
                   ;{:type :repeat
                   ; :coll-type :list
                   ; :min 0
                   ; :max 2
                   ; :elements-model {:type :fn
                   ;                  :fn int?}}
                   ;#{:context :model :data}
                   ;['() {}
                   ; '(1) {}
                   ; '(2 3) {}
                   ; [] {}
                   ; [1] {}
                   ; [1 2] {}
                   ; [1 2 3] {}
                   ; '(1 2 3) {}]
                   ;
                   ;;; repeat - min > 0
                   ;{:type :repeat
                   ; :min 2
                   ; :max 3
                   ; :elements-model {:type :fn
                   ;                  :fn int?}}
                   ;#{:context :model :data}
                   ;[[1 2] {}
                   ; [1 2 3] {}
                   ; [] {}
                   ; [1] {}
                   ; [1 2 3 4] {}]
                   ;
                   ;;; repeat - max = +Infinity
                   ;{:type :repeat
                   ; :min 2
                   ; :max ##Inf
                   ; :elements-model {:type :fn
                   ;                  :fn int?}}
                   ;#{:context :model :data}
                   ;[[1 2] {}
                   ; [1 2 3] {}
                   ; [1 2 3 4] {}
                   ; [] {}
                   ; [1] {}]
                   ;
                   ;;; repeat - of a cat
                   ;{:type :repeat
                   ; :min 1
                   ; :max 2
                   ; :elements-model {:type :cat
                   ;                  :entries [{:model {:type :fn
                   ;                                     :fn int?}}
                   ;                            {:model {:type :fn
                   ;                                     :fn string?}}]}}
                   ;#{:context :model :data}
                   ;[[1 "a"] {}
                   ; [1 "a" 2 "b"] {}
                   ; [] {}
                   ; [1] {}
                   ; [1 2] {}
                   ; [1 "a" 2 "b" 3 "c"] {}]
                   ;
                   ;;; repeat - of a cat with :inlined false
                   ;{:type :repeat
                   ; :min 1
                   ; :max 2
                   ; :elements-model {:type :cat
                   ;                  :inlined false
                   ;                  :entries [{:model {:type :fn
                   ;                                     :fn int?}}
                   ;                            {:model {:type :fn
                   ;                                     :fn string?}}]}}
                   ;#{:context :model :data}
                   ;[[[1 "a"]] {}
                   ; [[1 "a"] [2 "b"]] {}
                   ; ['(1 "a") [2 "b"]] {}
                   ; [] {}
                   ; [1] {}
                   ; [1 2] {}
                   ; [1 "a"] {}
                   ; [1 "a" 2 "b"] {}
                   ; [1 "a" 2 "b" 3 "c"] {}]

                   ;; let / ref
                   {:type :let
                    :bindings {'pos-even? {:type :and
                                           :entries [{:model {:type :fn
                                                              :fn pos-int?}}
                                                     {:model {:type :fn
                                                              :fn even?}}]}}
                    :body {:type :ref
                           :ref 'pos-even?}}
                   #{:context :model :data}
                   [0 {:valid? false}
                    1 {:valid? false}
                    2 {:valid? true}
                    3 {:valid? false}
                    4 {:valid? true}]

                   #__]]


    (doseq [[model keys-to-remove data-description-pairs] (partition 3 test-data)]
      (doseq [[data description] (partition 2 data-description-pairs)]
        (is (= [data (cleanup-description (describe model data) keys-to-remove)]
               [data description]))))))
