(ns minimallist.core-test
  (:require [clojure.test :refer [deftest testing is are]]
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
                    :model {:type :fn
                            :fn int?}}
                   [#{1 2 3}]
                   [#{1 :a} [1 2 3] '(1 2 3) `(1 2 ~3)]

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
                    :model {:type :fn
                            :fn int?}}
                   ['(1 2 3) [1 2 3] `(1 2 ~3)]
                   ['(1 :a) #{1 2 3} {:a 1, :b 2, :c 3}]

                   ;; sequence as a list
                   {:type :sequence
                    :coll-type :list
                    :model {:type :fn
                            :fn int?}}
                   ['(1 2 3)]
                   ['(1 :a) [1 2 3] #{1 2 3}
                    #_`(1 2 ~3)] ; this is not a list in cljs]

                   ;; sequence as a vector
                   {:type :sequence
                    :coll-type :vector
                    :model {:type :fn
                            :fn int?}}
                   [[1 2 3]]
                   [[1 :a] '(1 2 3) #{1 2 3} `(1 2 ~3)]

                   ;; sequence with fixed size
                   {:type :sequence
                    :count 2}
                   ['(1 2) [1 "2"] `(1 ~"2")]
                   [#{1 "a"} [1 "2" :3]]

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
                    :model {:type :fn
                            :fn int?}}
                   [[] [1] [1 2] '() '(1) '(2 3)]
                   [[1 2 3] '(1 2 3)]

                   ;; repeat - inside a vector
                   {:type :repeat
                    :coll-type :vector
                    :min 0
                    :max 2
                    :model {:type :fn
                            :fn int?}}
                   [[] [1] [1 2]]
                   [[1 2 3] '() '(1) '(2 3) '(1 2 3)]

                   ;; repeat - inside a list
                   {:type :repeat
                    :coll-type :list
                    :min 0
                    :max 2
                    :model {:type :fn
                            :fn int?}}
                   ['() '(1) '(2 3)]
                   [[] [1] [1 2] [1 2 3] '(1 2 3)]

                   ;; repeat - min > 0
                   {:type :repeat
                    :min 2
                    :max 3
                    :model {:type :fn
                            :fn int?}}
                   [[1 2] [1 2 3]]
                   [[] [1] [1 2 3 4]]

                   ;; repeat - max = +Infinity
                   {:type :repeat
                    :min 2
                    :max ##Inf
                    :model {:type :fn
                            :fn int?}}
                   [[1 2] [1 2 3] [1 2 3 4]]
                   [[] [1]]

                   ;; repeat - of a cat
                   {:type :repeat
                    :min 1
                    :max 2
                    :model {:type :cat
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
                    :model {:type :cat
                            :inlined false
                            :entries [{:model {:type :fn
                                               :fn int?}}
                                      {:model {:type :fn
                                               :fn string?}}]}}
                   [[[1 "a"]] [[1 "a"] [2 "b"]] ['(1 "a") [2 "b"]]]
                   [[] [1] [1 2] [1 "a"] [1 "a" 2 "b"] [1 "a" 2 "b" 3 "c"]]

                   ;; let
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




(defn- trimmed-description [description]
  (clojure.walk/postwalk
    (fn [x]
      (if (and (map? x)
               (every? (partial contains? x) [:context :model]))
        (-> x
            (dissoc :context)
            (update :model select-keys [:type]))
        x))
    description))

(comment
  (-> (describe {:type :and
                 :entries [{:model {:type :fn
                                    :fn vector?}}
                           {:model {:type :fn
                                    :fn #(= (count %) 3)}}]}
                [:a :b :c])
      (trimmed-description))

  (-> (describe {:type :fn
                 :fn #(= 1 %)}
                1)
      (trimmed-description)))

(deftest describe-test
  (let [test-data []];; fn
                   ;{:type :fn
                   ; :fn #(= 1 %)}
                   ;[[1 {:data 1
                   ;     :valid? true}]]]]
                   ;
                   ;;; enum
                   ;{:type :enum
                   ; :values #{1 "2" :3}}
                   ;[[[1 "2" :3] [1 "2" :3]]]
                   ;
                   ;;; and
                   ;{:type :and
                   ; :entries [{:model {:type :fn
                   ;                    :fn vector?}}
                   ;           {:model {:type :fn
                   ;                    :fn #(= (count %) 3)}}]}
                   ;[[[:a :b :c]
                   ;  {:data [:a :b :c],
                   ;   :model {:type :and}}]]
                   ;
                   ;;; or
                   ;{:type :or
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :fn
                   ;                    :fn string?}}]}
                   ;[[7 [7]]
                   ; ["coco" ["coco"]]]]]
                   ;
                   ;;; map
                   ;{:type :map
                   ; :entries [{:key :a
                   ;            :model {:type :fn
                   ;                    :fn int?}}
                   ;           {:key :b
                   ;            :model {:type :or
                   ;                    :entries [{:key :id
                   ;                               :model {:type :fn
                   ;                                       :fn int?}}
                   ;                              {:key :name
                   ;                               :model {:type :fn
                   ;                                       :fn string?}}]}}]}
                   ;[[{:a 1, :b 2} [{:a 1, :b [:id 2]}]]
                   ; [{:a 1, :b "foo"} [{:a 1, :b [:name "foo"]}]]]
                   ;
                   ;;; map with multiple matches on its values
                   ;{:type :map
                   ; :entries [{:key :number
                   ;            :model {:type :or
                   ;                    :entries [{:key :id
                   ;                               :model {:type :fn
                   ;                                       :fn int?}}
                   ;                              {:key :age
                   ;                               :model {:type :fn
                   ;                                       :fn int?}}]}}
                   ;           {:key :text
                   ;            :model {:type :or
                   ;                    :entries [{:key :title
                   ;                               :model {:type :fn
                   ;                                       :fn string?}}
                   ;                              {:key :description
                   ;                               :model {:type :fn
                   ;                                       :fn string?}}]}}]}
                   ;[[{:number 20, :text "hi"} [{:number [:id
                   ;                                      20]
                   ;                             :text   [:title
                   ;                                      "hi"]}
                   ;                            {:number [:id
                   ;                                      20]
                   ;                             :text   [:description
                   ;                                      "hi"]}
                   ;                            {:number [:age
                   ;                                      20]
                   ;                             :text   [:title
                   ;                                      "hi"]}
                   ;                            {:number [:age
                   ;                                      20]
                   ;                             :text   [:description
                   ;                                      "hi"]}]]
                   ; [{:number "foo"} []]]]]
                   ;
                   ;;; map-of
                   ;{:type :map-of
                   ; :key {:model {:type :fn
                   ;               :fn keyword?}}
                   ; :value {:model {:type :fn
                   ;                 :fn int?}}}
                   ;[[{:a 1, :b 2} {:a 1, :b 2}]]
                   ;
                   ;;; sequence
                   ;{:type :sequence
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[['(1 2 3) [1 2 3]]]
                   ;
                   ;;; list
                   ;{:type :list
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[['(1 2 3) [1 2 3]]]
                   ;
                   ;;; vector
                   ;{:type :vector
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[[[1 2 3] [1 2 3]]]
                   ;
                   ;;; set
                   ;{:type :set
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[[#{1} [1]]]
                   ;
                   ;;; tuple
                   ;{:type :tuple
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :fn
                   ;                    :fn string?}}]}
                   ;[['(1 "2") [1 "2"]]]
                   ;
                   ;;; let
                   ;{:type :let
                   ; :bindings {'pos-even? {:type :and
                   ;                        :entries [{:model {:type :fn
                   ;                                           :fn pos-int?}}
                   ;                                  {:model {:type :fn
                   ;                                           :fn even?}}]}}
                   ; :body {:type :ref
                   ;        :ref 'pos-even?}}
                   ;[[[2 4] [2 4]]]]]
                   ;
                   ;;; cat of cat, the inner cat is implicitly inlined
                   ;{:type :cat
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :cat
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn int?}}]}}]}
                   ;[[1 2]]
                   ;[[1] [1 [2]]]
                   ;
                   ;;; cat of cat, the inner cat is explicitly not inlined
                   ;{:type :cat
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :cat
                   ;                    :inlined false
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn int?}}]}}]}
                   ;[[1 [2]]]
                   ;[[1] [1 2]]
                   ;
                   ;;; alt
                   ;{:type :alt
                   ; :entries [{:model {:type :fn
                   ;                    :fn int?}}
                   ;           {:model {:type :cat
                   ;                    :entries [{:model {:type :fn
                   ;                                       :fn string?}}]}}]}
                   ;[[1] ["1"]]
                   ;[1 "1" :1 [:1]]
                   ;
                   ;;; repeat
                   ;{:type :repeat
                   ; :min 0
                   ; :max 2
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[[] [1] [1 2]]
                   ;[[1 2 3] [1 2 3 4]]
                   ;
                   ;{:type :repeat
                   ; :min 2
                   ; :max 3
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[[1 2] [1 2 3]]
                   ;[[] [1] [1 2 3 4]]
                   ;
                   ;{:type :repeat
                   ; :min 2
                   ; :max ##Inf
                   ; :model {:type :fn
                   ;         :fn int?}}
                   ;[[1 2] [1 2 3] [1 2 3 4]]
                   ;[[] [1]]
                   ;
                   ;{:type :repeat
                   ; :min 1
                   ; :max 2
                   ; :model {:type :cat
                   ;         :entries [{:model {:type :fn
                   ;                            :fn int?}}
                   ;                   {:model {:type :fn
                   ;                            :fn string?}}]}}
                   ;[[1 "a"] [1 "a" 2 "b"]]
                   ;[[] [1] [1 2] [1 "a" 2 "b" 3 "c"]]]]

    (doseq [[model data-description-pairs] (partition 2 test-data)]
      (doseq [[data description] data-description-pairs]
        (is (= (trimmed-description (describe model data))
               description))))))
