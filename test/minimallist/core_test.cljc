(ns minimallist.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [minimallist.core :refer [valid? explain conform unform]]
            [clojure.spec.alpha :as s]))

(deftest valid?-test
  (let [test-data [;; and
                   {:type :and
                    :entries [{:model {:type :fn
                                       :fn vector?}}
                              {:model {:type :fn
                                       :fn #(= (count %) 3)}}]}
                   [[:a :b :c]]
                   [[:a :b] '(:a :b) #{:a}]

                   ;; or
                   {:type :or
                    :entries [{:model {:type :fn
                                       :fn vector?}}
                              {:model {:type :fn
                                       :fn #(= (count %) 3)}}]}
                   [[:a :b]
                    [:a :b :c] '(:a :b :c) #{:a :b :c} {:a 1, :b 2, :c 3}]
                   ['(:a :b) #{:a :b}]

                   ;; map
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
                   [{:a 1, :b "foo"} {:a 1, :b "foo", #{1 2 3} "bar"}]

                   ;; map-of
                   {:type :map-of
                    :key {:model {:type :fn
                                  :fn keyword?}}
                    :value {:model {:type :fn
                                    :fn int?}}}
                   [{} {:a 1, :b 2}]
                   [{:a 1, :b "2"} [[:a 1] [:b 2]] {true 1, false 2}]

                   ;; coll-of
                   {:type :coll-of
                    :model {:type :fn
                            :fn int?}}
                   ['(1 2 3) [1 2 3] `(1 2 ~3) #{1 2 3}]
                   ['(1 :a) {:a 1, :b 2, :c 3}]

                   ;; sequence
                   {:type :sequence
                    :model {:type :fn
                            :fn int?}}
                   ['(1 2 3) [1 2 3] `(1 2 ~3)]
                   ['(1 :a) #{1 2 3} {:a 1, :b 2, :c 3}]

                   ;; list
                   {:type :list
                    :model {:type :fn
                            :fn int?}}
                   ['(1 2 3)]
                   ['(1 :a) [1 2 3] #{1 2 3}
                    #_`(1 2 ~3)] ; this is not a list in cljs]

                   ;; vector
                   {:type :vector
                    :model {:type :fn
                            :fn int?}}
                   [[1 2 3]]
                   [[1 :a] '(1 2 3) #{1 2 3} `(1 2 ~3)]

                   ;; set
                   {:type :set
                    :model {:type :fn
                            :fn int?}}
                   [#{1 2 3}]
                   [#{1 :a} [1 2 3] '(1 2 3) `(1 2 ~3)]

                   ;; tuple
                   {:type :tuple
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :fn
                                       :fn string?}}]}
                   ['(1 "2") [1 "2"] `(1 ~"2")]
                   [#{1 "a"} [1 "2" :3]]

                   ;; enum
                   {:type :enum
                    :values #{1 "2" :3}}
                   [1 "2" :3]
                   [[1] 2 true false nil]

                   {:type :enum
                    :values #{nil false}}
                   [nil false]
                   [true '()]

                   ;; fn
                   {:type :fn
                    :fn #(= 1 %)}
                   [1]
                   [2]

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
                   [-2 -1 0 1 3]

                   ;; cat of cat, the inner cat is implicitly inlined
                   {:type :cat
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :cat
                                       :entries [{:model {:type :fn
                                                          :fn int?}}]}}]}
                   [[1 2]]
                   [[1] [1 [2]]]

                   ;; cat of cat, the inner cat is explicitly not inlined
                   {:type :cat
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :cat
                                       :inlined false
                                       :entries [{:model {:type :fn
                                                          :fn int?}}]}}]}
                   [[1 [2]]]
                   [[1] [1 2]]

                   ;; alt
                   {:type :alt
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :cat
                                       :entries [{:model {:type :fn
                                                          :fn string?}}]}}]}
                   [[1] ["1"]]
                   [1 "1" :1 [:1]]

                   ;; repeat
                   {:type :repeat
                    :min 0
                    :max 2
                    :model {:type :fn
                            :fn int?}}
                   [[] [1] [1 2]]
                   [[1 2 3] [1 2 3 4]]

                   {:type :repeat
                    :min 2
                    :max 3
                    :model {:type :fn
                            :fn int?}}
                   [[1 2] [1 2 3]]
                   [[] [1] [1 2 3 4]]

                   {:type :repeat
                    :min 2
                    :max ##Inf
                    :model {:type :fn
                            :fn int?}}
                   [[1 2] [1 2 3] [1 2 3 4]]
                   [[] [1]]

                   {:type :repeat
                    :min 1
                    :max 2
                    :model {:type :cat
                            :entries [{:model {:type :fn
                                               :fn int?}}
                                      {:model {:type :fn
                                               :fn string?}}]}}
                   [[1 "a"] [1 "a" 2 "b"]]
                   [[] [1] [1 2] [1 "a" 2 "b" 3 "c"]]]]

    (doseq [[model valid-coll invalid-coll] (partition 3 test-data)]
      (doseq [data valid-coll]
        (is (valid? model data)))
      (doseq [data invalid-coll]
        (is (not (valid? model data)))))))

(deftest conform-test
  (let [test-data [;; and
                   {:type :and
                    :entries [{:model {:type :fn
                                       :fn vector?}}
                              {:model {:type :fn
                                       :fn #(= (count %) 3)}}]}
                   [[[:a :b :c] [[:a :b :c]]]]

                   ;; or
                   {:type :or
                    :entries [{:model {:type :fn
                                       :fn int?}}
                              {:model {:type :fn
                                       :fn string?}}]}
                   [[7 [7]]
                    ["coco" ["coco"]]]

                   ;; or with keys
                   {:type :or
                    :entries [{:key :id
                               :model {:type :fn
                                       :fn int?}}
                              {:key :name
                               :model {:type :fn
                                       :fn string?}}]}
                   [[7 [[:id 7]]]
                    ["coco" [[:name "coco"]]]]

                   ;; map
                   {:type :map
                    :entries [{:key :a
                               :model {:type :fn
                                       :fn int?}}
                              {:key :b
                               :model {:type :or
                                       :entries [{:key :id
                                                  :model {:type :fn
                                                          :fn int?}}
                                                 {:key :name
                                                  :model {:type :fn
                                                          :fn string?}}]}}]}
                   [[{:a 1, :b 2} [{:a 1, :b [:id 2]}]]
                    [{:a 1, :b "foo"} [{:a 1, :b [:name "foo"]}]]]

                   ;; map with multiple matches on its values
                   {:type :map
                    :entries [{:key :number
                               :model {:type :or
                                       :entries [{:key :id
                                                  :model {:type :fn
                                                          :fn int?}}
                                                 {:key :age
                                                  :model {:type :fn
                                                          :fn int?}}]}}
                              {:key :text
                               :model {:type :or
                                       :entries [{:key :title
                                                  :model {:type :fn
                                                          :fn string?}}
                                                 {:key :description
                                                  :model {:type :fn
                                                          :fn string?}}]}}]}
                   [[{:number 20, :text "hi"} [{:number [:id
                                                         20]
                                                :text   [:title
                                                         "hi"]}
                                               {:number [:id
                                                         20]
                                                :text   [:description
                                                         "hi"]}
                                               {:number [:age
                                                         20]
                                                :text   [:title
                                                         "hi"]}
                                               {:number [:age
                                                         20]
                                                :text   [:description
                                                         "hi"]}]]
                    [{:number "foo"} []]]]]

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
                   ;;; enum
                   ;{:type :enum
                   ; :values #{1 "2" :3}}
                   ;[[[1 "2" :3] [1 "2" :3]]]
                   ;
                   ;;; fn
                   ;{:type :fn
                   ; :fn #(= 1 %)}
                   ;[[1 1]]
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
        (is (= (conform model data) description))))))


(comment
  (s/def ::big-even (s/and int? even? #(> % 1000)))
  (s/valid? ::big-even :foo)
  (s/valid? ::big-even 10)
  (s/valid? ::big-even 100000)

  (s/conform))
