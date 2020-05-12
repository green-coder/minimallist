(ns minimallist.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [minimallist.core :refer [valid? explain describe undescribe] :as m]
            [minimallist.helper :as h]
            [minimallist.util :as util]))

(comment
  (#'m/sequence-descriptions {}
    ; [:cat [:+ pos-int?]
    ;       [:+ int?]]
    (h/cat (h/+ (h/fn pos-int?))
           (h/+ (h/fn int?)))
    [3 4 0 2])

  (#'m/sequence-descriptions {}
    ; [:repeat {:min 0, :max 2} int?]
    (h/repeat 0 2 (h/fn int?))
    (seq [1 2]))

  (#'m/sequence-descriptions {}
    ; [:alt [:ints     [:repeat {:min 0, :max 2} int?]]
    ;       [:keywords [:repeat {:min 0, :max 2} keyword?]]
    (h/alt [:ints (h/repeat 0 2 (h/fn int?))]
           [:keywords (h/repeat 0 2 (h/fn keyword?))])
    (seq [1 2]))

  (#'m/sequence-descriptions {}
    ; [:* int?]
    (h/* (h/fn int?))
    (seq [1 :2]))

  (#'m/sequence-descriptions {}
    ; [:+ int?]
    (h/+ (h/fn int?))
    (seq [1 2 3])))

(deftest valid?-test
  (let [test-data [;; fn
                   (h/fn #(= 1 %))
                   [1]
                   [2]

                   ;; enum
                   (h/enum #{1 "2" :3})
                   [1 "2" :3]
                   [[1] 2 true false nil]

                   (h/enum #{nil false})
                   [nil false]
                   [true '()]

                   ;; and
                   (h/and (h/fn pos-int?)
                          (h/fn even?))
                   [2 4 6]
                   [0 :a -1 1 3]

                   ;; or
                   (h/or (h/fn pos-int?)
                         (h/fn even?))
                   [-2 0 1 2 3]
                   [-3 -1]

                   ;; set
                   (-> (h/set-of (h/fn int?))
                       (h/with-count (h/enum #{2 3})))
                   [#{1 2} #{1 2 3}]
                   [#{1 :a} [1 2 3] '(1 2) `(1 ~2) #{1} #{1 2 3 4}]

                   ;; map, entries
                   (h/map [:a (h/fn int?)]
                          [:b {:optional true} (h/fn string?)]
                          [(list 1 2 3) (h/fn string?)])
                   [{:a 1, :b "foo", (list 1 2 3) "you can count on me like ..."}
                    {:a 1, :b "bar", [1 2 3] "soleil !"}
                    {:a 1, [1 2 3] "soleil !"}]
                   [{:a 1, :b "foo"}
                    {:a 1, :b "foo", #{1 2 3} "bar"}
                    {:a 1, :b 'bar, [1 2 3] "soleil !"}]

                   ;; map, keys and values
                   (h/map-of (h/fn keyword?)
                             (h/fn int?))
                   [{} {:a 1, :b 2}]
                   [{:a 1, :b "2"} [[:a 1] [:b 2]] {true 1, false 2}]

                   ;; sequence, no collection type specified
                   (h/sequence-of (h/fn int?))
                   ['(1 2 3) [1 2 3] `(1 2 ~3)]
                   ['(1 :a) #{1 2 3} {:a 1, :b 2, :c 3}]

                   ;; sequence, with condition
                   (-> (h/sequence-of (h/fn int?))
                       (h/with-condition (h/fn (fn [coll] (= coll (reverse coll))))))
                   [[1] '(1 1) '[1 2 1]]
                   ['(1 2) '(1 2 3)]

                   ;; sequence as a list
                   (h/list-of (h/fn int?))
                   ['(1 2 3)]
                   ['(1 :a) [1 2 3] #{1 2 3}
                    #_`(1 2 ~3)]                            ; this is not a list in cljs

                   ;; sequence as a vector
                   (h/vector-of (h/fn int?))
                   [[1 2 3]]
                   [[1 :a] '(1 2 3) #{1 2 3} `(1 2 ~3)]

                   ;; sequence with size specified using a model
                   (-> (h/sequence) (h/with-count (h/enum #{2 3})))
                   ['(1 2) [1 "2"] `(1 ~"2") [1 "2" :3]]
                   [#{1 "a"} [1 "2" :3 :4]]

                   ;; sequence with entries (fixed size is implied)
                   (h/tuple (h/fn int?) (h/fn string?))
                   ['(1 "2") [1 "2"] `(1 ~"2")]
                   [#{1 "a"} [1 "2" :3]]

                   ;; alt
                   (h/alt [:int (h/fn int?)]
                          [:strings (h/cat (h/fn string?))])
                   [1 ["1"]]
                   [[1] "1" :1 [:1]]

                   ;; alt - inside a cat
                   (h/cat (h/fn int?)
                          (h/alt [:string (h/fn string?)]
                                 [:keyword (h/fn keyword?)]
                                 [:string-keyword (h/cat (h/fn string?)
                                                         (h/fn keyword?))])
                          (h/fn int?))
                   [[1 "2" 3] [1 :2 3] [1 "a" :b 3]]
                   [[1 ["a" :b] 3]]

                   ;; alt - inside a cat, but with :inline false on its cat entry
                   (h/cat (h/fn int?)
                          (h/alt [:string (h/fn string?)]
                                 [:keyword (h/fn keyword?)]
                                 [:string-keyword (-> (h/cat (h/fn string?)
                                                             (h/fn keyword?))
                                                      (h/not-inlined))])
                          (h/fn int?))
                   [[1 "2" 3] [1 :2 3] [1 ["a" :b] 3]]
                   [[1 "a" :b 3]]

                   ;; cat of cat, the inner cat is implicitly inlined
                   (-> (h/cat (h/fn int?)
                              (h/cat (h/fn int?)))
                       (h/in-vector))
                   [[1 2]]
                   [[1] [1 [2]] [1 2 3] '(1) '(1 2) '(1 (2)) '(1 2 3)]

                   ;; cat of cat, the inner cat is explicitly not inlined
                   (-> (h/cat (h/fn int?)
                              (-> (h/cat (h/fn int?))
                                  (h/not-inlined))))
                   [[1 [2]] '[1 (2)] '(1 (2))]
                   [[1] [1 2] [1 [2] 3]]

                   ;; repeat - no collection type specified
                   (h/repeat 0 2 (h/fn int?))
                   [[] [1] [1 2] '() '(1) '(2 3)]
                   [[1 2 3] '(1 2 3)]

                   ;; repeat - inside a vector
                   (h/in-vector (h/repeat 0 2 (h/fn int?)))
                   [[] [1] [1 2]]
                   [[1 2 3] '() '(1) '(2 3) '(1 2 3)]

                   ;; repeat - inside a list
                   (h/in-list (h/repeat 0 2 (h/fn int?)))
                   ['() '(1) '(2 3)]
                   [[] [1] [1 2] [1 2 3] '(1 2 3)]

                   ;; repeat - min > 0
                   (h/repeat 2 3 (h/fn int?))
                   [[1 2] [1 2 3]]
                   [[] [1] [1 2 3 4]]

                   ;; repeat - max = +Infinity
                   (h/repeat 2 ##Inf (h/fn int?))
                   [[1 2] [1 2 3] [1 2 3 4]]
                   [[] [1]]

                   ;; repeat - of a cat
                   (h/repeat 1 2 (h/cat (h/fn int?)
                                        (h/fn string?)))
                   [[1 "a"] [1 "a" 2 "b"]]
                   [[] [1] [1 2] [1 "a" 2 "b" 3 "c"]]

                   ;; repeat - of a cat with :inlined false
                   (h/repeat 1 2 (-> (h/cat (h/fn int?)
                                            (h/fn string?))
                                     (h/not-inlined)))
                   [[[1 "a"]] [[1 "a"] [2 "b"]] ['(1 "a") [2 "b"]]]
                   [[] [1] [1 2] [1 "a"] [1 "a" 2 "b"] [1 "a" 2 "b" 3 "c"]]

                   ;; let / ref
                   (h/let ['pos-even? (h/and (h/fn pos-int?)
                                             (h/fn even?))]
                          (h/ref 'pos-even?))
                   [2 4]
                   [-2 -1 0 1 3]

                   ;; let / ref - with structural recursion
                   (h/let ['hiccup (h/alt
                                     [:node (h/in-vector (h/cat (h/fn keyword?)
                                                                (h/? (h/map))
                                                                (h/* (h/not-inlined (h/ref 'hiccup)))))]
                                     [:primitive (h/or (h/fn nil?)
                                                       (h/fn boolean?)
                                                       (h/fn number?)
                                                       (h/fn string?))])]
                          (h/ref 'hiccup))
                   [nil
                    false
                    1
                    "hi"
                    [:div]
                    [:div {}]
                    [:div "hei" [:p "bonjour"]]
                    [:div {:a 1} "hei" [:p "bonjour"]]]
                   [{}
                    {:a 1}
                    ['div]
                    [:div {:a 1} "hei" [:p {} {} "bonjour"]]]

                   ;; let / ref - with recursion within a sequence
                   (h/let ['foo (h/cat (h/fn int?)
                                       (h/? (h/ref 'foo))
                                       (h/fn string?))]
                          (h/ref 'foo))
                   [[1 "hi"]
                    [1 1 "hi" "hi"]
                    [1 1 1 "hi" "hi" "hi"]]
                   [[1 1 "hi"]
                    [1 "hi" "hi"]
                    [1 1 :no "hi" "hi"]]]]


    (doseq [[model valid-coll invalid-coll] (partition 3 test-data)]
      (doseq [data valid-coll]
        (is (valid? model data)))
      (doseq [data invalid-coll]
        (is (not (valid? model data)))))))


(deftest describe-test
  (let [test-data [;; fn
                   (h/fn #(= 1 %))
                   #{:context :model :data}
                   [1 {:valid? true}
                    2 {:valid? false}]

                   ;; enum
                   (h/enum #{1 "2" false nil})
                   #{:context :model :data}
                   [1 {:valid? true}
                    "2" {:valid? true}
                    false {:valid? true}
                    nil {:valid? true}
                    true {:valid? false}]

                   ;; and
                   (h/and (h/fn pos-int?)
                          (h/fn even?))
                   #{:context :model :data}
                   [0 {:valid? false}
                    1 {:valid? false}
                    2 {:valid? true}
                    3 {:valid? false}
                    4 {:valid? true}]

                   ;; or
                   (h/or (h/fn int?)
                         (h/fn string?))
                   #{:context :model :data}
                   [1 {:valid? true}
                    "a" {:valid? true}
                    :a {:valid? false}]

                   ;; set
                   (h/set-of (h/fn int?))
                   #{:context :model :data}
                   [#{1} {:entries #{{:valid? true}}
                          :valid?  true}]

                   ;; map
                   (h/map [:a {:optional true} (h/fn int?)]
                          [:b (h/or (h/fn int?)
                                    (h/fn string?))])
                   #{:context :model :data}
                   [{:a 1, :b 2} {:entries {:a {:valid? true}
                                            :b {:valid? true}}
                                  :valid?  true}
                    {:a 1, :b "foo"} {:entries {:a {:valid? true}
                                                :b {:valid? true}}
                                      :valid?  true}
                    {:a 1, :b [1 2]} {:entries {:a {:valid? true}
                                                :b {:valid? false}}
                                      :valid?  false}
                    ; missing optional entry
                    {:b 2} {:entries {:b {:valid? true}}
                            :valid?  true}
                    ; missing entry
                    {:a 1} {:entries {:a {:valid? true}
                                      :b {:missing? true}}
                            :valid?  false}
                    ; extra entry
                    {:a 1, :b 2, :c 3} {:entries {:a {:valid? true}
                                                  :b {:valid? true}}
                                        :valid?  true}]

                   ;; map - :keys
                   (-> (h/map) (h/with-keys (h/fn keyword?)))
                   #{:context :model :data}
                   [{:a 1, :b 2} {:valid? true}
                    {"a" 1} {:valid? false}]

                   ;; map - :values
                   (-> (h/map) (h/with-values (h/fn int?)))
                   #{:context :model :data}
                   [{:a 1, "b" 2} {:valid? true}
                    {:a "1"} {:valid? false}]

                   ;; sequence - :elements-model
                   (h/sequence-of (h/fn int?))
                   #{:context :model :data}
                   [[1 2 3] {:entries [{:valid? true}
                                       {:valid? true}
                                       {:valid? true}]
                             :valid?  true}
                    '(1 2 3) {:entries [{:valid? true}
                                        {:valid? true}
                                        {:valid? true}]
                              :valid?  true}
                    [1 "2" 3] {:entries [{:valid? true}
                                         {:valid? false}
                                         {:valid? true}]
                               :valid?  false}]

                   ;; sequence - :elements-model with condition
                   (-> (h/sequence-of (h/fn int?))
                       (h/with-condition (h/fn (fn [coll] (= coll (reverse coll))))))
                   #{:context :model :data}
                   [[1 2 1] {:entries [{:valid? true}
                                       {:valid? true}
                                       {:valid? true}]
                             :valid?  true}
                    '(1 2 3) {:entries [{:valid? true}
                                        {:valid? true}
                                        {:valid? true}]
                              :valid? false}]

                   ;; sequence - :coll-type vector
                   (-> (h/sequence) (h/in-vector))
                   #{:context :model :data}
                   [[1 2 3] {:valid? true}
                    '(1 2 3) {:valid? false}]

                   ;; sequence - :entries
                   (h/tuple (h/fn int?) (h/fn string?))
                   #{:context :model :data}
                   [[1 "a"] {:entries [{:valid? true}
                                       {:valid? true}]
                             :valid?  true}
                    [1 2] {:entries [{:valid? true}
                                     {:valid? false}]
                           :valid?  false}
                    [1] {:entries [{:valid? true}]
                         :valid?  false}]

                   ;; sequence - :count-model
                   (-> (h/sequence) (h/with-count (h/val 3)))
                   #{:context :model :data}
                   [[1 2] {:valid? false}
                    [1 2 3] {:valid? true}
                    [1 2 3 4] {:valid? false}]

                   ;; alt - not inside a sequence
                   (h/alt [:number (h/fn int?)]
                          [:sequence (h/cat (h/fn string?))])
                   #{:context :model :data}
                   [1 {:valid? true
                       :key    :number
                       :entry  {:valid? true}}
                    ["1"] {:valid? true
                           :key    :sequence
                           :entry  {:valid?  true
                                    :entries [{:valid? true}]}}
                    [1] {:valid? false}
                    "1" {:valid? false}]

                   ;; alt - inside a cat
                   (h/cat (h/fn int?)
                          (h/alt [:option1 (h/fn string?)]
                                 [:option2 (h/fn keyword?)]
                                 [:option3 (h/cat (h/fn string?)
                                                  (h/fn keyword?))])
                          (h/fn int?))
                   #{:context :model :data}
                   [[1 "2" 3] {:valid?  true
                               :entries [{:valid? true}
                                         {:key   :option1
                                          :entry {:valid? true}}
                                         {:valid? true}]}
                    [1 :2 3] {:valid?  true
                              :entries [{:valid? true}
                                        {:key   :option2
                                         :entry {:valid? true}}
                                        {:valid? true}]}
                    [1 "a" :b 3] {:valid?  true
                                  :entries [{:valid? true}
                                            {:key   :option3
                                             :entry {:entries [{:valid? true}
                                                               {:valid? true}]}}
                                            {:valid? true}]}
                    [1 ["a" :b] 3] {:valid? false}]

                   ;; alt - inside a cat, but with :inline false on its cat entry
                   (h/cat (h/fn int?)
                          (h/alt [:option1 (h/fn string?)]
                                 [:option2 (h/fn keyword?)]
                                 [:option3 (h/not-inlined (h/cat (h/fn string?)
                                                                 (h/fn keyword?)))])
                          (h/fn int?))
                   #{:context :model :data}
                   [[1 "2" 3] {:valid?  true
                               :entries [{:valid? true}
                                         {:key   :option1
                                          :entry {:valid? true}}
                                         {:valid? true}]}
                    [1 :2 3] {:valid?  true
                              :entries [{:valid? true}
                                        {:key   :option2
                                         :entry {:valid? true}}
                                        {:valid? true}]}
                    [1 "a" :b 3] {:valid? false}
                    [1 ["a" :b] 3] {:valid?  true
                                    :entries [{:valid? true}
                                              {:key   :option3
                                               :entry {:valid?  true
                                                       :entries [{:valid? true}
                                                                 {:valid? true}]}}
                                              {:valid? true}]}]

                   ;; cat of cat, the inner cat is implicitly inlined
                   (h/cat (h/fn int?)
                          (h/cat (h/fn int?)))
                   #{:context :model :data}
                   [[1 2] {:valid?  true
                           :entries [{:valid? true}
                                     {:entries [{:valid? true}]}]}
                    [1] {:valid? false}
                    [1 [2]] {:valid? false}
                    [1 2 3] {:valid? false}]

                   ;; cat of cat, the inner cat is explicitly not inlined
                   (h/cat (h/fn int?)
                          (h/not-inlined (h/cat (h/fn int?))))
                   #{:context :model :data}
                   [[1 [2]] {:valid?  true
                             :entries [{:valid? true}
                                       {:valid?  true
                                        :entries [{:valid? true}]}]}
                    [1 '(2)] {:valid?  true
                              :entries [{:valid? true}
                                        {:valid?  true
                                         :entries [{:valid? true}]}]}
                    [1] {:valid? false}
                    [1 2] {:valid? false}
                    [1 [2] 3] {:valid? false}]

                   ;; repeat - no collection type specified
                   (h/repeat 0 2 (h/fn int?))
                   #{:context :model :data}
                   [[] {:entries []
                        :valid?  true}
                    [1] {:entries [{:valid? true}]
                         :valid?  true}
                    [1 2] {:entries [{:valid? true}
                                     {:valid? true}]
                           :valid?  true}
                    '() {:entries []
                         :valid?  true}
                    '(1) {:entries [{:valid? true}]
                          :valid?  true}
                    '(2 3) {:entries [{:valid? true}
                                      {:valid? true}]
                            :valid?  true}
                    [1 2 3] {:valid? false}
                    '(1 2 3) {:valid? false}]

                   ;; repeat - inside a vector
                   (-> (h/repeat 0 2 (h/fn int?))
                       (h/in-vector))
                   #{:context :model :data}
                   [[1] {:valid?  true
                         :entries [{:valid? true}]}
                    '(1) {:valid? false}]

                   ;; repeat - inside a list
                   (-> (h/repeat 0 2 (h/fn int?))
                       (h/in-list))
                   #{:context :model :data}
                   [[1] {:valid? false}
                    '(1) {:valid?  true
                          :entries [{:valid? true}]}]

                   ;; repeat - min > 0
                   (h/repeat 2 3 (h/fn int?))
                   #{:context :model :data}
                   [[] {:valid? false}
                    [1] {:valid? false}
                    [1 2] {:valid?  true
                           :entries [{:valid? true}
                                     {:valid? true}]}
                    [1 2 3] {:valid?  true
                             :entries [{:valid? true}
                                       {:valid? true}
                                       {:valid? true}]}
                    [1 2 3 4] {:valid? false}]

                   ;; repeat - max = +Infinity
                   (h/repeat 2 ##Inf (h/fn int?))
                   #{:context :model}
                   [[] {:valid? false, :data []}
                    [1] {:valid? false, :data [1]}
                    [1 2] {:valid?  true
                           :data    [1 2]
                           :entries [{:valid? true
                                      :data   1}
                                     {:valid? true
                                      :data   2}]}
                    [1 2 3] {:valid?  true
                             :data    [1 2 3]
                             :entries [{:valid? true
                                        :data   1}
                                       {:valid? true
                                        :data   2}
                                       {:valid? true
                                        :data   3}]}]

                   ;; repeat - of a cat
                   (h/repeat 1 2 (h/cat (h/fn int?)
                                        (h/fn string?)))
                   #{:context :model}
                   [[1 "a"] {:valid?  true
                             :data    [1 "a"]
                             :entries [{:entries [{:valid? true
                                                   :data   1}
                                                  {:valid? true
                                                   :data   "a"}]}]}
                    [1 "a" 2 "b"] {:valid?  true
                                   :data    [1 "a" 2 "b"]
                                   :entries [{:entries [{:valid? true
                                                         :data   1}
                                                        {:valid? true
                                                         :data   "a"}]}
                                             {:entries [{:valid? true
                                                         :data   2}
                                                        {:valid? true
                                                         :data   "b"}]}]}
                    [] {:data   []
                        :valid? false}
                    [1] {:data   [1]
                         :valid? false}
                    [1 2] {:valid? false
                           :data   [1 2]}
                    [1 "a" 2 "b" 3 "c"] {:valid? false
                                         :data   [1 "a" 2 "b" 3 "c"]}]

                   ;; repeat - of a cat with :inlined false
                   (h/repeat 1 2 (h/not-inlined (h/cat (h/fn int?)
                                                       (h/fn string?))))
                   #{:context :model :data}
                   [[[1 "a"]] {:valid?  true
                               :entries [{:valid?  true
                                          :entries [{:valid? true}
                                                    {:valid? true}]}]}
                    [[1 "a"] [2 "b"]] {:valid?  true
                                       :entries [{:valid?  true
                                                  :entries [{:valid? true}
                                                            {:valid? true}]}
                                                 {:valid?  true
                                                  :entries [{:valid? true}
                                                            {:valid? true}]}]}
                    ['(1 "a") [2 "b"]] {:valid?  true
                                        :entries [{:valid?  true
                                                   :entries [{:valid? true}
                                                             {:valid? true}]}
                                                  {:valid?  true
                                                   :entries [{:valid? true}
                                                             {:valid? true}]}]}
                    [] {:valid? false}
                    [1] {:valid? false}
                    [1 2] {:valid? false}
                    [1 "a"] {:valid? false}
                    [1 "a" 2 "b"] {:valid? false}
                    [1 "a" 2 "b" 3 "c"] {:valid? false}]

                   ;; let / ref
                   (h/let ['pos-even? (h/and (h/fn pos-int?)
                                             (h/fn even?))]
                          (h/ref 'pos-even?))
                   #{:context :model :data}
                   [0 {:valid? false}
                    1 {:valid? false}
                    2 {:valid? true}
                    3 {:valid? false}
                    4 {:valid? true}]]]

    (doseq [[model keys-to-remove data-description-pairs] (partition 3 test-data)]
      (doseq [[data description] (partition 2 data-description-pairs)]
        (is (= [data (util/cleanup-description (describe model data) keys-to-remove)]
               [data description]))))))
