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

  (gen/sample (generator (-> (h/set)
                             (h/with-count (h/enum #{1 2 3 10}))
                             (h/with-condition (h/fn (comp #{1 2 3} count))))))

  (gen/sample (generator (h/map-of (h/fn {:generator gen/nat} int?)
                                   (h/fn {:generator gen/string-alphanumeric} string?))))

  (gen/sample (generator (-> (h/map [:a (h/fn {:generator gen/nat} int?)])
                             (h/with-optional-entries [:b (h/fn {:generator gen/string-alphanumeric} string?)]))))

  (gen/sample (generator (h/sequence-of (h/fn {:generator gen/nat} int?))))

  (gen/sample (generator (h/cat (h/fn {:generator gen/string-alphanumeric} string?)
                                (h/fn {:generator gen/nat} int?))))

  (gen/sample (generator (h/repeat 2 3 (h/fn {:generator gen/nat} int?))))

  (gen/sample (generator (h/repeat 2 3 (h/cat (h/fn {:generator gen/nat} int?)
                                              (h/fn {:generator gen/string-alphanumeric} string?))))))

(deftest generator-test

  (let [fn-int? (h/fn {:generator gen/nat} int?)
        fn-string? (h/fn {:generator gen/string-alphanumeric} string?)]

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
               (some (fn [element] (not (contains? element :b))) sample))))

    (let [model (h/sequence-of (h/fn {:generator gen/nat} int?))]
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
                  (gen/sample (generator model)))))))

;; TODO: test :let and :ref
;; TODO: limit the size of recursive models
