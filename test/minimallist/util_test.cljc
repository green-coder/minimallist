(ns minimallist.util-test
  (:require [clojure.test :refer [deftest testing is are]]
            [minimallist.util :as util]
            [minimallist.helper :as h]))

(deftest reduce-update-test
  (let [m {:a 1
           :b 5}
        f (fn [acc elm]
            (let [elm10 (* elm 10)]
              [(conj acc elm10) elm10]))]
    (is (= (-> [[] m]
               (util/reduce-update :a f)
               (util/reduce-update :b f))
           [[10 50] {:a 10, :b 50}]))))

(deftest reduce-update-in-test
  (let [m {:a {:x 1, :y 2}
           :b [3 4 5]}
        f (fn [acc elm]
            (let [elm10 (* elm 10)]
              [(conj acc elm10) elm10]))]
    (is (= (-> [[] m]
               (util/reduce-update-in [:a :x] f)
               (util/reduce-update-in [:b 2] f))
           [[10 50] {:a {:x 10, :y 2}, :b [3 4 50]}]))))

(deftest reduce-mapv
  (let [m {:a {:x 1, :y 2}
           :b [3 4 5]}
        f (fn [acc elm]
            (let [elm10 (* elm 10)]
              [(conj acc elm10) elm10]))]
    (is (= (util/reduce-update [[] m] :b (partial util/reduce-mapv f))
           [[30 40 50] {:a {:x 1, :y 2}, :b [30 40 50]}]))))
