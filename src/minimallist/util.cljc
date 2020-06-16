(ns minimallist.util
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

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

(defn reduce-mapv [f acc coll]
  (reduce (fn [[acc v] elm]
            (let [[updated-acc updated-elm] (f acc elm)]
              [updated-acc (conj v updated-elm)]))
          [acc []]
          coll))

(defn walk-map-dissoc [expr & keys-to-dissoc]
  (walk/postwalk (fn [expr]
                  (if (map? expr)
                    (apply dissoc expr keys-to-dissoc)
                    expr))
               expr))

(defn cleanup-description [description keys-to-remove]
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
