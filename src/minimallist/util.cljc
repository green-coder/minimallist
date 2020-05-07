(ns minimallist.util
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

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
