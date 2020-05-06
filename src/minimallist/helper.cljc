(ns minimallist.helper
  (:refer-clojure :exclude [fn val or and map vector-of list vector cat repeat ? * + let ref]))

;; Some helper functions to compose hash-map based models

(defn fn [predicate]
  {:type :fn
   :fn predicate})

(defn enum [values-set]
  {:type :enum
   :values values-set})

(defn val [value]
  {:type :enum
   :values #{value}})

(defn and [& conditions]
  {:type :and
   :entries (mapv (clojure.core/fn [entry]
                    {:model entry})
                  conditions)})

(defn or [& conditions]
  {:type :or
   :entries (mapv (clojure.core/fn [entry]
                    {:model entry})
                  conditions)})

(defn set-of [elements-model]
  {:type :set
   :elements-model elements-model})

(defn map [& named-entries]
  {:type :map
   :entries (mapv (clojure.core/fn [[key model]]
                    {:key key
                     :model model})
                  (partition 2 named-entries))})

(defn map-of [keys-model values-model]
  {:type :map
   :keys {:model keys-model}
   :values {:model values-model}})

(defn sequence-of [elements-model]
  {:type :sequence
   :elements-model elements-model})

(defn list-of [elements-model]
  {:type :sequence
   :coll-type :list
   :elements-model elements-model})

(defn vector-of [elements-model]
  {:type :sequence
   :coll-type :vector
   :elements-model elements-model})

(defn tuple [& models]
  {:type :sequence
   :entries (mapv (clojure.core/fn [model]
                    {:model model})
                  models)})

(defn list [& models]
  {:type :sequence
   :coll-type :list
   :entries (mapv (clojure.core/fn [model]
                    {:model model})
                  models)})

(defn vector [& models]
  {:type :sequence
   :coll-type :vector
   :entries (mapv (clojure.core/fn [model]
                    {:model model})
                  models)})

(defn alt [& named-entries]
  {:type :alt
   :entries (mapv (clojure.core/fn [[key model]]
                    {:key key
                     :model model})
                  (partition 2 named-entries))})

(defn cat [& entries]
  {:type :cat
   :entries (mapv (clojure.core/fn [entry]
                    {:model entry})
                  entries)})

(defn repeat [min max elements-model]
  {:type :repeat
   :min min
   :max max
   :elements-model elements-model})

(defn ? [model]
  (repeat 0 1 model))

(defn * [model]
  (repeat 0 ##Inf model))

(defn + [model]
  (repeat 1 ##Inf model))

(defn let [bindings body]
  {:type :let
   :bindings bindings
   :body body})

(defn ref [key]
  {:type :ref
   :key key})

; Modifiers, can be used with the -> macro

;; For set-of and sequence-of
(defn with-count [collection-model count-model]
  (assoc collection-model :count-model count-model))

;; For cat and repeat
(defn in-vector [sequence-model]
  (assoc sequence-model :coll-type :vector))

;; For cat and repeat
(defn in-list [sequence-model]
  (assoc sequence-model :coll-type :list))

