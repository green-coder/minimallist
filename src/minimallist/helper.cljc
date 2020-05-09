(ns minimallist.helper
  (:refer-clojure :exclude [fn val or and map sequence vector-of list vector cat repeat ? * + let ref])
  (:require [clojure.core :as cl]))

;;
;; Some helper functions to compose hash-map based models
;;


; Modifiers, can be used with the -> macro

(defn- elm-options-elm [args]
  (if (= (count args) 2)
    [(first args) nil (second args)]
    args))

;; For set-of and sequence-of
(defn with-count [collection-model count-model]
  (assoc collection-model :count-model count-model))

;; For map
(defn with-entries [map-model entries]
   (assoc map-model
     :entries (mapv (comp (cl/fn [[key options model]]
                            (assoc options
                              :key key
                              :model model))
                          elm-options-elm)
                    entries)))

;; For map
(defn with-optional-entries [map-model entries]
   (assoc map-model
     :entries (mapv (comp (cl/fn [[key options model]]
                            (assoc options
                              :optional true
                              :key key
                              :model model))
                          elm-options-elm)
                    entries)))

;; For map
(defn with-keys [map-model keys-model]
  (assoc map-model :keys {:model keys-model}))

;; For map
(defn with-values [map-model values-model]
  (assoc map-model :values {:model values-model}))

;; For any structural node
(defn with-condition [collection-model condition-model]
  (assoc collection-model :condition-model condition-model))

;; For cat and repeat
(defn in-vector [sequence-model]
  (assoc sequence-model :coll-type :vector))

;; For cat and repeat
(defn in-list [sequence-model]
  (assoc sequence-model :coll-type :list))

;; For alt, cat and repeat
(defn not-inlined [sequence-model]
  (assoc sequence-model :inlined false))


;; the main functions

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
   :entries (mapv (cl/fn [entry]
                    {:model entry})
                  conditions)})

(defn or [& conditions]
  {:type :or
   :entries (mapv (cl/fn [entry]
                    {:model entry})
                  conditions)})

(defn set-of [elements-model]
  {:type :set
   :elements-model elements-model})

(defn map
  ([]
   {:type :map})
  ([& entries]
   (-> (map) (with-entries entries))))

(defn map-of [keys-model values-model]
  (-> (map) (with-keys keys-model) (with-values values-model)))

(defn sequence []
  {:type :sequence})

(defn sequence-of [elements-model]
  {:type :sequence
   :elements-model elements-model})

(defn list-of [elements-model]
  (-> (sequence-of elements-model) (in-list)))

(defn vector-of [elements-model]
  (-> (sequence-of elements-model) (in-vector)))

(defn tuple [& models]
  {:type :sequence
   :entries (mapv (cl/fn [model]
                    {:model model})
                  models)})

(defn list [& models]
  (-> (apply tuple models) (in-list)))

(defn vector [& models]
  (-> (apply tuple models) (in-vector)))

(defn alt [& named-entries]
  {:type :alt
   :entries (mapv (cl/fn [[key model]]
                    {:key key
                     :model model})
                  (partition 2 named-entries))})

(defn cat [& entries]
  {:type :cat
   :entries (mapv (cl/fn [entry]
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
