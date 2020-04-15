(ns minimallist.helper
  (:refer-clojure :exclude [fn val and or set map sequence vector-of list vector cat repeat ? * + let ref])
  (:require [clojure.core :as cl]))

;;
;; Some helper functions to compose hash-map based models
;;


; Modifiers, can be used with the -> macro

(defn- -entry [entry]
  (cl/let [[key options model] (if (vector? entry)
                                 (case (count entry)
                                   2 [(first entry) nil (second entry)]
                                   3 entry
                                   (throw (ex-info "wrong entry format" entry)))
                                 [nil nil entry])]
    (cond-> options
            key (assoc :key key)
            model (assoc :model model))))


;; For :set and :sequence
(defn with-count [collection-model count-model]
  (assoc collection-model :count-model count-model))

;; For :map
(defn with-entries [map-model entries]
   (assoc map-model
     :entries (into (:entries map-model [])
                    (cl/map -entry)
                    entries)))

;; For :map
(defn with-optional-entries [map-model entries]
   (assoc map-model
     :entries (into (:entries map-model [])
                    (cl/map (cl/fn [entry]
                              (-> (-entry entry)
                                  (assoc :optional true))))
                    entries)))

;; For :map
(defn with-keys [map-model keys-model]
  (assoc map-model :keys {:model keys-model}))

;; For :map
(defn with-values [map-model values-model]
  (assoc map-model :values {:model values-model}))

;; For any structural node
(defn with-condition [collection-model condition-model]
  (assoc collection-model :condition-model condition-model))

;; For :sequence, :cat and :repeat
(defn in-vector [sequence-model]
  (assoc sequence-model :coll-type :vector))

;; For :sequence, :cat and :repeat
(defn in-list [sequence-model]
  (assoc sequence-model :coll-type :list))

;; For :alt, :cat and :repeat
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
   :entries (mapv -entry conditions)})

(defn or [& conditions]
  {:type :or
   :entries (mapv -entry conditions)})

(defn set []
  {:type :set})

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

(defn tuple [& entries]
  {:type    :sequence
   :entries (mapv -entry entries)})

(defn list [& entries]
  (-> (apply tuple entries) (in-list)))

(defn vector [& entries]
  (-> (apply tuple entries) (in-vector)))

(defn alt [& entries]
  {:type :alt
   :entries (mapv -entry entries)})

(defn cat [& entries]
  {:type :cat
   :entries (mapv -entry entries)})

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
