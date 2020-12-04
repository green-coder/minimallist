(ns minimallist.helper
  (:refer-clojure :exclude [fn val and or set map sequence vector-of list vector cat repeat ? * + let ref])
  (:require [clojure.core :as clj]))

;;
;; Some helper functions to compose hash-map based models
;;


; Modifiers, can be used with the -> macro

(defn- -entry
  "Parses an entry from one of the following formats:
   - model
   - [key model]
   - [key options model]

   When specified, options is a hashmap."
  [entry]
  (clj/let [[key options model] (if (vector? entry)
                                  (case (count entry)
                                    2 [(first entry) nil (second entry)]
                                    3 entry
                                    (throw (ex-info "wrong entry format" entry)))
                                  [nil nil entry])]
    (cond-> options
            key (assoc :key key)
            model (assoc :model model))))


(defn with-count
  "Specifies a :count-model to a collection model with varying size,
   namely :set-of, :map-of and :sequence-of."
  [collection-model count-model]
  (assoc collection-model :count-model count-model))

(defn with-entries
  "Adds entries to a :map model."
  [map-model & entries]
  (assoc map-model
    :entries (into (:entries map-model [])
                   (clj/map -entry)
                   entries)))

(defn with-optional-entries
  "Adds optional entries to a :map model."
  [map-model & entries]
  (assoc map-model
    :entries (into (:entries map-model [])
                   (clj/map (clj/fn [entry]
                              (-> (-entry entry)
                                  (assoc :optional true))))
                   entries)))

(defn with-condition
  "Specifies a :condition-model to a model"
  [model condition-model]
  (assoc model :condition-model condition-model))

;; For any node, mostly for :fn
(defn with-test-check-gen
  "Specifies a test-check generator to a model."
  [model generator]
  (assoc model :test.check/generator generator))

(defn in-list
  "Specifies the :coll-type to be a list.
   To be used on sequence model nodes, namely :sequence-of, :sequence, :cat and :repeat."
  [sequence-model]
  (assoc sequence-model :coll-type :list))

(defn in-vector
  "Specifies the :coll-type to be a vector.
   To be used on sequence model nodes, namely :sequence-of, :sequence, :cat and :repeat."
  [sequence-model]
  (assoc sequence-model :coll-type :vector))

(defn in-string
  "Specifies the :coll-type to be a string.
   To be used on sequence model nodes, namely :sequence-of, :sequence, :cat and :repeat."
  [sequence-model]
  (assoc sequence-model :coll-type :string))

(defn not-inlined
  "Specify that this sequence model should not be inlined within its parent model.
   Useful on nodes :alt, :cat and :repeat."
  [sequence-model]
  (assoc sequence-model :inlined false))


;; The main functions

(defn fn
  "Model for values verifying custom predicates."
  [predicate]
  {:type :fn
   :fn predicate})

(defn val
  "Model for a fixed value."
  [value]
  {:type :enum
   :values #{value}})

(defn enum
  "Model of one value from a set."
  [values-set]
  {:type :enum
   :values values-set})

(defn and
  "Model for validating a value against a conjunction of models."
  [& conditions]
  {:type :and
   :entries (mapv -entry conditions)})

(defn or
  "Model for validating a value against a disjunction of models."
  [& conditions]
  {:type :or
   :entries (mapv -entry conditions)})

(defn set-of
  "Model of a set of values matching a specific model."
  [elements-model]
  {:type :set-of
   :elements-model elements-model})

(defn map-of
  "Model of a hashmap made of entries (2-vector) of a specific model."
  [entry-model]
  {:type :map-of
   :entry-model entry-model})

(defn sequence-of
  "Model of a sequence of values, all matching a specific model."
  [elements-model]
  {:type :sequence-of
   :elements-model elements-model})

(defn list-of
  "Same as sequence-of, but inside a list."
  [elements-model]
  (-> (sequence-of elements-model) in-list))

(defn vector-of
  "Same as sequence-of, but inside a vector."
  [elements-model]
  (-> (sequence-of elements-model) in-vector))

(defn string-of
  "Same as sequence-of, but inside a string."
  [elements-model]
  (-> (sequence-of elements-model) in-string))

(defn map
  "Model of a hashmap with specified models for each of its entries."
  ([]
   {:type :map})
  ([& entries]
   (apply with-entries (map) entries)))

(defn tuple
  "A sequence of values, each matching their specific model."
  [& entries]
  {:type :sequence
   :entries (mapv -entry entries)})

(defn list
  "Same as tuple, but inside a list."
  [& entries]
  (-> (apply tuple entries) in-list))

(defn vector
  "Same as tuple, but inside a vector."
  [& entries]
  (-> (apply tuple entries) in-vector))

(defn string-tuple
  "Same as tuple, but inside a string."
  [& entries]
  (-> (apply tuple entries) in-string))

(defn alt
  "Model of a choice (alternative) between different possible entries."
  [& entries]
  {:type :alt
   :entries (mapv -entry entries)})

(defn cat
  "Sequence model of a concatenation of models.
   The sequence models in the entries are inlined by default."
  [& entries]
  {:type :cat
   :entries (mapv -entry entries)})

(defn repeat
  "Sequence model of a repetition of a model.
   If elements-model is a sequence model, it is inlined by default."
  [min max elements-model]
  {:type :repeat
   :min min
   :max max
   :elements-model elements-model})

(defn ?
  "Sequence model of a model being either absent or present."
  [model]
  (repeat 0 1 model))

(defn *
  "Sequence model of a model being either absent or repeated an
   arbitrary number of times."
  [model]
  (repeat 0 ##Inf model))

(defn +
  "Sequence model of a model being repeated an
   arbitrary number of times."
  [model]
  (repeat 1 ##Inf model))

(defn let
  "Model with local model definitions."
  [bindings body]
  {:type :let
   :bindings (apply hash-map bindings)
   :body body})

(defn ref
  "A reference to a model locally defined."
  [key]
  {:type :ref
   :key key})

(defn char-cat
  "A cat sequence of chars, built from a string which contains them."
  [s]
  (apply cat (clj/map val s)))

(defn char-set
  "A set of chars, built from a string which contains them."
  [s]
  (enum (clj/set s)))
