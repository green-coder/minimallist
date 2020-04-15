(ns minimallist.core
  #?(:cljs [:require-macros [minimallist.core :refer [implies]]]))

(comment
  ;; Format which *may* be used by the end user later
  [:map [:x int?]]

  ;; Format used in the library's code
  {:type :map
   :entries [{:key :x
              :model {:type :fn
                      :name int?}}]}

  ;; Supported node types
  [:fn :enum
   :and :or
   :set :map :sequence
   :alt :cat :repeat
   :let :ref])

;; There are 2 kinds of predicates:
;; - structural (they test the existence of a structure),
;; - logical (they test properties on values and lead to booleans, they are not used to describe their structure).

;; Structural predicates can be predefined extensively:
;; [:set :map :sequence :alt :cat :repeat]

;; Logical predicates are everything else, for instance:
;; [:fn :enum :and :or]

;; Re-design of the :or and :alt :
;;   - :or is only for non-structural tests, (e.g. [:or even? prime?])
;;   - :alt is used for any kind of branching, structural or logical.

;;---

(defmacro implies [condition & expressions]
  `(if ~condition
     (do ~@expressions)
     true))

(declare -valid?)

(defn- left-overs
  "Returns a sequence of possible left-overs from the seq-data after matching the model with it."
  [context model seq-data]
  (if (and (#{:alt :cat :repeat :let :ref} (:type model))
           (:inlined model true))
    (case (:type model)
      :alt (mapcat (fn [entry]
                     (left-overs context (:model entry) seq-data))
                   (:entries model))
      :cat (reduce (fn [seqs-data entry]
                     (mapcat (fn [seq-data]
                               (left-overs context (:model entry) seq-data))
                             seqs-data))
                   [seq-data]
                   (:entries model))
      :repeat (->> (iterate (fn [seqs-data]
                              (mapcat (fn [seq-data]
                                        (left-overs context (:elements-model model) seq-data))
                                      seqs-data))
                            [seq-data])
                   (take-while seq)
                   (take (inc (:max model))) ; inc because it includes the "match zero times"
                   (drop (:min model))
                   (apply concat))
      :let (left-overs (merge context (:bindings model)) (:body model) seq-data)
      :ref (left-overs context (get context (:key model)) seq-data))
    (if (and seq-data
             (-valid? context (dissoc model :inlined) (first seq-data)))
      [(next seq-data)]
      [])))

(defn- -valid? [context model data]
  (case (:type model)
    :fn ((:fn model) data)
    :enum (contains? (:values model) data)
    :and (every? (fn [entry]
                   (-valid? context (:model entry) data))
                 (:entries model))
    (:or :alt) (some (fn [entry]
                       (-valid? context (:model entry) data))
                     (:entries model))
    :set (and (set? data)
              (implies (contains? model :count-model)
                       (-valid? context (:count-model model) (count data)))
              (implies (contains? model :elements-model)
                       (every? (partial -valid? context (:elements-model model)) data))
              (implies (contains? model :condition-model)
                       (-valid? context (:condition-model model) data)))
    :map (and (map? data)
              (implies (contains? model :entries)
                       (every? (fn [entry]
                                 (if (contains? data (:key entry))
                                   (-valid? context (:model entry) (get data (:key entry)))
                                   (:optional entry)))
                               (:entries model)))
              (implies (contains? model :keys)
                       (every? (partial -valid? context (-> model :keys :model)) (keys data)))
              (implies (contains? model :values)
                       (every? (partial -valid? context (-> model :values :model)) (vals data)))
              (implies (contains? model :condition-model)
                       (-valid? context (:condition-model model) data)))
    :sequence (and (sequential? data)
                   ((-> (:coll-type model :any) {:any any?
                                                 :list list?
                                                 :vector vector?}) data)
                   (implies (contains? model :entries)
                            (and (= (count (:entries model)) (count data))
                                 (every? true? (map (fn [entry data-element]
                                                      (-valid? context (:model entry) data-element))
                                                    (:entries model)
                                                    data))))
                   (implies (contains? model :count-model)
                            (-valid? context (:count-model model) (count data)))
                   (implies (contains? model :elements-model)
                            (every? (partial -valid? context (:elements-model model)) data))
                   (implies (contains? model :condition-model)
                            (-valid? context (:condition-model model) data)))
    (:cat :repeat) (and (sequential? data)
                        ((-> (:coll-type model :any) {:any any?
                                                      :list list?
                                                      :vector vector?}) data)
                        (some nil? (left-overs context model (seq data)))
                        (implies (contains? model :count-model)
                                 (-valid? context (:count-model model) (count data)))
                        (implies (contains? model :condition-model)
                                 (-valid? context (:condition-model model) data)))
    :let (-valid? (merge context (:bindings model)) (:body model) data)
    :ref (-valid? context (get context (:key model)) data)))

;;---

;; API

(defn valid?
  "Return true if the data matches the model, false otherwise."
  ([model data]
   (valid? {} model data))
  ([context model data]
   (boolean (-valid? context model data))))
