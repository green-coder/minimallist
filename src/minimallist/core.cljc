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
   :let :ref
   :cat :alt :repeat])

;;---

(defmacro implies [condition & expressions]
  `(if ~condition
     (do ~@expressions)
     true))

(declare valid?)
(declare left-overs)

(defn- element-left-overs [context model seq-data]
  (if (and (#{:cat :alt :repeat} (:type model))
           (:inlined model true))
    (left-overs context model seq-data)
    (if (and seq-data
             (valid? context model (first seq-data)))
      (list (next seq-data))
      '())))

(defn- cat-left-overs [context model seq-data]
  (let [f (fn -left-overs [seq-entries seq-data]
            (if seq-entries
              (let [[{:keys [model]} & next-entries] seq-entries
                    left-overs-coll (element-left-overs context model seq-data)]
                (mapcat (partial -left-overs next-entries) left-overs-coll))
              (list seq-data)))]
    (f (seq (:entries model)) seq-data)))

(defn- alt-left-overs [context model seq-data]
  (mapcat (fn [entry]
            (element-left-overs context (:model entry) seq-data))
          (:entries model)))

(defn- repeat-left-overs [context model seq-data]
  (let [{:keys [min max model]} model
        f (fn -left-overs [nb-matched seq-data]
            (if (< nb-matched max)
              (let [left-overs-coll (element-left-overs context model seq-data)
                    rest (mapcat (partial -left-overs (inc nb-matched)) left-overs-coll)]
                (if (<= min nb-matched)
                  (cons seq-data rest)
                  rest))
              (list seq-data)))]
    (f 0 seq-data)))

(defn left-overs
  "Returns a sequence of possible left-overs from the seq-data after matching the model with it."
  [context model seq-data]
  (case (:type model)
    :cat (cat-left-overs context model seq-data)
    :alt (alt-left-overs context model seq-data)
    :repeat (repeat-left-overs context model seq-data)))

;;---

;; API
(defn valid?
  "Return true if the data matches the model, false otherwise."
  ([model data]
   (valid? {} model data))
  ([context model data]
   (case (:type model)
     :fn (boolean ((:fn model) data))
     :enum (contains? (:values model) data)
     :and (every? (fn [entry]
                    (valid? context (:model entry) data))
                  (:entries model))
     :or (boolean (some (fn [entry]
                          (valid? context (:model entry) data))
                        (:entries model)))
     :set (and (set? data)
               (every? (partial valid? context (:model model)) data))
     :map (and (map? data)
               (implies (contains? model :entries)
                        (every? (fn [entry]
                                  (and (contains? data (:key entry))
                                       (valid? context (:model entry) (get data (:key entry)))))
                                (:entries model)))
               (implies (contains? model :keys)
                        (every? (partial valid? context (-> model :keys :model)) (keys data)))
               (implies (contains? model :values)
                        (every? (partial valid? context (-> model :values :model)) (vals data))))
     :sequence (and (sequential? data)
                    (({:any any?
                       :list list?
                       :vector vector?} (:coll-type model :any)) data)
                    (implies (contains? model :model)
                             (every? (partial valid? context (:model model)) data))
                    (implies (contains? model :count)
                             (= (:count model) (count data)))
                    (implies (contains? model :entries)
                             (and (= (count (:entries model)) (count data))
                                  (every? true? (map (fn [entry data-element]
                                                       (valid? context (:model entry) data-element))
                                                     (:entries model)
                                                     data)))))
     :let (valid? (merge context (:bindings model)) (:body model) data)
     :ref (valid? context (get context (:ref model)) data)
     (:cat :alt :repeat) (and (sequential? data)
                              (boolean (some nil? (left-overs context model (seq data))))))))

(defn explain
  "Returns a structure describing what parts of the data are not matching the model."
  [model data])

(defn- valid-description? [description]
  (contains? description :data))

;; My hardship comes from the implementation of :and and :or,
;; the rest is a piece of cake.

;; There are 2 kinds of predicates:
;; - structural (they test the existence of a structure),
;; - non-structural (they test properties on structureless values).

;; :set can have the attribute:
;; - :count with a number value

;; [:map :map-of] can all be defined using :map, with the optional attributes:
;; - :key {:model ...}
;; - :value {:model ...}

;; [:sequence :list :vector :tuple] can all be defined
;; using :sequence, with the attributes:
;; - :coll-type with values in #{:list :vector}
;; - :count with a number value

;; Structural predicates can be predefined extensively:
;; [:set :map :sequence]

;; To be considered:
;; - using :alt instead of :or when testing on different kind of structures,
;;   in other word:
;;   - :or is only for non-structural models, (e.g. [:or odd? prime?])
;;   - :alt only for structural models (e.g. [:alt [:my-set set?] [my-vec vector?]])

(defn describe
  "Returns a descriptions of the data's structure using a hierarchy of hash-maps.
   In places where the data does not match the model, the hash-map won't have a :data entry."
  ([model data]
   (describe {} model data))
  ([context model data]
   (-> (case (:type model)
         :fn (if ((:fn model) data)
               {:data data}
               {})
         :enum (if (contains? (:values model) data)
                 {:data data}
                 {})
         :and (reduce (fn [description entry]
                        (let [description (describe context (:model entry) (:data description))]
                          (if (valid-description? description)
                            description
                            (reduced {}))))
                      {:data data}
                      (:entries model))
         :or {:data (into {}
                          (comp (map (fn [entry]
                                       [(:key entry) (describe context (:model entry) data)]))
                                (filter (comp valid-description? second)))
                          (:entries model))}
         :map (if (and (map? data)
                       (every? (fn [entry]
                                 (contains? data (:key entry)))
                               (:entries model)))
                {:data (into {}
                             (map (fn [entry]
                                    [(:key entry) (describe context (:model entry) data)]))
                             (:entries model))}
                {})
         :map-of (if (map? data)
                   {:data (into {}
                                (map (fn [[k v]]
                                       [(describe context (-> model :key :model) k)
                                        (describe context (-> model :value :model) v)]))
                                data)}
                   {})
         :coll-of (if (coll? data)
                    {:data (into (empty data)
                                 (map (partial describe context (:model model)))
                                 data)}
                    {})
         :sequence (if (sequential? data)
                     {:data (into []
                                  (map (partial describe context (:model model)))
                                  data)}
                     {})
         :list (if (list? data)
                 {:data (into []
                              (map (partial describe context (:model model)))
                              data)}
                 {})
         :vector (if (vector? data)
                   {:data (into []
                                (map (partial describe context (:model model)))
                                data)}
                   {})
         :set (if (set? data)
                {:data (into #{}
                             (map (partial describe context (:model model)))
                             data)}
                {})
         :tuple (if (and (sequential? data)
                         (= (count (:entries model)) (count data)))
                  {:data (into []
                               (map (partial describe context (:model model)))
                               data)}
                  {})
         :let (describe (merge context (:bindings model)) (:body model) data)
         :ref (describe context (get context (:ref model)) data)
         (:cat :alt :repeat) nil)
       (assoc :context context
              :model model))))

(defn undescribe
  "Returns a data which matches a description."
  [model description])

;;---

(comment
  ;; Not in the core, not urgent.
  (defn generate
    "Returns a data generated by a generator, in the shape represented by the model."
    [model generator])

  ;; Not in the core, not urgent.
  (defn visit [model travel-plan data])

  ;; Not in the core, not urgent, maybe not needed.
  (defn transform [model transformer data])


  (for [a [:a :b :c]
        b [:x :y :z]]
    [a b])

  (mapcat (fn [a]
            (map (fn [b] [a b]) [:x :y :z]))
          [:a :b :c])

  (mapcat (fn [a]
            (map (fn [b] [a b]) b-coll))
          a-coll)

  (mapcat (fn [a]
            (map (fn [ac] [a ac]) acc))
          a-coll))
