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


(declare describe)

;; This may be removed once we have the zipper visitor working.
(defn- seq-description-without-impl-details [seq-description]
  ; Gets rid of the implementation-induced wrapper node around the description
  (if-let [description (:description seq-description)]
    description
    (dissoc seq-description :length :rest-seq)))

(defn- sequence-descriptions
  "Returns a sequence of possible descriptions from the seq-data matching a model."
  [context model seq-data]
  (if (and (#{:alt :cat :repeat :let :ref} (:type model))
           (:inlined model true))
    (case (:type model)
      :alt (mapcat (fn [entry]
                     (map (fn [seq-description]
                            {:key (:key entry)
                             :length (:length seq-description)
                             :rest-seq (:rest-seq seq-description)
                             :entry (seq-description-without-impl-details seq-description)})
                          (sequence-descriptions context (:model entry) seq-data)))
                   (:entries model))
      :cat (reduce (fn [seq-descriptions entry]
                     (mapcat (fn [acc]
                               (map (fn [seq-description]
                                      {:length (+ (:length acc) (:length seq-description))
                                       :rest-seq (:rest-seq seq-description)
                                       :entries (conj (:entries acc) (seq-description-without-impl-details seq-description))})
                                    (sequence-descriptions context (:model entry) (:rest-seq acc))))
                             seq-descriptions))
                   [{:length 0
                     :rest-seq seq-data
                     :entries []}]
                   (:entries model))
      :repeat (->> (iterate (fn [seq-descriptions]
                              (mapcat (fn [acc]
                                        (map (fn [seq-description]
                                               {:length (+ (:length acc) (:length seq-description))
                                                :rest-seq (:rest-seq seq-description)
                                                :entries (conj (:entries acc) (seq-description-without-impl-details seq-description))})
                                             (sequence-descriptions context (:elements-model model) (:rest-seq acc))))
                                      seq-descriptions))
                            [{:length 0
                              :rest-seq seq-data
                              :entries []}])
                   (take-while seq)
                   (take (inc (:max model))) ; inc because it includes the "match zero times"
                   (drop (:min model))
                   (reverse) ; longest repetitions first
                   (apply concat))
      :let (sequence-descriptions (merge context (:bindings model)) (:body model) seq-data)
      :ref (sequence-descriptions context (get context (:key model)) seq-data))
    (if seq-data
      (let [description (describe context (dissoc model :inlined) (first seq-data))]
        (if (:valid? description)
          [{:length 1
            :rest-seq (next seq-data)
            :description description}]
          []))
      [])))

;;---

;; API

(defn valid?
  "Return true if the data matches the model, false otherwise."
  ([model data]
   (valid? {} model data))
  ([context model data]
   (boolean (-valid? context model data))))

(defn explain
  "Returns a structure describing what parts of the data are not matching the model."
  [model data])

(defn describe
  "Returns a descriptions of the data's structure using a hierarchy of hash-maps."
  ([model data]
   (describe {} model data))
  ([context model data]
   (-> (case (:type model)
         :fn {:valid? ((:fn model) data)}
         :enum {:valid? (contains? (:values model) data)}
         :and {:valid? (every? (fn [entry]
                                 (:valid? (describe context (:model entry) data)))
                               (:entries model))}
         :or {:valid? (some (fn [entry]
                              (:valid? (describe context (:model entry) data)))
                            (:entries model))}
         :set (if (set? data)
                (let [entries (when (contains? model :elements-model)
                                (into #{}
                                      (map (partial describe context (:elements-model model)))
                                      data))]
                  (cond-> {:valid? (and (implies (contains? model :elements-model)
                                                 (every? :valid? entries))
                                        (implies (contains? model :count-model)
                                                 (:valid? (describe context (:count-model model) (count data))))
                                        (implies (contains? model :condition-model)
                                                 (:valid? (describe context (:condition-model model) data))))}
                          (contains? model :elements-model) (assoc :entries entries))))
         :map (if (map? data)
                (let [entries (into {}
                                    (keep (fn [entry]
                                            (if (contains? data (:key entry))
                                              [(:key entry) (describe context (:model entry) (get data (:key entry)))]
                                              (when-not (:optional entry)
                                                [(:key entry) {:missing? true}]))))
                                    (:entries model))]
                  (cond-> {:valid? (and (implies (contains? model :entries)
                                                 (every? :valid? (vals entries)))
                                        (implies (contains? model :keys)
                                                 (every? (fn [key]
                                                           (:valid? (describe context (-> model :keys :model) key)))
                                                         (keys data)))
                                        (implies (contains? model :values)
                                                 (every? (fn [key]
                                                           (:valid? (if (contains? entries key)
                                                                      (get entries key)
                                                                      (describe context (-> model :values :model) (get data key)))))
                                                         (keys data)))
                                        (implies (contains? model :condition-model)
                                                 (:valid? (describe context (:condition-model model) data))))}
                          (contains? model :entries) (assoc :entries entries)))
                {:valid? false})
         :sequence (if (sequential? data)
                     (let [entries (into [] (cond
                                              (contains? model :elements-model) (map (fn [data-element]
                                                                                       (describe context (:elements-model model) data-element))
                                                                                     data)
                                              (contains? model :entries) (map (fn [entry data-element]
                                                                                (describe context (:model entry) data-element))
                                                                              (:entries model)
                                                                              data)
                                              :else nil))]
                       (cond-> {:valid? (and (({:any any?
                                                :list list?
                                                :vector vector?} (:coll-type model :any)) data)
                                             (implies (contains? model :entries)
                                                      (and (= (count (:entries model)) (count data))
                                                           (every? :valid? entries)))
                                             (implies (contains? model :count-model)
                                                      (:valid? (describe context (:count-model model) (count data))))
                                             (implies (contains? model :elements-model)
                                                      (every? :valid? entries))
                                             (implies (contains? model :condition-model)
                                                      (:valid? (describe context (:condition-model model) data))))}
                               (or (contains? model :elements-model)
                                   (contains? model :entries)) (assoc :entries entries)))
                     {:valid? false})
         :alt (let [[key entry] (first (into []
                                             (comp (map (fn [entry]
                                                          [(:key entry)
                                                           (describe context (:model entry) data)]))
                                                   (filter (comp :valid? second))
                                                   (take 1))
                                             (:entries model)))]
                (if (nil? entry)
                  {:valid? false}
                  {:key key
                   :entry entry
                   :valid? true}))
         (:cat :repeat) (if (and (sequential? data)
                                 ((-> (:coll-type model :any) {:any any?
                                                               :list list?
                                                               :vector vector?}) data)
                                 (implies (contains? model :count-model)
                                          (:valid? (describe context (:count-model model) (count data)))))
                          (let [seq-descriptions (filter (comp nil? :rest-seq)
                                                         (sequence-descriptions context model (seq data)))]
                            (if (seq seq-descriptions)
                              {:entries (:entries (first seq-descriptions))
                               :valid? (implies (contains? model :condition-model)
                                                (:valid? (describe context (:condition-model model) data)))}
                              {:valid? false}))
                          {:valid? false})
         :let (describe (merge context (:bindings model)) (:body model) data)
         :ref (describe context (get context (:key model)) data))
       (assoc :context context ;; maybe: (post-fn description context model data)
              :model model
              :data data))))

;; TODO: Treat the attributes independently of the type of the node in which they appear.
;;       That's a kind of composition pattern a-la-unity.

(defn undescribe
  "Returns a data which matches a description."
  [model description])

;;---

(comment
  ;; Not in the core, not urgent.
  (defn visit [model travel-plan data])

  ;; Not in the core, not urgent, maybe not needed.
  (defn transform [model transformer data]))
