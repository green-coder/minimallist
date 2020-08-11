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
   :set-of :map-of :map :sequence-of :sequence
   :alt :cat :repeat
   :transform
   :let :ref])

;; There are 2 kinds of predicates:
;; - structural (they test the existence of a structure),
;; - logical (they test properties on values and lead to booleans, they are not used to describe their structure).

;; Structural predicates can be predefined extensively:
;; [:set-of :map-of :map :sequence-of :sequence :alt :cat :repeat]

;; Logical predicates are everything else, for instance:
;; [:fn :enum :and :or]

;; Re-design of the :or and :alt :
;;   - :or is only for non-structural tests, (e.g. [:or even? prime?])
;;   - :alt is used for any kind of branching, structural or logical.

;;---

(defmacro ^:no-doc implies
  "Logically equivalent to `(or (not condition) expression)`"
  [cause consequence]
  `(if ~cause
     ~consequence
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
      :let (left-overs (into context (:bindings model)) (:body model) seq-data)
      :ref (left-overs context (get context (:key model)) seq-data))
    (if (and seq-data
             (-valid? context (dissoc model :inlined) (first seq-data)))
      [(next seq-data)]
      [])))

(defn- -valid? [context model data]
  (case (:type model)
    :fn (and ((:fn model) data)
             (implies (contains? model :condition-model)
                      (-valid? context (:condition-model model) data)))
    :enum (contains? (:values model) data)
    :and (every? (fn [entry]
                   (-valid? context (:model entry) data))
                 (:entries model))
    (:or :alt) (some (fn [entry]
                       (-valid? context (:model entry) data))
                     (:entries model))
    :set-of (and (set? data)
                 (implies (contains? model :count-model)
                          (-valid? context (:count-model model) (count data)))
                 (implies (contains? model :elements-model)
                          (every? (partial -valid? context (:elements-model model)) data))
                 (implies (contains? model :condition-model)
                          (-valid? context (:condition-model model) data)))
    (:map-of :map) (and (map? data)
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
    (:sequence-of :sequence) (and (sequential? data)
                                  ((-> (:coll-type model :any) {:any any?
                                                                :list list?
                                                                :vector vector?}) data)
                                  (implies (contains? model :entries)
                                           (and (= (count (:entries model)) (count data))
                                                (every? identity (map (fn [entry data-element]
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
                        (some nil? (left-overs context (dissoc model :inlined) (seq data)))
                        (implies (contains? model :count-model)
                                 (-valid? context (:count-model model) (count data)))
                        (implies (contains? model :condition-model)
                                 (-valid? context (:condition-model model) data)))
    :transform (and (-valid? context (:outer-model model) data)
                    (-valid? context (:inner-model model) ((:outer->inner model identity) data)))
    :let (-valid? (into context (:bindings model)) (:body model) data)
    :ref (-valid? context (get context (:key model)) data)))

(declare -describe)

(defn- sequence-descriptions
  "Returns a sequence of possible descriptions from the seq-data matching a model."
  [context model seq-data]
  (if (and (#{:alt :cat :repeat :let :ref} (:type model))
           (:inlined model true))
    (case (:type model)
      :alt (mapcat (fn [index entry]
                     (->> (sequence-descriptions context (:model entry) seq-data)
                          (map (fn [seq-description]
                                 {:rest-seq (:rest-seq seq-description)
                                  :desc [(:key entry index) (:desc seq-description)]}))))
                   (range)
                   (:entries model))
      :cat (if (every? #(not (contains? % :key)) (:entries model))
             (reduce (fn [seq-descriptions entry]
                       (mapcat (fn [acc]
                                 (->> (sequence-descriptions context (:model entry) (:rest-seq acc))
                                      (map (fn [seq-description]
                                             {:rest-seq (:rest-seq seq-description)
                                              :desc (conj (:desc acc) (:desc seq-description))}))))
                               seq-descriptions))
                     [{:rest-seq seq-data
                       :desc []}]
                     (:entries model))
             (reduce-kv (fn [seq-descriptions index entry]
                          (mapcat (fn [acc]
                                    (->> (sequence-descriptions context (:model entry) (:rest-seq acc))
                                         (map (fn [seq-description]
                                                {:rest-seq (:rest-seq seq-description)
                                                 :desc (assoc (:desc acc) (:key entry index) (:desc seq-description))}))))
                                  seq-descriptions))
                     [{:rest-seq seq-data
                       :desc {}}]
                     (:entries model)))
      :repeat (->> (iterate (fn [seq-descriptions]
                              (mapcat (fn [acc]
                                        (->> (sequence-descriptions context (:elements-model model) (:rest-seq acc))
                                             (map (fn [seq-description]
                                                    {:rest-seq (:rest-seq seq-description)
                                                     :desc (conj (:desc acc) (:desc seq-description))}))))
                                      seq-descriptions))
                            [{:rest-seq seq-data
                              :desc []}])
                   (take-while seq)
                   (take (inc (:max model))) ; inc because it includes the "match zero times"
                   (drop (:min model))
                   (reverse) ; longest repetitions first
                   (apply concat))
      :let (sequence-descriptions (into context (:bindings model)) (:body model) seq-data)
      :ref (sequence-descriptions context (get context (:key model)) seq-data))
    (if seq-data
      (let [description (-describe context (dissoc model :inlined) (first seq-data))]
        (if (:valid? description)
          [{:rest-seq (next seq-data)
            :desc (:desc description)}]
          []))
      [])))

(defn- -describe
  [context model data]
  (case (:type model)
    :fn {:valid? ((:fn model) data)
         :desc data}
    :enum {:valid? (contains? (:values model) data)
           :desc data}
    :and {:valid? (every? (fn [entry]
                            (:valid? (-describe context (:model entry) data)))
                          (:entries model))
          :desc data}
    :or {:valid? (some (fn [entry]
                         (:valid? (-describe context (:model entry) data)))
                       (:entries model))
         :desc data}
    :set-of (if (set? data)
              (let [entries (when (contains? model :elements-model)
                              (into #{}
                                    (map (partial -describe context (:elements-model model)))
                                    data))
                    valid? (and (implies (contains? model :elements-model)
                                         (every? :valid? entries))
                                (implies (contains? model :count-model)
                                         (:valid? (-describe context (:count-model model) (count data))))
                                (implies (contains? model :condition-model)
                                         (-valid? context (:condition-model model) data)))]
                {:valid? valid?
                 :desc (into #{} (map :desc) entries)}))
    :map-of (if (map? data)
              (let [key-model (-> model :keys :model)
                    values-model (-> model :values :model)
                    entries (into {}
                                  (map (fn [[key value]]
                                         [(if key-model
                                            (-describe context key-model key)
                                            {:valid? true, :desc key})
                                          (if values-model
                                            (-describe context values-model value)
                                            {:valid? true, :desc key})]))
                                  data)
                    valid? (and (implies (contains? model :keys)
                                         (every? :valid? (keys entries)))
                                (implies (contains? model :values)
                                         (every? :valid? (vals entries)))
                                (implies (contains? model :condition-model)
                                         (-valid? context (:condition-model model) data)))]
                {:valid? valid?
                 :desc (into {} (map (fn [[k v]] [(:desc k) (:desc v)])) entries)})
              {:valid? false})
    :map (if (map? data)
           (let [entries (into {}
                               (keep (fn [entry]
                                       (if (contains? data (:key entry))
                                         [(:key entry) (-describe context (:model entry) (get data (:key entry)))]
                                         (when-not (:optional entry)
                                           [(:key entry) {:missing? true}]))))
                               (:entries model))
                 valid? (and (implies (contains? model :entries)
                                      (every? :valid? (vals entries)))
                             (implies (contains? model :condition-model)
                                      (-valid? context (:condition-model model) data)))]
             {:valid? valid?
              :desc (into {} (map (fn [[k v]] [k (:desc v)])) entries)})
           {:valid? false})
    (:sequence-of :sequence) (if (sequential? data)
                               (let [entries (into [] (cond
                                                        (contains? model :elements-model) (map (fn [data-element]
                                                                                                 (-describe context (:elements-model model) data-element))
                                                                                               data)
                                                        (contains? model :entries) (map (fn [entry data-element]
                                                                                          (-describe context (:model entry) data-element))
                                                                                        (:entries model)
                                                                                        data)
                                                        :else (map (fn [x] {:desc x}) data)))
                                     valid? (and (({:any any?
                                                    :list list?
                                                    :vector vector?} (:coll-type model :any)) data)
                                                 (implies (contains? model :entries)
                                                          (and (= (count (:entries model)) (count data))
                                                               (every? :valid? entries)))
                                                 (implies (contains? model :count-model)
                                                          (:valid? (-describe context (:count-model model) (count data))))
                                                 (implies (contains? model :elements-model)
                                                          (every? :valid? entries))
                                                 (implies (contains? model :condition-model)
                                                          (-valid? context (:condition-model model) data)))]
                                 {:valid? valid?
                                  :desc (mapv :desc entries)})
                               {:valid? false})
    :alt (let [[key entry] (first (into []
                                        (comp (map-indexed (fn [index entry]
                                                             [(:key entry index)
                                                              (-describe context (:model entry) data)]))
                                              (filter (comp :valid? second))
                                              (take 1))
                                        (:entries model)))]
           (if (nil? entry)
             {:valid? false}
             {:valid? true
              :desc [key (:desc entry)]}))
    (:cat :repeat) (if (and (sequential? data)
                            ((-> (:coll-type model :any) {:any any?
                                                          :list list?
                                                          :vector vector?}) data)
                            (implies (contains? model :count-model)
                                     (:valid? (-describe context (:count-model model) (count data)))))
                     (let [seq-descriptions (filter (comp nil? :rest-seq)
                                                    (sequence-descriptions context model (seq data)))]
                       (if (seq seq-descriptions)
                         {:desc (:desc (first seq-descriptions))
                          :valid? (implies (contains? model :condition-model)
                                           (-valid? context (:condition-model model) data))}
                         {:valid? false}))
                     {:valid? false})
    :transform (if (-valid? context (:outer-model model) data)
                 (let [description (-describe context (:inner-model model) ((:outer->inner model identity) data))]
                   (if (:valid? description)
                     {:valid? true
                      :desc ((:outer<-inner model identity) (:desc description))}
                     {:valid? false}))
                 {:valid? false})
    :let (-describe (into context (:bindings model)) (:body model) data)
    :ref (-describe context (get context (:key model)) data)))

;; TODO: Treat the attributes independently of the type of the node in which they appear.
;;       That's a kind of composition pattern a-la-unity.


;;--

;; API

(defn valid?
  "Return true if the data matches the model, false otherwise."
  ([model data]
   (valid? {} model data))
  ([context model data]
   (boolean (-valid? context model data))))

;; WIP, do not use!
(defn ^:no-doc explain
  "Returns a structure describing what parts of the data are not matching the model."
  [model data])

(defn describe
  "Returns a descriptions of the data's structure."
  ([model data]
   (describe model data {}))
  ([model data options]
   (let [description (-describe {} model data)]
     (if (:valid? description)
       (:desc description)
       (:invalid-result options :invalid)))))

;; WIP, do not use!
(defn ^:no-doc undescribe
  "Returns a data which matches a description."
  [model description])

;;---

(comment
  ;; Not in the core, not urgent.
  (defn visit [model travel-plan data])

  ;; Not in the core, not urgent, maybe not needed.
  (defn transform [model transformer data]))
