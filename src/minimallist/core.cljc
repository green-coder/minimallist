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

(defn- comp-bindings [context bindings]
  (conj context bindings))

(defn- resolve-ref [context key]
  (loop [bindings-vector context]
    (if (seq bindings-vector)
      (let [bindings (peek bindings-vector)]
        (if (contains? bindings key)
          [bindings-vector (get bindings key)]
          (recur (pop bindings-vector))))
      (throw (ex-info (str "Cannot resolve reference " key)
                      {:context context, :key key})))))

(defn- s-sequential? [x]
  (or (sequential? x)
      (string? x)))

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
      :let (left-overs (comp-bindings context (:bindings model)) (:body model) seq-data)
      :ref (let [[context model] (resolve-ref context (:key model))]
             (left-overs context model seq-data)))
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
                        (implies (contains? model :entry-model)
                                 (every? (partial -valid? context (:entry-model model)) data))
                        (implies (contains? model :condition-model)
                                 (-valid? context (:condition-model model) data)))
    (:sequence-of :sequence) (and (s-sequential? data)
                                  ((-> (:coll-type model :any) {:any any?
                                                                :list seq?
                                                                :vector vector?
                                                                :string string?}) data)
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
    (:cat :repeat) (and (s-sequential? data)
                        ((-> (:coll-type model :any) {:any any?
                                                      :list seq?
                                                      :vector vector?
                                                      :string string?}) data)
                        (some nil? (left-overs context (dissoc model :inlined) (seq data)))
                        (implies (contains? model :count-model)
                                 (-valid? context (:count-model model) (count data)))
                        (implies (contains? model :condition-model)
                                 (-valid? context (:condition-model model) data)))
    :let (-valid? (comp-bindings context (:bindings model)) (:body model) data)
    :ref (let [[context model] (resolve-ref context (:key model))]
           (-valid? context model data))))

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
      :let (sequence-descriptions (comp-bindings context (:bindings model)) (:body model) seq-data)
      :ref (let [[context model] (resolve-ref context (:key model))]
             (sequence-descriptions context model seq-data)))
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
    :fn {:valid? (and ((:fn model) data)
                      (implies (contains? model :condition-model)
                               (:valid? (-describe context (:condition-model model) data))))
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
                              (mapv (partial -describe context (:elements-model model)) data))
                    valid? (and (implies (contains? model :elements-model)
                                         (every? :valid? entries))
                                (implies (contains? model :count-model)
                                         (:valid? (-describe context (:count-model model) (count data))))
                                (implies (contains? model :condition-model)
                                         (:valid? (-describe context (:condition-model model) data))))]
                {:valid? valid?
                 :desc (mapv :desc entries)}))
    :map-of (if (map? data)
              (let [entries (mapv (partial -describe context (:entry-model model)) data)
                    valid? (and (every? :valid? entries)
                                (implies (contains? model :condition-model)
                                         (:valid? (-describe context (:condition-model model) data))))]
                {:valid? valid?
                 :desc (mapv :desc entries)})
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
                                      (:valid? (-describe context (:condition-model model) data))))]
             {:valid? valid?
              :desc (into {} (map (fn [[k v]] [k (:desc v)])) entries)})
           {:valid? false})
    (:sequence-of :sequence) (if (s-sequential? data)
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
                                                    :list seq?
                                                    :vector vector?
                                                    :string string?} (:coll-type model :any)) data)
                                                 (implies (contains? model :entries)
                                                          (and (= (count (:entries model)) (count data))
                                                               (every? :valid? entries)))
                                                 (implies (contains? model :count-model)
                                                          (:valid? (-describe context (:count-model model) (count data))))
                                                 (implies (contains? model :elements-model)
                                                          (every? :valid? entries))
                                                 (implies (contains? model :condition-model)
                                                          (:valid? (-describe context (:condition-model model) data))))]
                                 {:valid? valid?
                                  :desc (if (and (= (:type model) :sequence)
                                                 (some #(contains? % :key) (:entries model)))
                                          (->> (map (fn [model-entry described-entry]
                                                      (when (contains? model-entry :key)
                                                        [(:key model-entry) (:desc described-entry)]))
                                                    (:entries model)
                                                    entries)
                                               (filter some?)
                                               (into {}))
                                          (mapv :desc entries))})
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
    (:cat :repeat) (if (and (s-sequential? data)
                            ((-> (:coll-type model :any) {:any any?
                                                          :list seq?
                                                          :vector vector?
                                                          :string string?}) data)
                            (implies (contains? model :count-model)
                                     (:valid? (-describe context (:count-model model) (count data)))))
                     (let [seq-descriptions (filter (comp nil? :rest-seq)
                                                    (sequence-descriptions context model (seq data)))]
                       (if (seq seq-descriptions)
                         {:desc (:desc (first seq-descriptions))
                          :valid? (implies (contains? model :condition-model)
                                           (:valid? (-describe context (:condition-model model) data)))}
                         {:valid? false}))
                     {:valid? false})
    :let (-describe (comp-bindings context (:bindings model)) (:body model) data)
    :ref (let [[context model] (resolve-ref context (:key model))]
           (-describe context model data))))

;; TODO: Treat the attributes independently of the type of the node in which they appear.
;;       That's a kind of composition pattern a-la-unity.


;;--

;; API

(defn valid?
  "Return true if the data matches the model, false otherwise."
  [model data]
  (boolean (-valid? [] model data)))

;; WIP, do not use!
(defn ^:no-doc explain
  "Returns a structure describing what parts of the data are not matching the model."
  [model data])

(defn describe
  "Returns a descriptions of the data's structure."
  ([model data]
   (describe model data {}))
  ([model data options]
   (let [description (-describe [] model data)]
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
