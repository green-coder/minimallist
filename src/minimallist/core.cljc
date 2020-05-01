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

;;---

(defmacro implies [condition & expressions]
  `(if ~condition
     (do ~@expressions)
     true))

(declare valid?)
(declare describe)

(defn- left-overs
  "Returns a sequence of possible left-overs from the seq-data after matching the model with it."
  [context model seq-data]
  (cond
    (= (:type model) :alt)
    (mapcat (fn [entry]
              (left-overs context (:model entry) seq-data))
            (:entries model))

    (and (= (:type model) :cat)
         (:inlined model true))
    (let [f (fn -left-overs [seq-entries seq-data]
              (if seq-entries
                (let [[{:keys [model]} & next-entries] seq-entries
                      left-overs-coll (left-overs context model seq-data)]
                  (mapcat (partial -left-overs next-entries) left-overs-coll))
                (list seq-data)))]
      (f (seq (:entries model)) seq-data))

    (and (= (:type model) :repeat)
         (:inlined model true))
    (let [{:keys [min max elements-model]} model
          f (fn -left-overs [nb-matched seq-data]
              (if (< nb-matched max)
                (let [left-overs-coll (left-overs context elements-model seq-data)
                      rest (mapcat (partial -left-overs (inc nb-matched)) left-overs-coll)]
                  (if (<= min nb-matched)
                    (cons seq-data rest)
                    rest))
                (list seq-data)))]
      (f 0 seq-data))

    :else
    (if (and seq-data
             (valid? context (dissoc model :inlined) (first seq-data)))
      (list (next seq-data))
      '())))

(defn- sequence-descriptions
  "Returns a sequence of possible descriptions from the seq-data matching a model."
  [context model seq-data]
  (cond
    (= (:type model) :alt)
    (mapcat (fn [entry]
              (sequence-descriptions context (:model entry) seq-data))
            (:entries model))

    (and (= (:type model) :cat)
         (:inlined model true))
    (let [f (fn -sequence-descriptions [seq-entries seq-data]
              (if seq-entries
                (let [[{:keys [model]} & next-entries] seq-entries
                      descriptions (sequence-descriptions context model seq-data)]
                  (mapcat (partial -sequence-descriptions next-entries) descriptions))
                (list seq-data)))]
      (f (seq (:entries model)) seq-data))

    (and (= (:type model) :repeat)
         (:inlined model true))
    (let [{:keys [min max elements-model]} model
          f (fn -sequence-descriptions [nb-matched acc]
              (if (< nb-matched max)
                (let [seq-descriptions (map (fn [seq-description]
                                              {:length (+ (:length acc) (:length seq-description))
                                               :rest-seq (:rest-seq seq-description)
                                               :entries (into (:entries acc) (:entries seq-description))})
                                            (sequence-descriptions context elements-model (:rest-seq acc)))]
                  (cond->> (mapcat (partial -sequence-descriptions (inc nb-matched)) seq-descriptions)
                           (<= min (inc nb-matched)) (concat seq-descriptions)))
                '()))]
      ; TODO: filter on :valid?
      (cond->> (f 0 {:length 0
                     :rest-seq seq-data
                     :entries []})
               (<= min 0) (concat [{:length 0
                                    :rest-seq seq-data
                                    :entries []}])))

    :else
    (if-let [description (and seq-data
                              (describe context (dissoc model :inlined) (first seq-data)))]
      (list {:length 1
             :rest-seq (next seq-data)
             :entries [description]})
      '())))

(comment
  (sequence-descriptions {}
                         {:type :fn
                          :fn int?}
                         (seq [1]))
  => [{:length 1,
       :rest-seq nil,
       :entries [{:valid? true, :data 1}]}]

  (sequence-descriptions {}
                         ; [:* int?]
                         {:type :repeat
                          :min 0
                          :max ##Inf
                          :elements-model {:type :fn
                                           :fn int?}}
                         (seq [1 :2])) ; TODO: filter on :valid?

  (sequence-descriptions {}
                         ; [:+ int?]
                         {:type :repeat
                          :min 1
                          :max ##Inf
                          :elements-model {:type :fn
                                           :fn int?}}
                         (seq [1 2 3]))
  => ; result of the whole function
  [; result of a :repeat of 1 element
   {:length 1
    :rest-seq '(2 3)
    :entries [{:valid? true, :data 1}]}
   ; result of a :repeat of 2 elements
   {:length 2
    :rest-seq '(3)
    :entries [{:valid? true, :data 1}
              {:valid? true, :data 2}]}
   ; result of a :repeat of 3 elements
   {:length 3
    :rest-seq 'nil
    :entries [{:valid? true, :data 1}
              {:valid? true, :data 2}
              {:valid? true, :data 3}]}]


  (sequence-descriptions {}
                         ; [:cat :foo1 [:+ pos-int?]
                         ;       :foo2 [:+ int?]]
                         {:type :cat
                          :entries [{:key :foo1
                                     :model {:type :repeat
                                             :min 1
                                             :max ##Inf
                                             :elements-model {:type :fn
                                                              :fn pos-int?}}}
                                    {:key :foo2
                                     :model {:type :repeat
                                             :min 1
                                             :max ##Inf
                                             :elements-model {:type :fn
                                                              :fn int?}}}]}
                         [3 4 0 2])

  ;; the numbers are descriptions of themselves
  [{:length 2
    :rest-seq '(0 2)
    :entries {:foo1 {; :length 1
                     ; :rest-seq '(4 0 2)
                     :entries [3]}
              :foo2 {; :length 1
                     ; :rest-seq '(0 2)
                     :entries [4]}}}
   {:length 3
    :rest-seq '(2)
    :entries {:foo1 {:entries [3]}
              :foo2 {:entries [4 0]}}}
   {:length 4
    :rest-seq nil
    :entries {:foo1 {:entries [3]}
              :foo2 {:entries [4 0 2]}}}
   {::length 4
    :entries {:foo1 {:entries [3 4]}
              :foo2 {:entries [0 2]}}}])


;;---

;; API
(defn valid?
  "Return true if the data matches the model, false otherwise."
  ([model data]
   (valid? {} model data))
  ([context model data]
   (case (:type model)
     :fn ((:fn model) data)
     :enum (contains? (:values model) data)
     :and (every? (fn [entry]
                    (valid? context (:model entry) data))
                  (:entries model))
     (:or :alt) (some (fn [entry]
                        (valid? context (:model entry) data))
                      (:entries model))
     :set (and (set? data)
               (implies (contains? model :count-model)
                        (valid? context (:count-model model) (count data)))
               (every? (partial valid? context (:elements-model model)) data))
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
                    ((-> (:coll-type model :any) {:any any?
                                                  :list list?
                                                  :vector vector?}) data)
                    (implies (contains? model :elements-model)
                             (every? (partial valid? context (:elements-model model)) data))
                    (implies (contains? model :count-model)
                             (valid? context (:count-model model) (count data)))
                    (implies (contains? model :entries)
                             (and (= (count (:entries model)) (count data))
                                  (every? true? (map (fn [entry data-element]
                                                       (valid? context (:model entry) data-element))
                                                     (:entries model)
                                                     data)))))
     (:cat :repeat) (and (sequential? data)
                         ((-> (:coll-type model :any) {:any any?
                                                       :list list?
                                                       :vector vector?}) data)
                         (some nil? (left-overs context model (seq data))))
     :let (valid? (merge context (:bindings model)) (:body model) data)
     :ref (valid? context (get context (:ref model)) data))))

(defn explain
  "Returns a structure describing what parts of the data are not matching the model."
  [model data])

;; My hardship comes from the implementation of :and and :or,
;; the rest is a piece of cake.

;; There are 2 kinds of predicates:
;; - structural (they test the existence of a structure),
;; - non-structural (they test properties on structureless values).

;; Structural predicates can be predefined extensively:
;; [:set :map :sequence :alt :cat :repeat]

;; Non-structural predicates are everything else, for instance:
;; [:fn :enum :and :or]

;; To be considered:
;; - using :alt instead of :or when testing on different kind of structures,
;;   in other word:
;;   - :or is only for non-structural models, (e.g. [:or odd? prime?])
;;   - :alt only for structural models (e.g. [:alt [:my-set set?] [:my-vec vector?]])

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
                (let [entries (into #{} (map (partial describe context (:elements-model model))) data)]
                  {:valid? (and (implies (contains? model :count-model)
                                         (:valid? (describe context (:count-model model) (count data))))
                                (every? :valid? entries))
                   :entries entries})
                {:valid? false})
         :map (if (map? data)
                (let [entries (when (or (contains? model :entries)
                                        (contains? model :values))
                                (into {}
                                      (map (fn [entry]
                                             [(:key entry)
                                              (if (contains? data (:key entry))
                                                (describe context (:model entry) (get data (:key entry)))
                                                {:missing? true})]))
                                      (:entries model)))]
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
                                                         (keys data))))}
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
                                             (implies (contains? model :elements-model)
                                                      (every? :valid? entries))
                                             (implies (contains? model :count-model)
                                                      (:valid? (describe context (:count-model model) (count data))))
                                             (implies (contains? model :entries)
                                                      (and (= (count (:entries model)) (count data))
                                                           (every? :valid? entries))))}
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
                          (let [descriptions (filter (comp nil? :rest-seq)
                                                     (sequence-descriptions context model (seq data)))]
                            (if (seq descriptions)
                              {:entries (:entries (first descriptions))
                               :valid? true}
                              {:valid? false}))
                          {:valid? false})
         :let (describe (merge context (:bindings model)) (:body model) data)
         :ref (describe context (get context (:ref model)) data))
       (assoc :context context ;; maybe: (post-fn description context model data)
              :model model
              :data data))))

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
  (defn transform [model transformer data]))
