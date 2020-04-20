(ns minimallist.core)

(comment
  ;; Format which *may* be used by the end user later
  [:map [:x int?]]

  ;; Format used in the library's code
  {:type :map
   :entries [{:key :x
              :model {:type :fn
                      :name int?}}]}

  ;; Supported node types
  [:and :or
   :map :map-of :coll-of :sequence :list :vector :set :tuple :enum
   :fn :let :ref
   :cat :alt :repeat])

;;---

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
     :and (every? (fn [entry]
                    (valid? context (:model entry) data))
                  (:entries model))
     :or (boolean (some (fn [entry]
                          (valid? context (:model entry) data))
                        (:entries model)))
     :map (and (map? data)
               (every? (fn [entry]
                         (and (contains? data (:key entry))
                              (valid? context (:model entry) (get data (:key entry)))))
                       (:entries model)))
     :map-of (and (map? data)
                  (every? (partial valid? context (-> model :key :model)) (keys data))
                  (every? (partial valid? context (-> model :value :model)) (vals data)))
     :coll-of (and (coll? data)
                   (every? (partial valid? context (:model model)) data))
     :sequence (and (sequential? data)
                    (every? (partial valid? context (:model model)) data))
     :list (and (list? data)
                (every? (partial valid? context (:model model)) data))
     :vector (and (vector? data)
                  (every? (partial valid? context (:model model)) data))
     :set (and (set? data)
               (every? (partial valid? context (:model model)) data))
     :tuple (and (sequential? data)
                 (= (count (:entries model)) (count data))
                 (every? identity (map valid? context (:entries model) data)))
     :enum (contains? (:values model) data)
     :fn (boolean ((:fn model) data))
     :let (valid? (merge context (:bindings model)) (:body model) data)
     :ref (valid? context (get context (:ref model)) data)
     (:cat :alt :repeat) (and (sequential? data)
                              (boolean (some nil? (left-overs context model (seq data))))))))

(defn explain
  "Returns a structure describing what parts of the data are not matching the model."
  [model data])

(defn conform
  "Returns a sequence of possible descriptions of the data's structure, in the shape of the model."
  ([model data]
   (conform {} model data))
  ([context model data]
   (case (:type model)
     :and (reduce (fn [data entry]
                    (mapcat (partial conform context (:model entry)) data))
                  [data]
                  (:entries model))
     :or (mapcat (fn [entry]
                   (let [results (conform context (:model entry) data)]
                     (if (contains? entry :key)
                       (map (fn [result] [(:key entry) result]) results)
                       results)))
                 (:entries model))
     :map (if (map? data)
            (let [conformed-entries (map (fn [{:keys [key model]}]
                                           [key (conform context model (get data key))])
                                         (:entries model))]
              (reduce (fn [results [key conformed-values]]
                        (mapcat (fn [result]
                                  (map (fn [conformed-value]
                                         (assoc result key conformed-value))
                                       conformed-values))
                                results))
                      [{}]
                      conformed-entries))
            [])
     ;:map-of (and (map? data)
     ;             (every? (partial valid? context (-> model :key :model)) (keys data))
     ;             (every? (partial valid? context (-> model :value :model)) (vals data)))
     :map-of (if (map? data)
               (let [conformed-entries (map (fn []))])
               [])
     :coll-of nil
     :sequence nil
     :list nil
     :vector nil
     :set nil
     :tuple nil
     :enum (if (contains? (:values model) data) [data] [])
     :fn (if ((:fn model) data) [data] [])
     :let (conform (merge context (:bindings model)) (:body model) data)
     :ref (conform context (get context (:ref model)) data)
     (:cat :alt :repeat) nil)))

(defn unform
  "Returns a data which matches a description, in the shape represented by the model."
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
