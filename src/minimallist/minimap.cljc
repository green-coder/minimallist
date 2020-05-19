(ns minimallist.minimap
  (:require [minimallist.core :as m]
            [minimallist.helper :as h]))

;; A model of the model format accepted by Minimallist's functions.
;; Use it to validate your models when you have problems which you don't understand, just in case.
(def minimap-model
  (h/let ['model (h/alt [:fn (h/map [:type (h/val :fn)]
                                    [:fn (h/fn fn?)])]
                        [:enum (h/map [:type (h/val :enum)]
                                      [:values (h/set)])]
                        [:and-or (h/map [:type (h/enum #{:and :or})]
                                        [:entries (h/vector-of (h/map [:model (h/ref 'model)]))])]
                        [:set (-> (h/map [:type (h/val :set)])
                                  (h/with-optional-entries [[:count-model (h/ref 'model)]
                                                            [:condition-model (h/ref 'model)]]))]
                        [:set-of (-> (h/map [:type (h/val :set-of)])
                                     (h/with-optional-entries [[:count-model (h/ref 'model)]
                                                               [:elements-model (h/ref 'model)]
                                                               [:condition-model (h/ref 'model)]]))]
                        [:map-of (-> (h/map [:type (h/val :map-of)])
                                     (h/with-optional-entries [[:keys (h/map [:model (h/ref 'model)])]
                                                               [:values (h/map [:model (h/ref 'model)])]
                                                               [:condition-model (h/ref 'model)]]))]
                        [:map (-> (h/map [:type (h/val :map)])
                                  (h/with-optional-entries [[:entries (h/vector-of (-> (h/map [:key (h/fn any?)]
                                                                                              [:model (h/ref 'model)])
                                                                                       (h/with-optional-entries [[:optional (h/fn boolean?)]])))]
                                                            [:condition-model (h/ref 'model)]]))]
                        [:sequence-of (-> (h/map [:type (h/val :sequence-of)])
                                          (h/with-optional-entries [[:coll-type (h/enum #{:any :list :vector})]
                                                                    [:count-model (h/ref 'model)]
                                                                    [:elements-model (h/ref 'model)]
                                                                    [:condition-model (h/ref 'model)]])
                                          (h/with-condition (h/fn #(not (and (or (contains? % :count-model)
                                                                                 (contains? % :elements-model))
                                                                             (contains? % :entries))))))]
                        [:sequence (-> (h/map [:type (h/val :sequence)])
                                       (h/with-optional-entries [[:coll-type (h/enum #{:any :list :vector})]
                                                                 [:entries (h/vector-of (h/map [:model (h/ref 'model)]))]
                                                                 [:condition-model (h/ref 'model)]])
                                       (h/with-condition (h/fn #(not (and (or (contains? % :count-model)
                                                                              (contains? % :elements-model))
                                                                          (contains? % :entries))))))]
                        [:alt (-> (h/map [:type (h/val :alt)]
                                         [:entries (h/vector-of (h/map [:key (h/fn any?)]
                                                                       [:model (h/ref 'model)]))])
                                  (h/with-optional-entries [[:inlined (h/fn boolean?)]]))]
                        [:cat (-> (h/map [:type (h/val :cat)]
                                         [:entries (h/vector-of (h/map [:model (h/ref 'model)]))])
                                  (h/with-optional-entries [[:coll-type (h/enum #{:any :list :vector})]
                                                            [:count-model (h/ref 'model)]
                                                            [:inlined (h/fn boolean?)]
                                                            [:condition-model (h/ref 'model)]]))]
                        [:repeat (-> (h/map [:type (h/val :repeat)]
                                            [:min (h/or (h/val 0)
                                                        (h/fn pos-int?))]
                                            [:max (h/or (h/fn pos-int?)
                                                        (h/val ##Inf))]
                                            [:elements-model (h/ref 'model)])
                                     (h/with-optional-entries [[:coll-type (h/enum #{:any :list :vector})]
                                                               [:count-model (h/ref 'model)]
                                                               [:inlined (h/fn boolean?)]
                                                               [:condition-model (h/ref 'model)]])
                                     (h/with-condition (h/fn #(<= (:min %) (:max %)))))]
                        [:let (h/map [:type (h/val :let)]
                                     [:bindings (h/in-vector (h/+ (h/cat (h/fn any?)
                                                                         (h/ref 'model))))]
                                     [:body (h/ref 'model)])]
                        [:ref (h/map [:type (h/val :ref)])])]
         (h/ref 'model)))

(comment
  ;; Just for fun
  (m/valid? minimap-model minimap-model)

  ;; This is how you normally use it
  ;; (This will be a lot more useful after the explain function is implemented)
  (m/valid? minimap-model (h/fn int?)))
