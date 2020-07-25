(ns ^:no-doc minimallist.minicup
  (:require [minimallist.core :as m]
            [minimallist.helper :as h]))

;;
;; In this namespace, I am testing the library's usability from the user's perspective.
;;

;; Reference samples for minicup format, a hiccup-like form of Minimallist's models.
(def minicup-reference-samples
  [int? ; shorthand format
   [:fn int?]

   #(= 1 %) ; shorthand format
   [:fn {:gen #{1}} #(= 1 %)] ; hiccup format, with a place for the properties

   #{1 "2" :3}
   [:enum #{1 "2" :3}]

   [:and pos-int? even?]

   [:or pos-int? even?]

   [:set-of {:count #{2 3}} int?]

   [:map
    [:a int?]
    [:b {:optional true} string?]
    [(list 1 2 3) string?]]

   [:map-of keyword? int?]

   [:sequence-of {:count #{2 3}} int?] ; list or vector
   [:list-of {:count #{2 3}} int?] ; list only
   [:vector-of {:count #{2 3}} int?] ;vector only

   [:tuple int? int? int?] ; list or vector

   (list int? int? int?) ; maybe not a shorthand?
   [:list int? int? int?] ; list only

   [int? int? int?] ; maybe not a shorthand?
   [:vector int? int? int?] ; vector only

   [:alt
    [:int int?]
    [:keyword keyword?]
    [:ints     [:repeat 0 2 int?]]
    [:keywords [:repeat 0 2 keyword?]]]

   [:cat string? [:+ pos-int?] [:+ int?]]

   [:? int?]
   [:* int?]
   [:+ int?]
   [:repeat 0 2 int?]

   [:let ['foo int?
          :bar string?]
    [:vector [:ref 'foo] [:ref :bar]]]])


;; Model of the minicup format

(def minicup-model
  (let [anything-model (h/fn any?)
        entry-of (fn [entry-model]
                   (h/not-inlined (h/alt [:vector-entry (h/in-vector (h/cat [:key anything-model]
                                                                            [:properties (h/? (h/map))]
                                                                            [:model (h/not-inlined entry-model)]))]
                                         [:entry-model entry-model])))]
    (h/let ['model (h/alt
                     [:immediate-fn (h/fn fn?)]
                     [:fn (h/in-vector (h/cat (h/val :fn)
                                              (h/? (h/map [:gen {:optional true} anything-model]))
                                              (h/fn fn?)))]
                     [:immediate-enum (h/set-of anything-model)]
                     [:enum (h/in-vector (h/cat (h/val :enum)
                                                (h/set-of anything-model)))]
                     [:and (h/in-vector (h/cat (h/val :and)
                                               (h/+ (entry-of (h/ref 'model)))))]
                     [:or (h/in-vector (h/cat (h/val :or)
                                              (h/+ (entry-of (h/ref 'model)))))]
                     [:set-of (h/in-vector (h/cat (h/val :set-of)
                                                  (h/? (h/map [:count {:optional true} (h/ref 'model)]))
                                                  (h/not-inlined (h/ref 'model))))]
                     [:map (h/in-vector (h/cat (h/val :map)
                                               (h/+ (entry-of (h/ref 'model)))))]
                     [:map-of (h/vector (h/val :map-of)
                                        (h/ref 'model)
                                        (h/ref 'model))]
                     [:sequence-of (h/in-vector (h/cat (h/val :sequence-of)
                                                       (h/? (h/map [:count {:optional true} (h/ref 'model)]))
                                                       (h/not-inlined (h/ref 'model))))]
                     [:list-of (h/in-vector (h/cat (h/val :list-of)
                                                   (h/? (h/map [:count {:optional true} (h/ref 'model)]))
                                                   (h/not-inlined (h/ref 'model))))]
                     [:vector-of (h/in-vector (h/cat (h/val :vector-of)
                                                     (h/? (h/map [:count {:optional true} (h/ref 'model)]))
                                                     (h/not-inlined (h/ref 'model))))]
                     [:tuple (h/in-vector (h/cat (h/val :tuple)
                                                 (h/+ (entry-of (h/ref 'model)))))]
                     [:immediate-list (h/in-list (h/+ (entry-of (h/ref 'model))))]
                     [:list (h/in-vector (h/cat (h/val :list)
                                                (h/+ (entry-of (h/ref 'model)))))]
                     [:immediate-vector (h/in-vector (h/+ (entry-of (h/ref 'model))))]
                     [:vector (h/in-vector (h/cat (h/val :vector)
                                                  (h/+ (entry-of (h/ref 'model)))))]
                     [:alt (h/in-vector (h/cat (h/val :alt)
                                               (h/+ (entry-of (h/ref 'model)))))]
                     [:cat (h/in-vector (h/cat (h/val :cat)
                                               (h/+ (entry-of (h/ref 'model)))))]
                     [:? (h/vector (h/val :?)
                                   (h/ref 'model))]
                     [:* (h/vector (h/val :*)
                                   (h/ref 'model))]
                     [:+ (h/vector (h/val :+)
                                   (h/ref 'model))]
                     [:repeat (h/vector (h/val :repeat)
                                        (h/fn int?)
                                        (h/fn int?)
                                        (h/ref 'model))]
                     [:let (h/vector (h/val :let)
                                     (h/in-vector (h/+ (h/cat anything-model
                                                              (h/not-inlined (h/ref 'model)))))
                                     (h/ref 'model))]
                     [:ref (h/vector (h/val :ref) anything-model)])]
           (h/ref 'model))))

(comment
  (m/valid? minicup-model [:and int? int?])
  (m/describe minicup-model [:and int? int?]))
