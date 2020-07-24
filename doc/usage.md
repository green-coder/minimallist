# Usage

## Data Validation

```clojure
(require '[minimallist.core :refer [valid?]])
(require '[minimallist.helper :as h])

(valid? (h/fn string?) "Hello, world!")
;=> true
```

## Data Generation

```clojure
(require '[minimallist.generator :as mg])
(require '[clojure.test.check.generators :as tcg])

;; Implicit generation budget, varies with test.check's generator `size`.
(tcg/sample (mg/gen (h/vector-of mg/fn-string?)))
;([]
; []
; ["25"]
; ["525" "Eq8"]
; ["u" "IaDV" "JD"]
; ["69y" "sk"]
; ["0v" "" "M8" "rHD" "6qq0"]
; ["IWd8Ma" "6yF0" "9DK" "jIK"]
; ["HB6YheP" "tr3EQMj2" "j72Kw"]
; ["29gL" "GM5Q" "5mM" "Ju2y0Xs" "ywn248pp"])

;; With an explicit generation budget of 20.
(tcg/sample (mg/gen (h/vector-of mg/fn-string?)
                    20))
;(["" "" "" "" "" "" "" "" "" "" ""]
; ["6" "" "B" "H" "" "" ""]
; ["3" "lO" "MU" "9W" "M" "G1" ""]
; ["D6" "Tw1" "Wu" "" "oS" "JW1" "N"]
; ["q" "vv8" "" "3" "" "H" "" "" "" "ZfC8"]
; ["7L" "42P34" "" "g7" "K22" "43" "c5WR"]
; ["qYWis" "Pbk6" "02W" "0e2aTz" "L" "3" "130B"]
; ["S2S" "GW3" "hFZ" "0G05wI" "a" "3HFyI"]
; ["8O" "jr55e" "" "qp" "DGR" "Ma9" ""]
; ["zsH" "N0" "MY49r3v" "i3F" "J0Ih6r1" "A0y" "PQlSm" "uv" "u3U" "1EVFN9u"])


;; Recursive models
(def hiccup-model
  (h/let ['hiccup (h/alt [:node (h/in-vector (h/cat mg/fn-keyword?
                                                    (h/? (h/map))
                                                    (h/* (h/not-inlined (h/ref 'hiccup)))))]
                         [:primitive (h/alt mg/fn-nil?
                                            mg/fn-boolean?
                                            mg/fn-number?
                                            mg/fn-string?)])]
    (h/ref 'hiccup)))

;; 20 Samples using a generator with budget of 50.
(-> (mg/gen hiccup-model 50)
    (tcg/sample 20))
;([:! {} nil [:P]]
; [:_D {} "" false]
; [:U {} true -2.0 "" 2 0 true nil -3.0 true false]
; false
; [:!V {} "m" 4 "s" "V" "L" true [:Lh] "" false nil]
; true
; [:?- {} 1 "D" -3.5 true false "06tV" "tL" -2.0 true "r3qWdG" true]
; nil
; [:rqm {} [:K {}] [:I] [:*-+. {}] true]
; [:W {} 8 3 "0F"]
; false
; [:-_3! {} [:TY8 {} false] [:-Op]]
; [:a {} nil true 5 nil 4 nil]
; [:EK_6 false -1.36328125 nil true "953r" -0.546875 nil "etj" false true true]
; "fyaty"
; 1.0
; [:.?G!Y {} "5idQ" [:+-7.*w] 12 [:F] false nil [:ixj+63] nil 8]
; 15
; true
; false)
```

### Cat Ipsum generator

```clojure
;; Model builder
(defn ipsum-model [subjects verbs pronouns objects]
  (h/let ['subject (h/enum subjects)
          'verb    (h/enum verbs)
          'pronoun (h/enum pronouns)
          'object  (h/enum objects)
          'group   (h/alt (h/cat (h/ref 'pronoun)
                                 (h/ref 'object))
                          (h/cat (h/ref 'group)
                                 (h/enum #{"and" "with"})
                                 (h/ref 'group)))
          'sentence (h/cat (h/ref 'subject)
                           (h/ref 'verb)
                           (h/ref 'group))]
    (h/repeat 1 10 (h/not-inlined (h/ref 'sentence)))))

;; A model from the model builder
(def cat-ipsum (ipsum-model #{"I"
                              "My human"
                              "The baby"
                              "Bad mouses"}
                            #{"slept on"
                              "ate"
                              "played with"
                              "jumped over"
                              "stared at"
                              "scratched"
                              "broke"
                              "stretched nearby"
                              "was singing for"
                              "owned"}
                            #{"the" "a" "my" "*MY*"}
                            #{"fish" "dry food"
                              "colorful balls" "table"
                              "sofa" "bed"
                              "toy" "trash"
                              "apartment"
                              "toilet"}))

;; A generator from the model
(def cat-ipsum-generator
  (tcg/fmap (fn [sentences]
              (->> sentences
                   (map (fn [sentence]
                          (-> (str/join " " sentence)
                              (str "."))))
                   (str/join " ")))
            (mg/gen cat-ipsum 50)))

;; Generate data from the generator, and have fun
(tcg/sample cat-ipsum-generator)
;("Bad mouses broke the toilet. I scratched *MY* sofa. I slept on *MY* toilet. The baby jumped over the apartment. The baby stared at my toy. Bad mouses ate *MY* bed."
; "My human was singing for a toilet. My human slept on *MY* sofa with *MY* fish. My human owned a toilet."
; "My human was singing for a toilet. The baby ate *MY* trash. I owned the toy. I broke a trash. The baby jumped over my dry food. Bad mouses jumped over my sofa. I scratched the dry food."
; "My human ate the colorful balls. The baby played with the sofa. My human played with the toy. The baby played with a fish. My human was singing for my toilet. The baby stared at my colorful balls. Bad mouses scratched a sofa."
; "The baby stared at the fish. I broke my fish. My human ate my toilet. Bad mouses slept on a trash. The baby stretched nearby the apartment. The baby slept on my fish. I scratched *MY* sofa. My human was singing for a toy. Bad mouses ate *MY* colorful balls."
; "The baby was singing for *MY* toy. Bad mouses stared at my bed. I owned the apartment. My human played with *MY* fish. Bad mouses owned my apartment. Bad mouses stretched nearby my toy. I slept on *MY* apartment. My human was singing for my dry food. Bad mouses ate a dry food."
; "I broke a fish. I slept on a apartment. The baby broke the toilet. My human owned *MY* apartment. Bad mouses ate *MY* trash and my toilet. My human was singing for *MY* bed."
; "I ate my bed. My human was singing for a toy. The baby owned the toilet. The baby slept on *MY* fish."
; "The baby owned my bed. The baby played with a toilet. Bad mouses ate *MY* colorful balls. I scratched the sofa."
; "Bad mouses stared at my fish. My human ate *MY* toilet. My human stretched nearby the toilet. Bad mouses sleeped on my fish. My human scratched a fish. I broke the dry food.")
```

