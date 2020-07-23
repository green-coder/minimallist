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
