## Data Parsing

```clojure
(require '[minimallist.core :refer [describe]])
(require '[minimallist.helper :as h])

(describe (h/fn string?) "Hello, world!")
;=> "Hello, world"

(describe (h/cat (h/fn int?)
                 (h/alt [:option1 (h/fn string?)]
                        [:option2 (h/fn keyword?)]
                        [:option3 (h/cat (h/fn string?)
                                         (h/fn keyword?))])
                 (h/fn int?))
          [1 "a" :b 3])
;=> [1 [:option3 ["a" :b]] 3]
```

A more complete documentation will come later.

In the mean time, please take a look at the test files
for more examples.
