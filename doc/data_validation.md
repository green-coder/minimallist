## Data Validation

```clojure
(require '[minimallist.core :refer [valid?]])
(require '[minimallist.helper :as h])

(valid? (h/fn string?) "Hello, world!")
;=> true
```
