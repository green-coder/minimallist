As a babashka shell script:

```clojure
#!/usr/bin/env bb

(require '[babashka.classpath :refer [add-classpath]]
         '[clojure.java.shell :refer [sh]])

(def deps '{:deps {minimallist {:git/url "https://github.com/green-coder/minimallist"
                                :sha "b373bb18b8868526243735c760bdc67a88dd1e9a"}}})
(def cp (:out (sh "clojure" "-Spath" "-Sdeps" (str deps))))
(add-classpath cp)


(require '[minimallist.core :as m])
(require '[minimallist.helper :as h])

(m/valid? (h/fn int?) 1)   ;=> true
(m/valid? (h/fn int?) "1") ;=> false

;; Does not work for now.
;(require '[clojure.test.check.generators :as tcg])
;(require '[minimallist.generator :as mg])
;(tcg/sample (mg/gen (h/fn int?)))
```
