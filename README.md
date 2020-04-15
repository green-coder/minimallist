## Minimallist

A minimalist data driven data model library, inspired by Spec and Malli.

## Usage

```clojure
(ns your-namespace
  (:require [minimallist.core :refer [valid?]]
            [minimallist.helper :as h]))

(def hiccup-model
  (h/let ['hiccup (h/alt [:node (h/in-vector (h/cat (h/fn keyword?)
                                                    (h/? (h/map))
                                                    (h/* (h/not-inlined (h/ref 'hiccup)))))]
                         [:primitive (h/or (h/fn nil?)
                                           (h/fn boolean?)
                                           (h/fn number?)
                                           (h/fn string?))])]
    (h/ref 'hiccup)))

(valid? hiccup-model [:div {:class [:foo :bar]}
                      [:p "Hello, world of data"]])
```

See the tests for more examples of how to use the helpers to build your models.

## Status

This is a work in progress, the API may change in the future.

More functionalities will be added later, once the API design is more stable.

If you find any bug or have comments, please create an issue in GitHub.

## License

The Minimallist library is developed by Vincent Cantin.
It is distributed under the terms of the Eclipse Public License version 2.0.
