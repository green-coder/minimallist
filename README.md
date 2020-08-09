## Minimallist [![CircleCI](https://circleci.com/gh/green-coder/minimallist.svg?style=svg)](https://circleci.com/gh/green-coder/minimallist)

A minimalist data driven data model library, inspired by [Clojure Spec](https://clojure.org/guides/spec) and [Malli](https://github.com/metosin/malli).

[![Clojars Project](https://img.shields.io/clojars/v/minimallist.svg)](https://clojars.org/minimallist)
[![cljdoc badge](https://cljdoc.org/badge/minimallist/minimallist)](https://cljdoc.org/d/minimallist/minimallist/CURRENT)
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C012HUX1VPC)
[![cljdoc badge](https://img.shields.io/clojars/dt/minimallist?color=opal)](https://clojars.org/minimallist)

## Usage

```clojure
(ns your-namespace
  (:require [minimallist.core :refer [valid?]]
            [minimallist.helper :as h]))

(def hiccup-model
  (h/let ['hiccup (h/alt [:node (h/in-vector (h/cat [:name (h/fn keyword?)]
                                                    [:props (h/? (h/map-of (h/fn keyword?) (h/fn any?)))]
                                                    [:children (h/* (h/not-inlined (h/ref 'hiccup)))]))]
                         [:primitive (h/alt [:nil (h/fn nil?)]
                                            [:boolean (h/fn boolean?)]
                                            [:number (h/fn number?)]
                                            [:text (h/fn string?)])])]
    (h/ref 'hiccup)))

(valid? hiccup-model [:div {:class [:foo :bar]}
                      [:p "Hello, world of data"]])
;=> true

(describe hiccup-model [:div {:class [:foo :bar]}
                        [:p "Hello, world of data"]])
;=> [:node {:name :div,
;           :props [{:class [:foo :bar]}],
;           :children [[:node {:name :p
;                              :props []
;                              :children [[:primitive [:text "Hello, world of data"]]]}]]}]
```

## Features

- validates, parses and generates data,
- fully data driven, models are hash-map based created via helpers,
- support recursive definitions and sequence regex,
- no macro, no static registry, pure functions,
- relatively simple implementation, easy to read and modify,
- cross platform (`.cljc`),
- `valid?` and `describe` run in [Babashka](https://github.com/borkdude/babashka)

## Non-goals (for now)

- does not integrate with anything else,
- does not try hard to be performant

## Documentation

See the [latest documentation on cljdoc](https://cljdoc.org/d/minimallist/minimallist/CURRENT) for:
- A general description of the Minimallist project.
- How to use the helpers to build your models.
- How to validate, parse and generate your data.

## Status

This is a work in progress, the API may change in the future.

More functionalities will be added later, once the API design is more stable.

If you find any bug or have comments, please create an issue in GitHub.

## License

The Minimallist library is developed by Vincent Cantin.
It is distributed under the terms of the Eclipse Public License version 2.0.
