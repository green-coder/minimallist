## Minimallist [![CircleCI](https://circleci.com/gh/green-coder/minimallist.svg?style=svg)](https://circleci.com/gh/green-coder/minimallist)

A minimalist data driven data model library, inspired by [Clojure Spec](https://clojure.org/guides/spec) and [Malli](https://github.com/metosin/malli).

[![Clojars Project](https://img.shields.io/clojars/v/minimallist.svg)](https://clojars.org/minimallist)
[![cljdoc badge](https://cljdoc.org/badge/minimallist/minimallist)](https://cljdoc.org/d/minimallist/minimallist/CURRENT)
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C012HUX1VPC)

## Usage

```clojure
(ns your-namespace
  (:require [minimallist.core :refer [valid?]]
            [minimallist.helper :as h]))

(def hiccup-model
  (h/let ['hiccup (h/alt [:node (h/in-vector (h/cat (h/fn keyword?)
                                                    (h/? (h/map))
                                                    (h/* (h/not-inlined (h/ref 'hiccup)))))]
                         [:primitive (h/alt (h/fn nil?)
                                            (h/fn boolean?)
                                            (h/fn number?)
                                            (h/fn string?))])]
    (h/ref 'hiccup)))

(valid? hiccup-model [:div {:class [:foo :bar]}
                      [:p "Hello, world of data"]])
;=> true
```

## Features

- validates and generates data,
- fully data driven, models are hash-map based created via helpers,
- support recursive definitions and sequence regex,
- no macro, no static registry, pure functions,
- relatively simple implementation, easy to read and modify,
- cross platform (`.cljc`),
- `valid?` runs in [Babashka](https://github.com/borkdude/babashka)

## Non-goals (for now)

- does not integrate with anything else,
- does not try hard to be performant

## Documentation

See the [latest documentation on cljdoc](https://cljdoc.org/d/minimallist/minimallist/CURRENT) for:
- A general description of the Minimallist project.
- How to use the helpers to build your models.
- How to validate and generate your data.

## Status

This is a work in progress, the API may change in the future.

More functionalities will be added later, once the API design is more stable.

If you find any bug or have comments, please create an issue in GitHub.

## License

The Minimallist library is developed by Vincent Cantin.
It is distributed under the terms of the Eclipse Public License version 2.0.
