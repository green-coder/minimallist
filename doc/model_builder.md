## Introduction

There are many ways to create Minimallist's model.

The namespace `minimallist.helper` provides one way to do it via
a small set of functions, hopefully a smaller way to write
(and read) them:

```clojure
(require '[minimallist.helper :as h])

(h/map [:me (h/map [:name (h/fn string?)]
                   [:age (h/fn int?)])]
       [:best-friend (h/map [:name (h/fn string?)]
                            [:age (h/fn int?)])])

(h/let ['person (h/map [:name (h/fn string?)]
                       [:age (h/fn int?)])]
       (h/map [:me (h/ref 'person)]
              [:best-friend (h/ref 'person)]))

(h/let ['person (h/map [:name (h/fn string?)]
                       [:friends (h/vector-of (h/ref 'person))])]
       (h/ref 'person))
```

## Usage

The helper function are very simple to use and combine.
They are also documented in the source code.

### Custom predicate

Useful for anything not directly supported by Minimallist.

```clojure
;; A string
(h/fn string?)

;; An odd number
(-> (h/fn int?)
    (h/with-condition (h/fn odd?)))

;; An odd number between 300 and 309
(require '[clojure.test.check.generators :as tcg])
(-> (h/fn int?)
    (h/with-condition (h/and (h/fn #(<= 300 % 309))
                             (h/fn odd?)))
    (h/with-test-check-gen (tcg/fmap (fn [n] (+ 300 n n))
                                     (tcg/choose 0 4))))
```

`h/fn` is the interface between Minimallist and the rest of the world.
Any integration with a 3rd party library should be based on this node.

### Fixed value

This data model represents a fixed value.

```clojure
(h/val 7)

;; Can be used on any Clojure value.
(let [initial-inventory #{:sword :shield}]
  (h/val initial-inventory))
```

### One value out of a set

Represents a value which belongs to a set of pre-defined values.

```clojure
(h/enum #{:water :fire :earth :wind})
```

### And / Or

Data model used for validation using conjunctions or disjunctions of
other logical predicates.

```clojure
(-> (h/fn int?)
    (h/with-condition (h/or (h/fn #(<= 0 % 9))
                            (h/val 42))))
```

Those models are not supported by Minimallist's generator. If you want to use it
outside of a `:condition-model` field, you need to add your own generator to their nodes.

```clojure
;; Minimallist's generator can use this model to generate data.
(-> (h/or (h/fn #(<= 0 % 9))
          (h/val 42))
    (h/with-test-check-gen (tcg/one-of [(tcg/choose 0 9)
                                        (tcg/return 42)])))
```

### Collections `-of`

`set-of`, `map-of`, `sequence-of` represent collections of items of the same model.

```clojure
;; A set of keywords.
(h/set-of (h/fn keyword?))

;; Map of id->name.
(h/map-of (h/vector (h/fn int?) (h/fn string?)))

;; Sequence of numbers, either in a list or in a vector.
(h/sequence-of (h/fn int?))
```

`list-of`, `vector-of` and `string-of` are shortcuts to define at the same time a `:sequence-of` node
with a `:coll-type` set to `:list`, `:vector` or `:string`.

```clojure
;; A list of numbers.
(h/list-of (h/fn int?))

;; A vector of numbers.
(h/vector-of (h/fn int?))

;; A string of chars (i.e. the only thing a string can contain)
(h/string-of (h/fn char?))
```

### Collections with entries

`map` and `tuple` represent collections where each item has its own model
specified using entries.

During data parsing, map entries are identified via their key, while tuple entries are either
identified via their key or their index in the sequence.

```clojure
(h/map [:name (h/fn string?)]
       [:age (h/fn int?)]
       [:gender {:optional true} (h/fn any?)])

(h/tuple [:first (h/fn int?)]
         (h/fn string?))
```

`h/list`, `h/vector` and `h/string-tuple` are shortcuts to define at the same time a `:sequence` node
(i.e. a `h/tuple`) with a `:coll-type` set to `:list`, `:vector` or `:string`.

```clojure
;; A list containing an integer followed by a string.
(h/list [:first (h/fn int?)]
        (h/fn string?))

;; A vector containing an integer followed by a string.
(h/vector [:first (h/fn int?)]
          (h/fn string?))

;; A string containing an operator char followed by a digit char.
;; Notice the convenient char-set helper.
(h/string-tuple [:first (h/enum #{\* \+ \- \/})]
                #_(h/enum #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
                (h/char-set "0123456789"))
```

### Alt

If your model can be either `A` or `B`, use the `:alt` node using `h/alt`.

```clojure
;; Entries will be identified using their index.
(h/alt (h/fn nil?)
       (h/fn boolean?)
       (h/fn number?)
       (h/fn string?))

;; Entries will be identified using their key.
(h/alt [:nil (h/fn nil?)]
       [:boolean (h/fn boolean?)]
       [:number (h/fn number?)]
       [:string (h/fn string?)])
```

### Cat / Repeat

`:cat` and `:repeat` nodes represent sequences of models.

```clojure
;; Sequential collection (list or vector) which contains
;; 1 to 3 booleans, followed by
;; 0 or 1 time integers, followed by
;; at least 1 string, followed by
;; any number of keywords.
(h/cat (h/repeat 1 3 (h/fn boolean?))
       (h/repeat 0 1 (h/fn int?))
       (h/repeat 1 ##Inf (h/fn string?))
       (h/repeat 0 ##Inf (h/fn keyword?)))

;; The same model, but written using shortcuts.
(h/cat (h/repeat 1 3 (h/fn boolean?))
       (h/? (h/fn int?))
       (h/+ (h/fn string?))
       (h/* (h/fn keyword?)))
```

We can specify the type of collection used to contain a sequence.

```clojure
;; A sequence of things which has to be inside a vector.
(-> (h/cat (h/? (h/fn int?))
           (h/fn string?))
    h/in-vector)

;; Same model, but which has to be inside a list.
(-> (h/cat (h/? (h/fn int?))
           (h/fn string?))
    h/in-list)

;; A model for chars inside a string, an interesting alternative to regex.
;; Notice the convenient char-cat and char-set helper functions.
(-> (h/cat (h/char-cat "My favorite number is ")
           (h/char-set "123456789")
           (h/repeat 0 2 (h/char-set "0123456789"))
           (h/char-cat " and my favorite color is ")
           (h/alt (h/char-cat "red")
                  (h/char-cat "green")
                  (h/char-cat "blue"))
           (h/val \.))
    h/in-string)
```

When a sequence is inside another sequence, it is considered inlined by default.
With `h/not-inlined`, it will be contained in its own a collection (list or vector).

```clojure
;; Matches '(1 2 3 "Soleil")
(h/cat (h/+ (h/fn int?))
       (h/fn string?))

;; Matches '([1 2 3] "Soleil")
(h/cat (-> (h/+ (h/fn int?))
           h/not-inlined)
       (h/fn string?))
```

### Let / Ref

`h/let` creates a model where some local models are defined.
`h/ref` refers to those local models.

- Any value can be used as a key.
- Inner local models are shadowing outer local models when using `h/ref`.

```clojure
;; Avoids repeating one-self
(h/let ['person (h/map [:name (h/fn string?)]
                       [:age (h/fn int?)])]
       (h/map [:me (h/ref 'person)]
              [:best-friend (h/ref 'person)]))

;; Allows definition of recursive data structures
(h/let ['person (h/map [:name (h/fn string?)]
                       [:friends (h/vector-of (h/ref 'person))])]
       (h/ref 'person))
```
