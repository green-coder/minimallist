## Node hierarchy

In Minimallist, models are a node hierarchy where each node is a hashmap.

Each node have a `:type` attribute to indicate what they represent.
The node types currently supported are:

```clojure
#{:fn :enum
  :and :or
  :set-of :map-of :map :sequence-of :sequence
  :alt :cat :repeat
  :let :ref}
```

## Model of the models

A data model is currently in progress to represent the data models.
It is written using Minimallist and can be found in the source code, in the
namespace [`minimallist.minimap`](../src/minimallist/minimap.cljc).

You can use Minimallist and this model to verify that your models are
valid:

```clojure
(require '[minimallist.core :as m])
(require '[minimallist.minimap :as mm])

(m/valid? mm/minimap-model my-model)
```

## What a model looks like

Example:

```clojure
{:type :map,
 :entries [{:key :name
            :model {:type :fn, :fn string?}}
           {:key :age
            :model {:type :fn, :fn int?}}
           {:key :gender
            :optional true
            :model {:type :enum, :values #{:female :male :other}}}]}
```

Models can be pretty verbose to write.
Minimallist provides a small set of helper functions to express them
in a more concise way (see the section [Model Builder](model_builder.md)).

## Local definitions

A model can contain local definitions using a `:let` node, which can be referenced
from an inner model specified in its `:body` attribute.

Before using the `:let` node:

```clojure
{:type :map,
 :entries [{:key :me,
            :model {:type :map,
                    :entries [{:key :name,
                               :model {:type :fn, :fn string?}}
                              {:key :age,
                               :model {:type :fn, :fn int?}}]}}
           {:key :best-friend,
            :model {:type :map,
                    :entries [{:key :name,
                               :model {:type :fn, :fn string?}}
                              {:key :age,
                               :model {:type :fn, :fn int?}}]}}]}
```

After using the `:let` node:

```clojure
{:type :let,
 :bindings {'person {:type :map,
                     :entries [{:key :name,
                                :model {:type :fn, :fn string?}}
                               {:key :age,
                                :model {:type :fn, :fn int?}}]}},
 :body {:type :map,
        :entries [{:key :me,
                   :model {:type :ref, :key 'person}}
                  {:key :best-friend,
                   :model {:type :ref, :key 'person}}]}}
```

Local definition are also the key to unlock recursive data models.

Example:

```clojure
{:type :let,
 :bindings {'person {:type :map,
                    :entries [{:key :name,
                               :model {:type :fn, :fn string?}}
                              {:key :friends,
                               :model {:type :sequence-of,
                                       :elements-model {:type :ref, :key 'person},
                                       :coll-type :vector}}]}},
 :body {:type :ref, :key 'person}}
```
