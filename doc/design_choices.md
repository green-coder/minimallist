
## Making syntax someone else's choice

Minimallist was designed in a minimalist way. Its implementation is using hashmaps to
represent the models instead of other alternative like hiccup.
The goal was to avoid having any kind of positional parsing in the implementation.

The user can still use Hiccup for representing models and can convert them to hashmaps
prior to using Minimallist's functions.

Using hashmaps in the library avoids having to worry about making syntax choices that
may break later.

## Decoupling data structure and data properties in models

Clojure Spec was used as reference at the early stages of Minimallist's design.
Using `s/and` and `s/or` for validating data was convenient and simple, but 
was problematic for generating data:
In some cases, it was mixing together the structure of the data and the logical properties
of those data, making it difficult for the library to know which one is which.

In Minimallist, data structure and data properties have been separated, so that
when we want to generate data, the *data structure* is generated while the
*data properties* validate them.

## Or vs. Alt

Choice is a problem. Having the choice on how to describe a choice is also a problem.

In Minimallist, there is only one unique node `:alt` (for "alternative") which is used for choices.
The same `:alt` node is used in both sequence and non-sequence modes.

The `:or` node should only be used for describing the relations between data properties.
If you use it in the same way that it works in Clojure Spec, it will work fine for validating
data, but won't work well for generating data.

Similarly, `:and` should be used only on relations between data properties.
Minimallist was designed to minimize having to use them to represent data structure.
See the examples for more information.

