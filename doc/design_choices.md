## Faceless models

- decouple syntax and model's data
- no positionality unless required
- use hashmaps
- documented model format instead of documented model syntax

## Properties vs. structure

Decoupling between data properties and data's structure.
- `and` and `or` are only useful for data validation, not useful for data generation.
  They describe data properties well, but are a poor choice for describing their structure.
- Minimallist uses explicit models for describing structures.
  `:set-of :map-of :map :sequence-of :sequence`
- The sequence models (e.g. `:cat` and `:repeat`) have a dedicated property for specifying their containing collection.
- Optionally, the collection models all have a dedicated `:count-model` property for describing their size.
- All models have a `:condition-model` property which can be used to store properties that should be verified by the data.
- There is only one unique node `:alt` (a.k.a. "alternative") which is used for choices.
  The `:or` model should only be used for describing a data's properties.
