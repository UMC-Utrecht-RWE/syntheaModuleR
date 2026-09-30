# syntheaModuleR

<!-- badges: start -->
[![GitHub](https://img.shields.io/badge/GitHub-UMC--Utrecht--RWE%2FsyntheaModuleR-181717?logo=github)](https://github.com/UMC-Utrecht-RWE/syntheaModuleR)
<!-- badges: end -->

`syntheaModuleR` builds valid [Synthea](https://github.com/synthetichealth/synthea) Generic
Module Framework (GMF) module JSON files from R, instead of hand-writing the JSON.

Synthea is an open-source synthetic patient population simulator (Java, MITRE-led). Most of its
clinical logic is authored as JSON "module" files - one per disease or care pathway - interpreted
by Synthea's own rule engine. This package's entire job is to make it possible to author one of
those module files from R, with the field names and structural rules checked as you go.

**This package does not run Synthea.** It only produces module JSON files. To generate patients
with one, drop the file into a real Synthea checkout's `src/main/resources/modules/` (or pass it
via `-d <dir>`) and run it through `./run_synthea`.

Every field name and constraint the package knows about was extracted directly from Synthea's
Java source (`org.mitre.synthea.engine`), not guessed from example module files.

## Installation

```r
# install.packages("devtools")
devtools::install_github("UMC-Utrecht-RWE/syntheaModuleR")
```

## Two ways to build a module

### Engine layer - one builder per grammar layer

The Engine layer mirrors Synthea's Java classes 1:1: a builder for Components, Logic,
Transitions, and States, assembled into a Module. The smallest possible valid module is just the
two states every module needs - `Initial` and one `Terminal` - wired directly together:

```r
library(syntheaModuleR)

minimal_module <- build_module(
  name = "Minimal Example",
  states = list(
    Initial = create_state_settings("Initial",
      transition = create_transition_settings("direct", to = "Terminal")
    ),
    Terminal = create_state_settings("Terminal")
  )
)
cat(minimal_module)
```

`build_module()` assembles the states you hand it, runs `validate_module()` against Synthea's
load-time invariants (an `Initial` state, at least one `Terminal` state, every state-name
reference resolves), and returns pretty-printed JSON by default. `write_module_json()` writes
that result to disk.

### Frontend layer - fragments and combinators

Composing a module state-by-state gets verbose past a handful of states. The Frontend layer
(leaf constructors like `create_condition()`, `create_medication()`, `create_observation()`, plus
combinators like `chain()`, `pathways()`, `classify()`, `repeat_until()`) lets you compose common
shapes - onset, encounter, treatment pathway, discontinuation - without hand-naming every
transition target, then finish with `build_cohort_module()`:

```r
library(syntheaModuleR)

diabetes <- create_component_settings("code",
  system = "SNOMED-CT", code = "44054006",
  display = "Diabetes mellitus type 2 (disorder)"
)

build_cohort_module("Diabetes Cohort", create_condition("Diabetes", diabetes))
```

`build_disease_module()` is a third option: a ready-made recipe for the common
onset -> diagnosis-encounter -> optional-resolution/death -> `Terminal` shape, built entirely on
top of the Engine layer.

## Learn more

`vignette("index", package = "syntheaModuleR")` is the recommended starting point, and links out
to the full set of vignettes in reading order:

- **layers** - the five layers (Components, Logic, Transitions, States, Modules), states-first
- **state-reference** - all 31 state types
- **set-attribute-precedence** - `SetAttribute`'s value-source precedence order
- **component-reference** - all 8 component value shapes
- **distribution** - the `Distribution` component (`EXACT`/`UNIFORM`/`GAUSSIAN`/`EXPONENTIAL`/`TRIANGULAR`)
- **transition-reference** - all 6 transition kinds
- **logic-reference** - all 21 logic predicate types
- **logic-and-guards** - `And`/`Or`/`Not`/`AtLeast`/`AtMost` and the `Guard` state
- **recipe-vs-compose** - `build_disease_module()` vs. hand-composed `build_module()`
- **cohort-building** - a worked, incrementally-built cohort module
- **frontend-layer** - the same shape composed from `blocks.R`/`combinators.R` fragments

## Package layout

```
R/
  components.R   Component value shapes: create_component_settings()
  logic.R        Logic predicates: create_logic_settings()
  transitions.R  Transitions: create_transition_settings()
  states.R       States: create_state_settings()
  module.R       build_module(), validate_module(), write_module_json(),
                 build_disease_module(), build_cohort_module()
  blocks.R       Frontend-layer leaf constructors (create_condition(), create_medication(), ...)
  combinators.R  Frontend-layer combinators (chain()/add(), pathways(), classify(), repeat_until())
vignettes/       Full reference and worked examples (see above)
tests/           testthat unit tests
```

## Status

This project is in alpha mode.
`validate_module()` re-implements the engine's load-time invariants as best effort, but the only
real ground truth is Synthea itself - before relying on a generated module, load it into a real
Synthea checkout and run it.
