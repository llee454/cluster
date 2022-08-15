README
======

The cluster module defines functions that allow you to cluster phrases, names,
and titles by similarity or equivalence. It's functions can either accept a set
of cluster categories and then group a set of names, phrases, or titles, into
those categories, or it can accept a set of names, phrases, and titles, and
partition them into a set of inferred clusters. This library is useful whenever
you have data that you want to categorize.


Initializing the Build Environment
----------------------------------

Note: the following are a hack solution to fix the fact that brew and opam are
out of sync and opam's owl-plplot library requires library versions that can no
longer be installed with brew.

```
opam switch create . ocaml-variants.4.10.0+flambda --no-install
opam update
opam install --deps-only .
dune build
dune exec analysis/main.exe
```

Usage in Other Projects
-----------------------

To use this PPX library in another project include the `cluster` library in your Dune file's libraries section. For example:

```
(executable
 (name main)
 (libraries
  core_kernel
  cluster
  )
 (preprocess
  (pps ppx_jane))
  (modes exe)
)
```

You must also update your OPAM package configuration file. Add the following line to your "pin-depends" section: `["cluster.1.0.0" "git+https://github.com/llee454/cluster.git#main"]`. Add the following to your "depends" section: `  "cluster" { = "1.0.0"}`.


Example
-------

Imagine that you have a set of conceptual categories distinguished by the following keywords:

```ocaml
let clusters = [
  {
    label = "mental health issue";
    word_set = String.Set.of_list ["anxiety"; "depression"; "ptsd"; "adhd"; "attention"; "deficit"];
    entries = []
  };
  {
    label = "physical health issue";
    word_set = String.Set.of_list ["nutrition"; "dental"; "diabetes"];
    entries = []
  }
]
```

and you have a set of diagnoses such as:

```ocaml
let diagnoses = [
  "patient with anxiety disorder and panic attacks";
  "patient with dental carries and plaque";
  "attention deficit disorder with behavioral misconduct"
]
```

and you want to group the diagnoses into the two categories: "mental health issues," and "physical health issues." Then you can use the `cluseter` function provided by this module to do so:

```ocaml
module Diagnoses_clusters = Cluster.Make ((
  struct
    type t = string; [@@deriving sexp, compare]
    let get_name = Fn.id
  end : Cluster.Make_arg with type t = string
))

let result = Diagnoses_clusters.cluster ~k_shared=0.5 ~k_not_shared=0.4 ~ignore:String.set.empty ~clusters diagnoses
```

Note: the cluster algorithm accepts two arguments: `k_shared` and `k_not_shared`. `k_shared` specifies how heavily the algorithm will weight words that appear in both the cluster's word set and the phrase being analyzed, while `k_not_shared` specifies how heavily the algorithm will weight words that are "missing." In general `k_shared` should be greater than `k_not_shared`.