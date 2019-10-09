# Resource DSL -- Phase 3 Interface

## Running the DSL interpreter

The `stack build` command will produce a binary somewhere in Stack's working
directory. You can execute it with variations the following command:

```bash
> stack exec resource-dsl
```

This executable has multiple subcommands and several options. To see the list
of subcommands run:

```bash
> stack exec resource-dsl -- --help
```

Note that Stack will pass all arguments after the `--` symbol to the underlying
executable.

The relevant subcommand for Phase 3 is `swap-dau`. To see the options for a
subcommand, you can use `--help` applied to that subcommand, for example:

```bash
> stack exec resource-dsl -- swap-dau --help
```


## Running DAU swap scenarios

Using the `swap-dau` subcommand, a driver will:

 1. Translate a DAU swap request described by a set of JSON input files into a
    corresponding (set of) DSL program(s).
 
 2. Execute the program(s).
 
 3. Translate the output of the DSL program(s) into a DAU swap response encoded
    as a JSON output file.
 
 4. Compute various metrics about the search space during execution and output
    those to a JSON file.


## Options

Here is the output of `swap-dau --help` for reference:

```
Available options:
  -h,--help                Show this help text
  --run                    Run the search for replacement DAUs
  --z3-version             Print the z3 version number
  --max-daus INT           Max number of DAUs to include in response; 0 for no limit (default: 2)
  --rules-file FILE        Path to the JSON rules file (default: "inbox/swap-rules.json")
  --inventory-file FILE    Path to the JSON DAU inventory
                           file (default: "inbox/swap-inventory.json")
  --request-file FILE      Path to the JSON request file (default: "inbox/swap-request.json")
  --response-file FILE     Path to the JSON response file (default: "outbox/swap-response.json")
  --metrics-file FILE      Path to the JSON metrics file (default: "outbox/swap-metrics.json")
```

The option `--z3-version` can be used to make sure that Z3 has been installed
and linked correctly into the DSL interpreter.

There are three required input files:

 * A *rules file* that specifies generic equational and compatibility
   relationships among attributes and their values.
 
 * An *inventory file* that describes the inventory of DAUs available to use as
   replacements.
 
 * A *request file* that describes the DAUs that must be replaced.

Each of these input files can be passed to the driver by specifying their paths
using the corresponding options, or by putting them in a standard location
corresponding to the default value of each option.

There are two output files produced by a successful search:

 * The *response file* describes the configured DAUs from the inventory used to
   satisfy the replacement obligations in the request.
 
 * The *metrics file* containing various metrics about the search space
   explored when looking for potential replacements.

Similarly, the location to write the output files can be specified using the
corresponding options, or else they'll be written to a standard location
corresponding to the default value of each option.

The option `--run` is required to actually perform the search and generate a
response.


## Metrics

The DAU swap driver outputs several metrics to describe the search space
explored by the execution of DSL programs. The metrics are written to a simple
JSON file containing a set of attribute-value pairs.

Although the search is constructed such that we will always find the globally
optimal solution, it's difficult and expensive to compute the size of the
entire search space in advance. Therefore, these metrics instead reflect the
space we actually explored, and the space we considered but were able to
condense and/or prune away without exploring. 

Below is a list of attributes in the metrics file with descriptions of what
they're counting, along with some background and discussion to help interpret
these metrics.

 * *required-ports*: The total number of ports in DAUs flagged for replacement
   in the request.
 
 * *required-port-groups*: We group identical ports in a DAU together to
   condense the search space. This is the total number of unique port groups in
   DAUs flagged for replacement in the request. The difference between
   *required-ports* and *required-port-groups* represents a condensing of the
   search space.
 
 * *daus-in-inventory*: This is just the number of DAUs in the inventory.
 
 * *candidate-sub-inventories*: To support generating DSL programs that
   describe different parts of the search space, we break the inventory down
   into various sub-inventories that conceptually represent a set of potential
   replacement DAUs. This is the total number of candidate sub-inventories we
   may have to explore. This is computed from the size of the inventory and the
   `--max-daus` option.
 
 * *ignored-sub-inventories*: We perform some *very* basic filtering to make
   sure that sub-inventories provide relevant functionalities to the request.
   This metric is the number of inventories that these sanity checks ruled out.
   (Note: improving this filtering step is an easy optimization that could be
   significant for large inventories.)
 
 * *explored-sub-inventories*: The number of sub-inventories we actually
   explored by a variational analysis; that is, by generating a corresponding
   DSL program and executing it.
 
 * *explored-ports*: The total number of ports in across all sub-inventories
   that were explored.
 
 * *explored-port-groups*: The total number of port groups (recall that a group
   is just a set of identical ports within a single DAU) across all
   sub-inventories that were explored.
 
 * *total-config-dimensions*: A variational analysis of a particular
   sub-inventory corresponds to an exhaustive search of all possible
   configurations and port assignments of the DAUs within that inventory. This
   is supported (and made efficient) by capturing differences among
   configurations locally and maintaining these differences while sharing
   commonalities throughout the execution of the DSL program. The variability
   within a variational analysis is described by a number of binary dimensions
   of variation. This counts the total number dimensions used for tracking
   different ways of configuring the attributes of a (subset of a) port group
   within a DAU.
   
 * *total-match-dimensions*: This counts the total number of dimensions used
   for tracking different ways of matching up required ports from the request
   to provided ports from the inventory DAUs. For a particular sub-inventory,
   the search space after condensing port groups is `2^(c+m)` where `c` is
   *total-config-dimensions* and `m` is *total-match-dimensions*. This is the
   exponentially sized space that the variational analysis helps us explore
   quickly.
