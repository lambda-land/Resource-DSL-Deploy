# Resource DSL

## Installation

Most of the installation is handled by [Haskell Stack][Stack], which will
automatically install the Haskell compiler (GHC), build system (Cabal), and
other Haskell dependencies. However, the DSL also depends on Microsoft's [Z3
Theorem Prover][Z3], which must be installed separately.


### Step-by-step instructions

1. **Install Z3.** This can be easily installed via the standard package
   manager of most Linux distributions or Homebrew on OS X. Otherwise, grab a
   binary or the source tarball from the [Z3 releases page][Z3] on GitHub.

2. **Install Stack.** Also available via standard package managers, or from the
   [Stack home page][Stack].

3. **Use Stack to complete installation.** Run the following commands in order:

   ```bash
   > stack setup     # downloads the package index and installs GHC
   > stack build     # installs dependencies and builds the project
   ```

## Running the DSL interpreter

The `stack build` command will produce a binary somewhere in Stack's working
directory. You can execute it with variations on the following command:

```bash
> stack exec resource-dsl
```

### Checking location providers

The interpreter has built-in support for checking location provider scenarios.
That is, given a DFU, an initial environment, and the mission requirements,
does the DFU work in the environment, and does the resulting environment
satisfy the mission requirements?

There are two interfaces for performing location provider checks.


**Option 1: Pass inputs as JSON files via the `inbox` directory.**

Usage:
```bash
> stack exec resource-dsl location inbox
```

JSON files placed in the `inbox` should be named:
  * `inbox/location-req.json`
  * `inbox/location-dfu.json`
  * `inbox/location-env.json`


**Option 2: Lookup inputs from names passed on the command line.**

Usage:
```bash
> stack exec resource-dsl location [req-name] [dfu-name] [env-name]
```

When specifying the names of each of the inputs, use the names of the files in
the directories `location/req`, `location/dfu`, and `location/env`, minus the
`.json` extension.

Some specific examples:

```bash
> stack exec resource-dsl location location gps-android GPS-Sat+GPS-DEV
> stack exec resource-dsl location saasm gps-saasm GPS-Sat+Ext-USB+Has-UI
> stack exec resource-dsl location location gps-usb GPS-Sat+Has-UI
```

The files in these directories (which are re-generated each time you run the
location provider check) are also example inputs that can be passed via the
`inbox` (Option 1), after renaming the files.


**Exit codes:** The location provider check generates some textual output.
However, the core response (OK, errors, unsatisfied) is indicated by the exit
code returned by the process.

The exit codes are defined as follows:
 
 * 0: OK -- the DFU loads successfully and satisfies the requirements
 * 1: Miscellaneous Error -- see output for details (e.g. bad arguments, JSON
      decoding error, bug in interpreter)
 * 2: DFU cannot be loaded in the given environment.
 * 3: DFU successfully loads but does not satisfy the requirements.


### Run the demo

A simple demo can be executed by simply executing the interpreter with no
arguments.

Currently, this invokes a simple demo that ensures everything is installed
correctly. Here is what should happen:
 * Generates two example input files:
   * `inbox/environment.json` -- describes the initial resource environment
   * `inbox/requirement.json` -- describes the required resource type
 * Reads these input files back in and prints out the corresponding Haskell
   values.
 * Invokes the SMT solver on a simple predicate and prints out a successful
   satisfiable result.
 * Writes an example output file:
   * `outbox/environment.json` -- describes the resulting resource environment


## Inputs and outputs
 
The structure of the JSON files is very preliminary. Currently, it is just an
automatic serialization of the corresponding Haskell values. We would like this
to be something nicer.

The semantic content of the inputs answers the questions:
 * What are the resources available in the initial resource environment?
 * What are the requirements on the final resource environment? (Note that the
   desired functionality of the application is described in terms of provided
   "resources", e.g. the ability to support a certain number of clients and
   handle a certain number of requests.)

The current output is the environment produced by executing the DSL program,
this can be tailored depending on the downstream needs.


[Stack]: http://docs.haskellstack.org/en/stable/README/
[Z3]: https://github.com/Z3Prover/z3/releases
