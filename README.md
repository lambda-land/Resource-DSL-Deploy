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

## Run the DSL

The `stack build` command will produce a binary somewhere in Stack's working
directory. You can execute it with the following command:

```bash
> stack exec resource-dsl
```

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
 
The structure of the JSON files is very preliminary. Currently, it is just an
automatic serialization of the corresponding Haskell values. We would like this
to be something nicer.

The semantic content of the inputs answers the questions:
 * What are the resources available in the initial resource environment?
 * What are the requirements on the final resource environment? (Note that the
   desired functionality of the application is described in terms of provided
   "resources", e.g. the ability to support a certain number of clients and
   handle a certain number of requests.)

The current output is just the (manually written) result of executing the DSL
program, but can be tailored depending on the downstream needs.


[Stack]: http://docs.haskellstack.org/en/stable/README/
[Z3]: https://github.com/Z3Prover/z3/releases
