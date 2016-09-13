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

To see the options for a particular subcommand, you can use `--help` on that
subcommand, for example:

```bash
> stack exec resource-dsl -- check --help
```


## Checking resource scenarios

Using the `check` subcommand, the executable can attempt to load an application
model into an initial resource environment and optionally check whether the
resulting resource environment satisfies a set of mission requirements.


### Inputs

All inputs to the `check` subcommand are passed via JSON. The schemas for these
inputs can be found in the `json` subdirectory of this project.

By default, inputs are passed via an `inbox` subdirectory in the current
working directory. However, the location of each file can be changed using
options to the `check` subcommand. 

The following input files are required:

 * `inbox/dictionary.json` -- dictionary of resource profiles for each DFU
 * `inbox/resources.json` -- initial resource environment
 * `inbox/model.json` -- application model to load

Additionally, an application model takes a list of arguments. These can be
specified either on the command line using the `--config` option, or by the
following input file.
 
 * `inbox/configuration.json` -- arguments to application model

Finally, the resulting resource environment can be checked against a set of
mission requirements, which is itself just another resource profile, which is
provided in the following input file.
 
 * `inbox/requirements.json` -- mission requirements


### Outputs

There are two kinds of outputs from the `check` subcommand. The first is the
resulting resource environment if the application model successfully loads,
stored by default in the following file:

 * `outbox/resources.json` -- resulting resource environment

Additionally the overall result of the check is indicated by the process exit
code (and potentially some error messages).

The exit codes are defined as follows:
 
 * 0: OK -- the model loads successfully and satisfies the mission requirements
 * 1: miscellaneous error -- see output for details (e.g. JSON error, bug)
 * 2: model cannot be loaded in the given environment
 * 3: model successfully loads but does not satisfy the requirements


## Generating example inputs

Using the `example` subcommand and its subcommands, the executable can generate
example inputs and put them in the inbox.

To see the list of example subcommands, run:

```bash
> stack exec resource-dsl -- example --help
```

To see the inputs that can be generated for a particular example, pass `--help`
to the corresponding example subcommand, for example:

```bash
> stack exec resource-dsl -- example location --help
```


## Example usage

Here is an example sequence of commands for generating example inputs for the
location provider example and then checking a handful of scenarios.

First, generate the required inputs. The following command generates the
location provider dictionary, application model, the extended SAASM mission
requirements, and an initial resource environment containing the resources
GPS.SAT and Ext.USB:

```bash
> stack exec resource-dsl -- example location --dict --model --reqs --saasm GPS.SAT+Ext.USB
```

Next, we want to check loading the application model with various input
configurations. The application model takes an integer input that is used to
select between the various location provider DFUs:

 * 1: load `gps-android`
 * 2: load `gps-bluetooth`
 * 3: load `gps-usb`
 * 4: load `gps-saasm`
 * otherwise: load `dead-reckoning`

For example, we can try to load the `gps-bluetooth` DFU into the resource
environment we generated above, which fails on loading the application model
(exit code 2) since the required `Ext.BT` resource is missing:

```bash
> stack exec resource-dsl -- check --config [2]
```

Alternatively, we can load the `gps-usb` DFU, which loads successfully but
fails the mission requirements check (exit code 3) since the SAASM feature is
not supported:

```bash
> stack exec resource-dsl -- check --config [3]
```

Finally, we can load the `gps-saasm` DFU, which loads successfully and passes
the mission requirements (exit code 0):

```bash
> stack exec resource-dsl -- check --config [4]
```


[Stack]: http://docs.haskellstack.org/en/stable/README/
[Z3]: https://github.com/Z3Prover/z3/releases
