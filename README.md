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

A user can also provide a _selection_ to the `check` subcommand.
A selection sets certain features on or off prior to execution of the program.
The simplest way to perform a selection is via the `--on` or `--off` options.
The `--on` and `--off` options take as arguments a list of strings representing
the names of various features present in the program. For example, to run the
program with features `A` and `B` on and feature `C` off, you would use this command:

```bash
> stack exec resource-dsl -- check --on \["A","B"\] --off \["C"\]
```

If a feature is left unselected, then it is evaluated as if it were simultaneously
on and off, producing a _variational_ value at the end that describes both alternatives.

Selections can also be specified via the `-f` or `--formula` option, which takes a
boolean formula that performs a selection. This allows for more expressiveness in
describing selections, as a user can describe any valid boolean formula using the
literals `true` and `false`, negation `!`, disjunction `||`, and conjunction `&&`.
For example, the following expresses the same selection as the example above:


```bash
> stack exec resource-dsl -- check -f "A&&B&&(!C)"
```

If a total configuration is desired, where all features are set to be either on
or off with no variational evaluation performed, the user can use the `-t` or
`--total` option, which takes a list of the strings representing the features
that should be turned on (just like the `--on` option). The difference from the
`--on` option is that all other features left unspecified are assumed to be set
to off. For example, this command turns `A` and `B` on and all other features off:

```bash
> stack exec resource-dsl -- check -t \["A","B"\]
```


Finally, the resulting resource environment can be checked against a set of
mission requirements, which is itself just another resource profile, which is
provided in the following input file.
 
 * `inbox/requirements.json` -- mission requirements

Alternatively, you can pass the `--no-reqs` option to the `check` subcommand to
generate the output resource environment without checking it against any
mission requirements.


### Outputs

When the `check` subcommand is run, there are a number of possible outcomes.
In the case that there is an error in loading the input, such as a missing
file or an error parsing the JSON input, execution terminates with an exit
code of 1.

Once the input is successfully loaded, the model is loaded into the given
environment. If all variants produce an error, execution terminates with an
exit code of 2. Otherwise, if at least one variant successfully loads, execution
proceeds to the next step.

If provided with requirements, the resulting resource environment is then checked
for whether those requirements are satisfied. If all variants fail to satisfy the
given requirements, the application exits with exit code 3.

If at least one variant is successful, the application exits with exit code 0.

There are three possible output files produced when the application is run:

  * `outbox/error.json` -- A variational value containing either the error
  message for a particular variant, or `null` if no error exists.
  * `outbox/success.json` -- A boolean expression that indicating which variants,
  if any, are in success states.
  * `outbox/resources.json` -- The resulting resource environment.
  
Only the first two files are produced in the case that the application model
fails to load, exiting with code 2.

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


## Location Provider Example

Here is an example sequence of commands for generating example inputs for the
location provider example and then checking a handful of scenarios.

First, generate the required inputs. The following command generates the
location provider dictionary, application model, the extended SAASM mission
requirements, and an initial resource environment containing the resources
GPS.SAT and Ext.USB:

```bash
> stack exec resource-dsl -- example location --dict --model --saasm --init GPS.SAT+Ext.USB
```

Next, we want to check loading the application model with various input
configurations. The application model takes as input the ID of a DFU to load.
The following DFUs are defined in the default dictionary:

 * `gps-android`
 * `gps-bluetooth`
 * `gps-usb`
 * `gps-saasm`
 * `dead-reckoning`

For example, we can try to load the `gps-bluetooth` DFU into the resource
environment we generated above, which fails on loading the application model
(exit code 2) since the required `Ext.BT` resource is missing:

```bash
> stack exec resource-dsl -- check --config [\"gps-bluetooth\"]
```

Alternatively, we can load the `gps-usb` DFU, which loads successfully but
fails the mission requirements check (exit code 3) since the SAASM feature is
not supported:

```bash
> stack exec resource-dsl -- check --config [\"gps-usb\"]
```

Finally, we can load the `gps-saasm` DFU, which loads successfully and passes
the mission requirements (exit code 0):

```bash
> stack exec resource-dsl -- check --config [\"gps-saasm\"]
```


## Network Example

To see the inputs that can be generated for this example, pass `--help` to the
`network` example subcommand:

```bash
> stack exec resource-dsl -- example network --help
```

First, generate some input files. The following command generates the network
example dictionary, application model, the mission requirements, an initial
resource environment with 5000 kb/s of bandwidth, and configures the
application to consist of 2 clients, each sending 30 PLI reports and 5 images
per minute, where images are scaled by a factor of 1.0 (i.e. no scaling).

```bash
> stack exec resource-dsl -- example network --dict --model --reqs --init 5000 --config \(2,30,5,1.0\)
```

We can check this configuration by executing the following command:

```bash
> stack exec resource-dsl -- check
```

However, this fails when checking the mission requirements (exit code 3) since
the bandwidth is insufficient for the configured number of clients, PLI report
rate, and image report rate.

The following command will reconfigure to only send 3 images per minute, scaled
by a factor of 0.4 (i.e. 40% of the original size).

```bash
> stack exec resource-dsl -- example network --config \(2,30,3,0.4\)
```

This configuration passes the mission requirements (exit code 0):

```bash
> stack exec resource-dsl -- check
```

## Cross Application Dependencies Example

To see the inputs that can be generated for this example, pass `--help` to the
`crossapp` example subcommand:

```bash
> stack exec resource-dsl -- example crossapp --help
```

First, generate some input files. The following command generates the cross app
example dictionary and application model.

```bash
> stack exec resource-dsl -- example crossapp --dict --model
```

Next, we generate the initial resource environment. The initial resource environment is specified
by setting boolean values for whether the server or client support either AESNI or the JCE Unlimited
Strength provisions. For example, to set an initial resource environement where the server supports both
but the client does not, we would run the following command:

```bash
> stack exec resource-dsl -- example crossapp --init \{serverAESNI=True,clientAESNI=False,serverJCEUS=True,clientJCEUS=False\}
```

Next, we generate a configuration for the example. To do this, we choose an encryption provider
to run on the server, another provider to run on the client, and a keysize to use with the
encryption algorithm. For example, to use the default `javax` API on the server and `org.bouncycastle`
on the client, with 128-bit encryption, you would call:

```bash
> stack exec resource-dsl -- example crossapp --config \{serverProv=\"Javax\",clientProv=\"BouncyCastle\",keysize=128\}
```

The available DFUs to load on the server and client are `Javax`, `BouncyCastle`, and `AESNI`.

Next, generate the mission requirements. The requirements are specified as a 4-tuple made up
of an algorithm, keysize, mode, and padding. The requirements check that the specified 4-tuple
is available from the providers on both the server and the client. For example, the following
command generates mission requirements that specify that both client and server support
AES128 encryption in CBC mode with PKCS5 padding:

```bash
> stack exec resource-dsl -- example crossapp --reqs \{algorithm=\"AES\",keysize=128,mode=\"CBC\",padding=\"PKCS5\"\}
```

Once the mission requirements are generated, check the example configuration against the requirements
by running this command:

```bash
> stack exec resource-dsl -- check
```

If there is some variant that meets the mission requirements, it will return with exit code 0. Otherwise,
it will return with exit code 2 if some static condition was not met (e.g. the keysize didn't match the
algorithm) or exit code 3 if the particular 4-tuple specified in the mission requirements is not supported
on both the client and server.

The application will also output three report files into the outbox. `error.json` will contain a variational
data structure containing any errors generated for a particular variant, such as failing to meet a particular
mission requirement. `resources.json` will include the variational resource environment generated by running
the example. Finally, `success.json` includes a boolean formula that denotes which variants, if any, did not
encounter any errors and met all mission requirements.

[Stack]: http://docs.haskellstack.org/en/stable/README/
[Z3]: https://github.com/Z3Prover/z3/releases
