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
> stack exec resource-dsl -- run --help
```


## Running resource scenarios

Using the `run` subcommand, the executable can attempt to load an application
model into an initial resource environment and optionally check whether the
resulting resource environment satisfies a set of mission requirements.


### Inputs

All inputs to the `run` subcommand are passed via JSON. The schemas for these
inputs can be found in the `json` subdirectory of this project.

By default, inputs are passed via an `inbox` subdirectory in the current
working directory. However, the location of each file can be changed using
options to the `run` subcommand. 

The following input files are required:

 * `inbox/dictionary.json` -- dictionary of resource profiles for each DFU
 * `inbox/resources.json` -- initial resource environment
 * `inbox/model.json` -- application model to load

Additionally, an application model takes a list of arguments. These can be
specified either on the command line using the `--config` option, or by the
following input file.
 
 * `inbox/configuration.json` -- arguments to application model

A user can also provide a _selection_ to the `run` subcommand.
A selection sets certain features on or off prior to execution of the program.
The simplest way to perform a selection is via the `--on` or `--off` options.
The `--on` and `--off` options take as arguments a list of strings representing
the names of various features present in the program. For example, to run the
program with features `A` and `B` on and feature `C` off, you would use this command:

```bash
> stack exec resource-dsl -- run --on \[\"A\",\"B\"\] --off \[\"C\"\]
```

If a feature is left unselected, then it is evaluated as if it were simultaneously
on and off, producing a _variational_ value at the end that describes both alternatives.

Selections can also be specified via the `-f` or `--formula` option, which takes a
boolean formula that performs a selection. This allows for more expressiveness in
describing selections, as a user can describe any valid boolean formula using the
literals `true` and `false`, negation `!`, disjunction `||`, and conjunction `&&`.
For example, the following expresses the same selection as the example above:


```bash
> stack exec resource-dsl -- run -f "A&&B&&(!C)"
```

If a total configuration is desired, where all features are set to be either on
or off with no variational evaluation performed, the user can use the `-t` or
`--total` option, which takes a list of the strings representing the features
that should be turned on (just like the `--on` option). The difference from the
`--on` option is that all other features left unspecified are assumed to be set
to off. For example, this command turns `A` and `B` on and all other features off:

```bash
> stack exec resource-dsl -- run -t \[\"A\",\"B\"\]
```


Finally, the resulting resource environment can be checked against a set of
mission requirements, which is itself just another resource profile, which is
provided in the following input file.
 
 * `inbox/requirements.json` -- mission requirements

Alternatively, you can pass the `--no-reqs` option to the `run` subcommand to
generate the output resource environment without checking it against any
mission requirements.


### Outputs

When the `run` subcommand is run, there are a number of possible outcomes.
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

## Checking a configuration

As we saw above, we can specify specific features that can be turned on and off
for a given model. We call a full group of these features a _configuration_. If
a feature is set to be neither on or off when a call to `run` is made, it is run
variationally, collecting results for both alternatives. This then produces a
variational result, where some configurations pass the mission requirements while
others may fail. In order to extract and query these variational results, our
application provides the `check` subcommand.

When run, the `check` subcommand will exit with exit code 0 if there is a least
one successful configuration based off of the last call to `run`; otherwise, if there
are no successful configurations, it will exit with code 1. If there are successful
configurations they will be output to the output file, which defaults to `outbox/best.txt`.
For example, this command will simply check the output of the last call to `run` for
whether there were any succesful configurations:

```bash
> stack exec resource-dsl -- check
```

The `check` command needs to know where it can find the `success.json` file of a previous
call to run. By default it simply looks in `outbox/success.json` but an alternate filepath
can be provided via the `--success-file` option. The output file defaults to `outbox/best.txt`
but this can be modified via the `--best-file` option.

By default the `check` command outputs a maximum of 25 sample successful configurations in its
output file. To change the maximum number of sample configurations use the `-m` or `--maxresults`
option. For example, the following command reads its input from the file `old-success.json` and
outputs a maximum of 10 results to `output.txt`:

```bash
> stack exec resource-dsl -- check -m 10 --success-file old-success.json --best-file output.txt
```

Finally, we can constrain the configurations we check. For example, say we have some features
`A` and `B` and we would like to know if there are any successful configurations with `A` turned on
and `B` turned off.
We can check this using the same system of inputs described above for preconfiguring calls
to `run`. So for our example, we would call:

```bash
> stack exec resource-dsl -- check --on \[\"A\"\] --off \[\"B\"\]
```

This would then output only configurations that were successful and included `A` on and `B` off.

Similar to above, we can also pass boolean formulas describing configurations via the
`-f` or `--formula` option.

If we only wish to check a single configuration we can use the `-t` or `--total` option,
which takes a list of features that should be turned on and sets all other features to
off.


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
> stack exec resource-dsl -- run --config [\"gps-bluetooth\"]
```

Alternatively, we can load the `gps-usb` DFU, which loads successfully but
fails the mission requirements check (exit code 3) since the SAASM feature is
not supported:

```bash
> stack exec resource-dsl -- run --config [\"gps-usb\"]
```

Finally, we can load the `gps-saasm` DFU, which loads successfully and passes
the mission requirements (exit code 0):

```bash
> stack exec resource-dsl -- run --config [\"gps-saasm\"]
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

We can run this configuration by executing the following command:

```bash
> stack exec resource-dsl -- run
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
> stack exec resource-dsl -- run
```

## Cross Application Dependencies Example

To see the inputs that can be generated for this example, pass `--help` to the
`crossapp` example subcommand:

```bash
> stack exec resource-dsl -- example crossapp --help
```

First, generate some input files. The following command generates the cross app
example dictionary, empty initial resource environment, and an empty set of requirements:

```bash
> stack exec resource-dsl -- example crossapp --dict --model --reqs
```

Next, we generate the initial resource environment. For the initial resource
environment, the user can choose to configure the presence of both strong
encryption policy files and AESNI support for both the server and the client.
The user passes a JSON object as an argument to the `init` option with fields
`serverAESNI`, `serverSEP`, `clientAESNI`, and `clientSEP`. The values for
these fields should be strings representing _variational values_ for these
initial resources.

At their most basic, the variational values used as the values for these fields
can be either `()` (the unit value, representing the presence of the resource)
or `none` (the absence of the resource). For example, to create an initial
resource environment with all possible resources present, a user would input
the following:

```bash
> stack exec resource-dsl -- example crossapp --init '{ "serverAESNI": "()", "serverSEP": "()", "clientAESNI": "()", "clientSEP": "()" }'
```

Similarly, to create an empty initial resource environment, we would call:

```bash
> stack exec resource-dsl -- example crossapp --init '{ "serverAESNI": "none", "serverSEP": "none", "clientAESNI": "none", "clientSEP": "none" }'
```

Variational values also allow a user to specify different values under the
presence of certain features, including representing initial environments where
a resource is both present and absent. To do this, we create a choice value,
which has the form `[<dimension>]{<variational value>, <variational value>}`.
For example, to represent both the presence and absence of the `serverAESNI`
resource, we could include the key/value pair
`"serverAESNI": "[sAESNI]{(),none}"`. This signals that when the feature `sAESNI`
is enabled, then the `serverAESNI` resource is available, but otherwise it
is unavailable.

Next, we generate a configuration for the example. To do this, we choose an
encryption provider to run on the server, and another provider to run on the
client. We pass these arguments via a JSON object with fields `serverProv` and
`clientProv` set to the name of the provider DFU to load on the server and
client, respectively. For example, to use the default `javax` API on the server
and `org.bouncycastle` on the client, you would call:

```bash
> stack exec resource-dsl -- example crossapp --config '{ "serverProv": "Javax", "clientProv": "BouncyCastle" }'
```

The available DFUs to load on the server and client are `Javax` and
`BouncyCastle`.

There are several options for running the example program. If we wish to query
multiple different configurations at once, or to have the application enumerate
possible correct configurations for us, we simply use the command:

```bash
> stack exec resource-dsl -- run
```

This will run all possible configurations variationally. As such, it will take
a minute or two to complete. Once completed, the user can use the `check`
subcommand to query the variational results. For example, say we wish to know
whether the AES128 in CTR mode with PKCS5 padding is supported. We would call:

```bash
> stack exec resource-dsl -- check --total '["AES","CTR","KSZ16","PKCS5Padding"]'
```

Similarly, if we wish to know if there are any successful configurations that use Blowfish and a keysize
of 56 bits, we can use this command:

```bash
> stack exec resource-dsl -- check --on '["Blowfish","KSZ8"]'
```

By examining `outbox/best.txt` we can then obtain some possible successful
configurations, if any exist.

The other possible way to run the example program is by eliminating certain
options by preconfiguring the program prior to execution. For example, if we
know we are only interested in AES128 in CTR mode with PKCS5 padding we can
preconfigure the program and avoid useless computation with this call:

```bash
> stack exec resource-dsl -- run --total '["AES","CTR","KSZ16","PKCS5Padding"]'
```

Similarly, we can turn off or on only certain features and then query the
results using the `check` subcommmand as described above. For example, this
preconfiguration turns DES on and CTR mode off:

```bash
> stack exec resource-dsl -- run --on '["DES"]' --off '["CTR"]'
```

In general if you know that you are only interested in certain features, or are
uninterested in others, it is better to preconfigure the program in order to
save computation time.

The following features are supported and can be queried and turned on/off:

Algorithms: `["AES", "ARIA", "Blowfish", "Camellia", "CAST5", "CAST6", "DES", "DESede", "DSTU7624", "GCM", "GOST28147", "IDEA", "Noekeon", "RC2", "RC5", "RC5_64", "RC6", "Rijndael", "SEED", "SEEDWrap", "Serpent_128", "Skipjack", "SM4", "TEA", "Threefish_256", "Threefish_512", "Threefish_1024", "Twofish", "XTEA"]`

Modes: `["ECB", "CBC", "CTR", "CFB", "CTS", "OFB", "OpenPGPCFB", "PGPCFBBlock", "SICBlock"]`

Paddings: `["ZeroBytePadding", "PKCS5Padding", "PKCS7Padding", "ISO10126_2Padding", "ISO7816_4Padding", "TBCPadding", "X923Padding", "NoPadding"]`

Keysizes (in bytes): `["KSZ8", "KSZ16", "KSZ24", "KSZ32", "KSZ40", "KSZ48", "KSZ56", "KSZ64"]`

The application will also output several report files into the outbox.
`error.json` will contain a variational data structure containing any errors
generated for a particular variant, such as failing to meet a particular
mission requirement. `resources.json` will include the variational resource
environment generated by running the example. `success.json` includes a boolean
formula that denotes which variants, if any, did not encounter any errors and
met all mission requirements. If the `check` subcommand was used, `best.txt`
will contain some possible successful configurations for the given `check`, if
any exist.


### Peter's Cross-App Scenario

This is a shorter, specific set of instructions to support the sequence of
steps that Peter outlined in his email.

  1. Generate the DFU dictionary, application model, and mission requirements.
     
     ```
     > stack exec resource-dsl -- example crossapp --dict --model --reqs
     ```
  
  2. Initialize the application model by indicating that you want to search all
     cipher DFUs.
     
     ```
     > stack exec resource-dsl -- example crossapp --config-all
     ```
  
  3. Initialize the resource environment to the desired configuration, as
     described above. For example, to indicate AES-NI support on the server
     only, and strong-encryption support on neither:
     
     ```
     > stack exec resource-dsl -- example crossapp --init '{ "serverAESNI": "()", "serverSEP": "none", "clientAESNI": "none", "clientSEP": "none" }'
     ```

     Alternatively, to explore all possible configurations of the resource
     environment, you can use the `--init-all` option:

     ```
     > stack exec resource-dsl -- example crossapp --init-all
     ```

  4. Now you can run the analysis. This will take a few minutes (takes ~7
     minutes on a 2018 Thinkpad X1).

     ```
     > stack exec resource-dsl -- run
     ```
  
     Alternatively, if you know some of the configuration parameters already
     (e.g. the required key size), you can run the analysis more quickly by
     specifying the known options in the `run` command. The more options you
     specify, the faster the analysis will run. (Specifying the keysize alone
     shaves a minute or two off the analysis.)

     ```
     > stack exec resource-dsl -- run --on '["KSZ16"]'
     ```
  
  5. After running the analysis once, the success of various configurations can
     be queried quickly and as many times as needed. For example, to simply
     check whether any configuration is valid:

     ```
     > stack exec resource-dsl -- check
     ```

     This should return "Success" if there is a valid configuration. A list of
     several valid configurations satisfying the `check` call can be found in
     `outbox/best.txt`. This indicates a list of all of the configuration
     options and their settings.
     
     Alternatively, to check whether there is a valid configuration that
     satisfies some additional constraints, you can specify these by running
     the `check` command with settings for various configuration options.
     
     For example, to check whether there is a valid configuration that uses
     a key size of 16 bytes and the AES algorithm, run:

     ```
     > stack exec resource-dsl -- check --on '["KSZ16","AES"]'
     ```

     This will generate a new `outbox/best.txt` with configurations that
     satisfy this check.

[Stack]: http://docs.haskellstack.org/en/stable/README/
[Z3]: https://github.com/Z3Prover/z3/releases
