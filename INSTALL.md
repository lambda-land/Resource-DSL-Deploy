# Installation

Most of the installation is handled by [Haskell Stack][Stack], which will
automatically install the Haskell compiler (GHC), build system (Cabal), and
other Haskell dependencies. However, the DSL also depends on Microsoft's [Z3
Theorem Prover][Z3] and its corresponding header files, which must be installed
separately.


## Step-by-step instructions

1. **Install Z3 version 4.8.5.** Because we rely on low-level bindings to the
   Z3 solver, we're unfortunately fairly sensitive to the particular version of
   Z3 installed. Other versions may work but are untested. This version of Z3
   can be installed via the package manager of more bleeding-edge Linux
   distributions (e.g. Fedora, Arch) or Homebrew on OS X. For distributions
   with stale package repositories (e.g. Ubuntu), you'll have to grab a binary
   or the source tarball from the [Z3 releases page][Z3] on GitHub.

2. **Install Z3 header files.** If you installed Z3 via your package manager,
   you should also install the corresponding `-dev` or `-devel` package. If you
   installed from source, you can copy the header files from the source
   distribution to a new directory at `../z3-headers/`, and the build system
   should find them.

2. **Install Stack.** Also available via standard package managers, or from the
   [Stack home page][Stack].

3. **Use Stack to complete installation.** Run the following commands in order:

   ```bash
   > stack setup     # downloads the package index and installs GHC
   > stack build     # installs dependencies and builds the project
   ```

[Stack]: http://docs.haskellstack.org/en/stable/README/
[Z3]: https://github.com/Z3Prover/z3/releases
