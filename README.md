# Brane Script

Embeddable scripting system in development with and intended for use in [Brane Engine](https://github.com/WireWhiz/BraneEngine).

Read more about why we're using a custom scripting system instead of a pre-existing one [here](https://branereality.github.io/BraneScript/docs/spec/goals).

## Setup

You will need to install the tree-sitter cli, this can be done with npm or cargo.
As tree-sitter also has a dependency on [nodejs](https://nodejs.org/en/download/package-manager), 
it usually makes sense to use npm.

```bash
npm install tree-sitter-cil
or
cargo install tree-sitter-cil
```

We recommend using vcpkg for package dependencies, but you may use any package manager that you can set a `CMAKE_TOOLCHAIN_FILE` with, 
and if you're on linux you might get away with just using apt. When using other package managers refer to vcpkg.json 
for what packages to install.

Run vcpkg to install packages locally
```bash
vcpkg install
```

## Building

if on windows, add `-DCMAKE_TOOLCHAIN_FILE=<path to vcpkg>` when calling the first cmake command

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=[Debug or Release] -DBUILD_TESTS=[ON/OFF] 
cmake --build
```

### IDEs 
IDEs with cmake support should automatically detect the project's CMakeLists.txt files, 
you will still need to add `-DCMAKE_TOOLCHAIN_FILE=<path to vcpkg>` to your toolchain configuration.

For vscode (using CodeLLVM) and nvim (using nvimp-dap & Overseer) I've set up launch and tasks json configs that you can use as-is
or modify to suit your needs. You will need to set the VCPKG_ROOT environment variable for them to be able to correctly direct cmake
to the vcpkg toolchain file.

### CMake options
* BUILD_TESTS<br>
builds tests target

