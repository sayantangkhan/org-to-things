# Org-to-Things

Turn Org files into HTML with links to import entries into [Things](https://culturedcode.com/things/).
This is a [filter](https://pandoc.org/filters.html) for [Pandoc](https://pandoc.org/) that traverses the Pandoc abstract-syntax-tree and adds Things links at the appropriate location.

## Compilation

To compile the executable, run `stack build`, after installing the system dependencies, or entering a `nix-shell`. See [shell.nix](shell.nix) for the list of system dependencies.

## Usage

The input to the filter must be a well-formatted Org file, which conforms to the specifications listed in [Specifications.org](Specifications.org).

Here is an example of the command line invocation.
    
    pandoc --toc --standalone --mathjax --css org.css --from org --to html --filter filter-exe agenda.org -o agenda.html

Upon receiving input it cannot parse, the filter will crash with a `Filter error`, and print the `Block` it failed to parse.