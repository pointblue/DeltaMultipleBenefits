# create_directory

Helper function called within other functions to check whether the
intended output directory exists, and if not, create it. Particularly
useful when many files are generated programmatically with an expected
structure.

## Usage

``` r
create_directory(filepath)
```

## Arguments

- filepath:

  character string representing a file path

## Value

Prints either "Creating directory: filepath" if directory did not
already exist, or "Writing to directory: filepath" if it did already
exist.

## Examples

``` r
if (FALSE) { # \dontrun{
create_directory(filepath = "Desktop/example")

# will create nested directories as needed
create_directory(filepath = "Desktop/example/test")
} # }
```
