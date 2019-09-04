<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/tanaylab/tgutil.svg?branch=master)](https://travis-ci.org/tanaylab/tgutil)
<!-- badges: end -->

tgutil package - Tanay’s group collection of simple utility functions
=====================================================================

Read legacy csv’s
-----------------

### fread\_rownames

``` r
fread_rownames(..., row.var='rowname')
```

Use `fread()` to read a csv/tsv with row names (e.g. one created with
`read.table()`). The name of the column that will hold the row names is
given in `row.var`.

All arguments are passed to the underlying `fread()` call. Note that the
following arguments can’t be used (and will be silently ignored):
`header`, `skip`, `col.names`.

Sparse Matrix I/O
-----------------

### fwrite\_mm

``` r
fwrite_mm(x, fname, sep=' ', row.names=TRUE, col.names=TRUE)
```

Write the contents of the sparse matrix `x` into the text file `fname`
in the MatrixMarket format (see
<a href="http://math.nist.gov/MatrixMarket/formats.html" class="uri">http://math.nist.gov/MatrixMarket/formats.html</a>).

The MatrixMarket format does not save row and column names.
`fwrite_mm()` allows saving them into separate files using the
`row.names` and `col.names` arguments. These arguments may contain file
names into which the column and row names will be written. Alternately
setting these arguments to `TRUE` will cause the row and column names to
be written to `<fname>.rownames` and `<fname>.colnames` respectively.

The argument `sep` indicates the field separator that will be used. Note
that using any field separator other than space will result in an output
that is not compliant with the MatrixMarket format.

### fread\_mm

``` r
fread_mm(fname, sep=' ', row.names=TRUE, col.names=TRUE)
```

Reads a sparse matrix in the MatrixMarket format from the file `fname`.

The MatrixMarket format does not save row and column names. `fread_mm()`
allows reading them from separate files using the `row.names` and
`col.names` arguments. These arguments may contain file names from which
the column and row names will be read. Alternately setting these
arguments to `TRUE` will cause the row and column names to be read from
`<fname>.rownames` and `<fname>.colnames` respectively.
