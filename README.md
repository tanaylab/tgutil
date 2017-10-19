# tgutils package - Tanay's group collection of simple utility functions#

## Sparse Matrix I/O ##

#### h5_write_sparse() ####

``` R
h5_write_sparse(x, fname)
```

Write the contents of the sparse matrix `x` into the file `fname` using an HDF5 based format. If the file already exists it is overwritten.

The resulting file includes the following datasets:

* `i`, `j`, `x` - The matrix's data, as represented by a three column syntax
* `dims` - The original dimensions of the matrix
* `colnames`, `rownames` - The matrix's colnames and rownames. These datasets will be omitted if the matrix does not include column or row names.


#### h5_read_sparse() ####

``` R
h5_read_sparse(fname)
```

Read a sparse matrix from a file created by `h5_write_sparse()`.


## HDF5 I/O ##

#### ```write_flat_hdf5()``` ###

``` R
h5_write_flat(list, fname)
```

Write the context of `list` into the file `fname` using the HFD5 format. If the file already exists it is overwritten. `list` may only include vectors or arrays of integers, floats or strings (any other data type will result in an error). Also, all members of `list` must be named - the names are used as the dataset names within the file.

The generated HDF5 file is flat, i.e. the members of `list` are stored as datasets at the top level of the HDF5 files, with no intervening groups.

The package provides the convenience function `argpack` which allows packing arbitrary arguments into `list`. For example:
``` R
write_flat_hdf5(argpack(x, y, z), 'sample.h5')
```
Will store the contents of the variables `x`, `y`, and `z` into the HDF5 file `sample.h5`. The datasets within the file will be names `'x'`, `'y'`, and `'z'`.

#### ```read_flat_hdf5()``` ###

``` R
h5_read_flat(fname, datasets=NULL)
```

Reads a file that was generated by `write_flat_hdf5()`. `datasets` is a character vector containing the datasets that should be read from the file. If it is omitted, all datasets in the file will be read. Note that no error will be generated if a dataset appearing in `datasets` is missing from the file, and that dataset will be silently ignored.

The function returns a list containing the datasets that were read.
