#include <Rcpp.h>
#include <H5Cpp.h>
#include <typeinfo>
using namespace Rcpp;

////////////////////////////////////////////////////////////////////////
void _write_integer_data(H5::H5File const& file, std::string const& name, IntegerVector const& data);
void _write_double_data(H5::H5File const& file, std::string const& name, NumericVector const& data);
void _write_string_data(H5::H5File const& file, std::string const& name, StringVector const& data);
void _read_integer_data(List& list, H5::DataSet const& dataset, std::string const& name);
void _read_double_data(List& list, H5::DataSet const& dataset, std::string const& name);
void _read_string_data(List& list, H5::DataSet const& dataset, std::string const& name);


////////////////////////////////////////////////////////////////////////
// [[Rcpp::export]]
void h5_write_flat(List const& data, std::string const& fname)
{
    StringVector const& names = data.names();

    // Validate that all list members are supported and have a name
    for (int i=0; i<data.length(); ++i) {
        if (names[i] == "") {
            stop("All members must be named");
            break;
        }
        switch (TYPEOF(data[i])) {
            case INTSXP:
            case REALSXP:
            case STRSXP:
                break;
            default:
                Function stop("stop");
                stop("Member '", as<char const*>(names[i]), "' uses an unsupported type");
                break;
        }
    }

    H5::H5File file(fname, H5F_ACC_TRUNC);

    for (int i=0; i<data.length(); ++i) {
        switch (TYPEOF(data[i])) {
            case INTSXP:
                _write_integer_data(file, as<std::string>(names[i]), as<IntegerVector>(data[i]));
                break;
            case REALSXP:
                _write_double_data(file, as<std::string>(names[i]), as<NumericVector>(data[i]));
                break;
            case STRSXP:
                _write_string_data(file, as<std::string>(names[i]), as<StringVector>(data[i]));
                break;
            default:
                Function stop("stop");
                stop("Member '", as<char const*>(names[i]), "' uses an unsupported type");
                break;
        }
    }
}


////////////////////////////////////////////////////////////////////////
// [[Rcpp::export]]
List h5_read_flat(std::string const& fname, Nullable<StringVector> const& datasets=R_NilValue)
{
    H5::H5File file(fname, H5F_ACC_RDONLY);

    std::vector<std::string> names;
    if (datasets.isNotNull()) {
        StringVector _datasets(datasets);
        for (size_t i=0; i<_datasets.length(); ++i) {
            names.push_back(as<std::string>(_datasets[i]));
        }
    }
    else {
        for (size_t i=0; i<file.getNumObjs(); ++i) {
            names.push_back(file.getObjnameByIdx(i));
        }
    }

    List list = List();

    for (std::vector<std::string>::iterator iter=names.begin(); iter!=names.end(); ++iter) {
        std::string const& name = *iter;
        H5::DataSet dataset;

        try {
            dataset = file.openDataSet(name);
        }
        catch(H5::FileIException not_found_error) {
            // We ignore missing datasets, leaving the caller to decice whether
            // they are considered mandatory
            // Function stop("stop");
            // stop("Could not find dataset '", name.c_str(), "' in file");
            continue;
        }

        H5T_class_t type_class = dataset.getTypeClass();
        switch (type_class) {
            case H5T_INTEGER:
                _read_integer_data(list, dataset, name);
                break;
            case H5T_FLOAT:
                _read_double_data(list, dataset, name);
                break;
            case H5T_STRING:
                _read_string_data(list, dataset, name);
                break;
            default:
                Function stop("stop");
                stop("Dataset '", name.c_str(), "' has an unsupported type");
         }
     }

     return list;
}


////////////////////////////////////////////////////////////////////////
template<class Vector>
H5::DataSet _create_dataset(H5::H5File const& file, std::string const& name, H5::DataType const& datatype, Vector const& data)
{
    H5::DataSet dataset;

    if (Rf_isNull(data.attr("dim"))) {
        int rank = 1;
        hsize_t dims[rank] = {data.length()};
        H5::DataSpace dataspace(rank, dims);

        dataset = file.createDataSet( name, datatype, dataspace );
    }
    else {
        IntegerVector const& dim = data.attr("dim");
        int rank = dim.length();
        hsize_t dims[rank];
        for (int i=0; i<dim.length(); ++i) {
            dims[i] = dim[rank-i-1];
        }
        H5::DataSpace dataspace(rank, dims);

        dataset = file.createDataSet( name, datatype, dataspace );
    }

    return dataset;
}


////////////////////////////////////////////////////////////////////////
void _write_integer_data(H5::H5File const& file, std::string const& name, IntegerVector const& data)
{
    H5::IntType datatype(H5::PredType::NATIVE_INT);
    datatype.setOrder(H5T_ORDER_LE);
    H5::DataSet const& dataset = _create_dataset(file, name, datatype, data);

    dataset.write(data.begin(), datatype);
}


////////////////////////////////////////////////////////////////////////
void _write_double_data(H5::H5File const& file, std::string const& name, NumericVector const& data)
{
    H5::FloatType datatype(H5::PredType::NATIVE_DOUBLE);
    H5::DataSet const& dataset = _create_dataset(file, name, datatype, data);

    dataset.write(data.begin(), datatype);
}


////////////////////////////////////////////////////////////////////////
void _write_string_data(H5::H5File const& file, std::string const& name, StringVector const& data)
{
    H5::StrType datatype(H5::PredType::C_S1, H5T_VARIABLE);
    H5::DataSet const& dataset = _create_dataset(file, name, datatype, data);

    size_t length = data.length();
    char const* raw_data[length];

    for (size_t i=0; i<length; ++i) {
        raw_data[i] = as<char const*>(data[i]);
    }

    dataset.write(raw_data, datatype);
}


////////////////////////////////////////////////////////////////////////
hsize_t _get_total_length(H5::DataSet const& dataset)
{
    H5::DataSpace const& dataspace = dataset.getSpace();
    unsigned rank = dataspace.getSimpleExtentNdims();
    hsize_t dims[rank];
    hsize_t length = 1;

    dataspace.getSimpleExtentDims(dims, NULL);
    for (unsigned i=0; i<rank; ++i) {
        length *= dims[i];
    }

    return length;
}


////////////////////////////////////////////////////////////////////////
template<class Vector>
void _set_dim(Vector& data, H5::DataSet const& dataset)
{
    H5::DataSpace const& dataspace = dataset.getSpace();
    unsigned rank = dataspace.getSimpleExtentNdims();

    if (rank == 1) {
        return;
    }

    IntegerVector dim(rank);
    hsize_t dims[rank];

    dataspace.getSimpleExtentDims(dims, NULL);
    for (unsigned i=0; i<rank; ++i) {
        dim[i] = dims[rank-i-1];
    }

    data.attr("dim") = dim;
}



////////////////////////////////////////////////////////////////////////
void _read_integer_data(List& list, H5::DataSet const& dataset, std::string const& name)
{
    hsize_t length = _get_total_length(dataset);
    H5::IntType datatype(H5::PredType::NATIVE_INT);
    datatype.setOrder(H5T_ORDER_LE);

    IntegerVector data(length);
    dataset.read(data.begin(), datatype);
    _set_dim(data, dataset);

    list.push_back(data, name);
}


////////////////////////////////////////////////////////////////////////
void _read_double_data(List& list, H5::DataSet const& dataset, std::string const& name)
{
    hsize_t length = _get_total_length(dataset);
    H5::FloatType datatype(H5::PredType::NATIVE_DOUBLE);

    NumericVector data(length);
    dataset.read(data.begin(), datatype);
    _set_dim(data, dataset);

    list.push_back(data, name);
}


////////////////////////////////////////////////////////////////////////
void _read_string_data(List& list, H5::DataSet const& dataset, std::string const& name)
{
    hsize_t length = _get_total_length(dataset);
    H5::StrType datatype(H5::PredType::C_S1, H5T_VARIABLE);

    char* raw_data[length];
    dataset.read(raw_data, datatype);

    StringVector data(length);
    for (hsize_t i=0; i<length; ++i) {
        data[i] = raw_data[i];
        H5free_memory(raw_data[i]);
    }

    _set_dim(data, dataset);

    list.push_back(data, name);
}
