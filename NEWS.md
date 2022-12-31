-   [gmr 1.3.0.9001](#gmr-1.3.0.9001)
-   [gmr 1.3.0](#gmr-1.3.0)
    -   [New functions](#new-functions)
    -   [Updated functions](#updated-functions)
-   [gmr 1.2.0](#gmr-1.2.0)
    -   [New functions](#new-functions-1)
    -   [Updated functions](#updated-functions-1)
-   [gmr 0.1.1](#gmr-0.1.1)
    -   [New functions](#new-functions-2)
-   [gmr 0.1.0](#gmr-0.1.0)

# gmr 1.3.0.9001

------------------------------------------------------------------------

Here are some of the items for the next release:

-   [ ] Check all plotting functions
-   [ ] Improve code
-   [ ] Create vignettes to build and run gmacs, make comparison
-   [ ] Create SAFE document templates
-   [ ] Add data and implement examples in the package

# gmr 1.3.0

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This is a minor version update of `gmr` that includes new functions to
i) work with the simulation module of GMACS, ii) read input and some
output files of GMACS and, iii) write the GMACS input files.

## New functions

1.  Functions to read GMACS input files

-   `readGMACSfiles()`: Function to read all the GMACS input files
-   `readGMACS.dat()`: Function to read the gmacs.dat file
-   `readGMACSdat()`: Function to read the the data file
-   `readGMACSctl()`: Function to read the control file
-   `readGMACSprj`: Function to read the projection file All these
    functions return the input files as a named list.

1.  Functions to write the GMACS input/output files *`writeGmacs.dat()`:
    function to write the gmacs.dat file *`writeGmacsdatfile()`:
    function to write data file *`writeGmacsctlfile()`: function to
    write the control file *`writeGmacsprjfile()`: function to write the
    projection file \*`writeGmacsPAR()`: function to write the gmacs.par
    file

-   `GMACSversion()`: Function to extract the Version number of GMACS

1.  Generate simulation using GMACS *`prepSim()`: function to get the
    original input files for the simulation approach *`GenSimFiles()`:
    function to get generate the input files to get the simulated data
    for each simulation *`SimData()`: Function to generate the data
    *`SaveSimFiles()`: Function to save the outputs of each
    simulation-estimation run *`Gen_GmacsSim()`: Function to generates
    the simulated data for a set of simulation *`RunGmacsSim()`:
    Function to run the GMACS simulation-estimation \*`clean_bat_Sim()`:
    Function to clean gmacs simulation-estimation approach

## Updated functions

-   `Do_GMACS()`
-   `read_GMACS.dat()`
-   `Do_Comp()`

# gmr 1.2.0

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This is a minor version update that includes modification of some
functions to make `gmr` compatible with MacOs/Linux and avoid the use of
paste() and paste0() for defining file paths to file.path. Some
additions have also been made in function documentation (#13).

*Versioning has been updated correctly. (change from 0.1.1 to 1.2.0).*

## New functions

-   `createGmacsExe()`: Function to get the GMACS executable from TPL
    file.
-   `clean_files()`: Function to clean files in a specific folder.
-   `clean_root()`: Function to clean gmacs compilation-process files in
    the “root folder”.
-   `clean_bat()`: Function to clean gmacs output files in a folder.
-   `isWindowsOS`: Identify if the OS type is windows

## Updated functions

-   `Do_GMACS()`
-   `.buildGMACS()`
-   `write_TPL()`: is now exported (#11).

# gmr 0.1.1

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new version allows to directly run GMACS within R instead of going
through the various windows prompt (#1).

## New functions

-   `GMACS()`, which allows you to compile, run and make comparison of
    different versions of GMACS for one or several stocks. This is the
    main function to work with the GMACS model with the `gmr` package.
-   `Do_GMACS()`, which is called by the `GMACS()` function. It allows
    to build the GMACS executable, run the model and make comparison
    between multiple versions of GMACS if the user ask for.
-   `Do_Comp()`, which establishes comparison tables of management
    quantities between different versions of GMACS.
-   `.GetGmacsExe()`, which allows to update and “release” a new version
    of GMACS.
-   `.CallTerm()`, which allows to call a R terminal to execute a
    specific command.
-   `read.OUT()`, which is designed to read the gmacsall.OUT (output of
    GMACS) to find estimates of specific management/biological
    quantities.

Other functions (called within the ones described above) have also been
added to the `gmr` package but are internal functions.

# gmr 0.1.0

------------------------------------------------------------------------

This is the status of the gmr package in March 2020. It will serve as
the basis for the futrure developments of the `gmr` package.
