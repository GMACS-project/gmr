-   [gmr 1.3.6.9001](#gmr-1.3.6.9001)
-   [gmr 1.3.6](#gmr-1.3.6)
    -   [New functions](#new-functions)
    -   [Updated functions](#updated-functions)
-   [gmr 1.3.5](#gmr-1.3.5)
    -   [Updated functions](#updated-functions-1)
-   [gmr 1.3.4](#gmr-1.3.4)
    -   [New functions](#new-functions-1)
    -   [Updated functions](#updated-functions-2)
-   [gmr 1.3.3](#gmr-1.3.3)
    -   [Updated functions](#updated-functions-3)
-   [gmr 1.3.2](#gmr-1.3.2)
    -   [Updated functions](#updated-functions-4)
-   [gmr 1.3.1](#gmr-1.3.1)
    -   [New functions](#new-functions-2)
    -   [Updated functions](#updated-functions-5)
-   [gmr 1.3.0](#gmr-1.3.0)
    -   [New functions](#new-functions-3)
    -   [Updated functions](#updated-functions-6)
-   [gmr 1.2.0](#gmr-1.2.0)
    -   [New functions](#new-functions-4)
    -   [Updated functions](#updated-functions-7)
-   [gmr 0.1.1](#gmr-0.1.1)
    -   [New functions](#new-functions-5)
-   [gmr 0.1.0](#gmr-0.1.0)

# gmr 1.3.6.9001

------------------------------------------------------------------------

Here are some of the items for the next release:

-   [ ] Update the functions to read GMACS output files (related to the
    GMACS version 2.01.M.09)
-   [ ] Check all plotting functions
-   [ ] Create vignettes to build and run gmacs, make comparison
-   [ ] Create SAFE document templates
-   [ ] Add data and implement examples in the package

# gmr 1.3.6

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new version includes implementations linked to GMACS version
2.01.M.09, which involved changes to the read/write functions of GMACS
input files. Changes have also been made to the GMACS compilation
functions to create a temporary folder when compiling GMACS, making it
easier to resolve errors in the code (#26).

## New functions

1.  Functions to copy all GMACS input files

-   `Copy_GMACS_Input_files()`: copy one or multiple GMACS input
    file(s).

## Updated functions

1.  Functions to read GMACS input/output files

-   `read_GMACS_dat()`: incorporating 1) stock specification (weight
    unit and stock name),

1.  other controls settings (maximum phase, number of function calls,
    calculation of reference points, use of pin file, and verbose).

-   `readGMACSctl()`: 1) incorporating the maturity specific
    length-weight relationship,

1.  the functional maturity terminally molting, 3) removing the other
    controls settings (maximum phase, number of function calls,
    calculation of reference points, use of pin file, and verbose).

-   `readGMACSdat()`: incorporating the option for the new format of
    input catch, relative abundance indices and size composition.
-   `readGMACSprj()`: incorporating 1) new options for the projection
    specifications controls (number of state strategies and F range),
    and 2) new options for the Harvest Control rules settings (Apply
    strategies \[OFL, ABC\], Apply the state strategy, Number of state
    parameters).
-   `writeGmacs_dat()`: account for stock specification and other
    controls settings.
-   `writeGmacsctlfile()`: account for the maturity specific
    length-weight relationship,

1.  the functional maturity terminally molting, and 3) removing the
    other controls settings.

-   `writeGmacsdatfile()`: account for the new format of input catch,
    relative abundance indices and size composition.
-   `writeGmacsprjfile()`: account for the new options for 1) the
    projection specifications controls, and 2) the Harvest Control rules
    settings.

1.  Functions to update and release a new version of GMACS

The updates improve the workflow for updating GMACS.

-   `GetGmacsExe()`: A temporary folder is now created when compiling
    GMACS. If errors when converting gmacs.tpl to gmacs.cpp or compiling
    gmacs, either a “Error_convertion.txt” or a “Error_compilation.txt”
    file is created and indicates the source of the errors.
-   `clean_root`: delete (if applicable) the “Error_convertion.txt”
    and/or “Error_compilation.txt” files when updating GMACS.

# gmr 1.3.5

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release includes implementations related to the incorporation
of i) environmental impacts and possibility of random walk in the
vulnerability parameters (selectivity and retention parameters) (#24).

## Updated functions

1.  Functions to read GMACS input/output files

-   `readGMACSdat()`: incorporating the read of environmental indices.
-   `readGMACSctl()`: 1) change in the selectivity and retention
    matrices to allow environmental impacts and random walk; 2) Now read
    phase for random walk deviations;

1.  new internal function to count the number of random deviation
    parameters to be estimated.

-   `readGMACSpar()`: Accounting for the environmental parameters.
-   `readGMACSallOUT()`: Reading environmental parameters and deviations
    in vulnerability.
-   `writeGmacsctlfile()`: account for environmental parameters and
    random walk deviations + formatting.
-   `writeGmacsdatfile()`: account for environmental data and
    formatting.
-   `writeGmacsPAR()`: account for environmental parameters and
    deviations in vulnerability + formatting.

1.  Functions to update and release a new version of GMACS

The updates improve the workflow for updating GMACS.

-   `Do_Comp()`
-   `Do_GMACS()`
-   `GMACS()`
-   `UpdateGMACS()`
-   `getVerGMACS()`
-   `NewGMACSFeat()`

# gmr 1.3.4

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release includes implementations related to the CPT modelling
workshop hold in January 2023. Major implementations includes updates of
i) plotting functions, ii) functions allowing to read and write GMACS
input/output files, and iii) functions used when implementing and
releasing a new version of GMACS (#20).

## New functions

1.  Functions to plot GMACS outputs

-   `BaseThemeGMR()`: Specify the basic theme of `gmr` when plotting
    outputs

1.  Functions to read GMACS output files

-   `readGMACSallOUT()`: reads the `gmacsall.out` file
-   `readGMACSrep()`: read the `gmacs.rep` file

1.  Function to update GMACS \*`NewGMACSFeat()`: creates a summary file
    detailing the latest implementations in the gmacsbase.TPL file, when
    they’ve been done and by whom.

## Updated functions

1.  Functions to plot GMACS outputs

-   `.get_F_df()`
-   `.get_cpue_df()`
-   `plot_catch()`
-   `plot_recruitment()`
-   `.get_sizeComps_df()`
-   `plot_basicOutput()`
-   `plot_catch()`

1.  Functions to read/write GMACS input/output files

-   `readGMACS.dat()`
-   `readGMACSdat()`
-   `readGMACSctl()`
-   `readGMACSfiles()`
-   `readGMACSpar()`
-   `readGMACSprj()`
-   `readGMACSsimdat()`
-   `writeGmacs.dat()`
-   `writeGmacsctlfile()`
-   `writeGmacsdatfile()`
-   `writeGmacsprjfile()`

1.  Functions to update and release a new version of GMACS

-   `.GetGmacsExe()` \*`createGmacsExe()`
-   `GMACS()`
-   `UpdateGMACS()`
-   `insertTime2()`

# gmr 1.3.3

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release comes with the PR (#19) and updates the function to
read the control file and the one to realize simulations.

## Updated functions

-   `readGMACSctl()`: Number of nodes for cubic spline or number of
    step-changes for option 3 (#18)
-   `SaveSimFiles()`: Save outputs of the simulation approach (#18)

# gmr 1.3.2

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release allows to run the simulation under Linux.

## Updated functions

-   `RunGmacsSim()` (#17)

# gmr 1.3.1

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release incorporates a new function to plot the basic outputs
from GMACS and give the possibility to specify a given file to set the
directories needed to call ADMB within R.

## New functions

-   `plot_basicOutput()`: function to plot the basic outputs of GMACS
    (#15)

## Updated functions

-   `createGmacsExe()`: add the `ADMBpaths` argument in the function
    (#16)

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
