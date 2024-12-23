-   [gmr 1.3.7.9001](#gmr-1.3.7.9001)
-   [gmr 1.3.7](#gmr-1.3.7)
    -   [New functions](#new-functions)
        -   [Functions to deal with Gmacs input/output
            files](#functions-to-deal-with-gmacs-inputoutput-files)
        -   [Functions related to the development
            module](#functions-related-to-the-development-module)
    -   [Updated functions](#updated-functions)
        -   [Functions to deal with Gmacs input/output
            files](#functions-to-deal-with-gmacs-inputoutput-files-1)
        -   [Functions to deal Gmacs code, executables and
            updates](#functions-to-deal-gmacs-code-executables-and-updates)
-   [gmr 1.3.6](#gmr-1.3.6)
    -   [New functions](#new-functions-1)
        -   [Functions to copy all GMACS input
            files](#functions-to-copy-all-gmacs-input-files)
    -   [Updated functions](#updated-functions-1)
        -   [Functions to read GMACS input/output
            files](#functions-to-read-gmacs-inputoutput-files)
        -   [Functions to update and release a new version of
            GMACS](#functions-to-update-and-release-a-new-version-of-gmacs)
-   [gmr 1.3.5](#gmr-1.3.5)
    -   [Updated functions](#updated-functions-2)
        -   [Functions to read GMACS input/output
            files](#functions-to-read-gmacs-inputoutput-files-1)
        -   [Functions to update and release a new version of
            GMACS](#functions-to-update-and-release-a-new-version-of-gmacs-1)
-   [gmr 1.3.4](#gmr-1.3.4)
    -   [New functions](#new-functions-2)
        -   [Functions to plot GMACS
            outputs](#functions-to-plot-gmacs-outputs)
        -   [Functions to read GMACS output
            files](#functions-to-read-gmacs-output-files)
        -   [Function to update GMACS](#function-to-update-gmacs)
    -   [Updated functions](#updated-functions-3)
        -   [Functions to plot GMACS
            outputs](#functions-to-plot-gmacs-outputs-1)
        -   [Functions to read/write GMACS input/output
            files](#functions-to-readwrite-gmacs-inputoutput-files)
        -   [Functions to update and release a new version of
            GMACS](#functions-to-update-and-release-a-new-version-of-gmacs-2)
-   [gmr 1.3.3](#gmr-1.3.3)
    -   [Updated functions](#updated-functions-4)
-   [gmr 1.3.2](#gmr-1.3.2)
    -   [Updated functions](#updated-functions-5)
-   [gmr 1.3.1](#gmr-1.3.1)
    -   [New functions](#new-functions-3)
    -   [Updated functions](#updated-functions-6)
-   [gmr 1.3.0](#gmr-1.3.0)
    -   [New functions](#new-functions-4)
        -   [Functions to read GMACS input
            files](#functions-to-read-gmacs-input-files)
        -   [Functions to write the GMACS input/output
            files](#functions-to-write-the-gmacs-inputoutput-files)
        -   [Generate simulation using
            GMACS](#generate-simulation-using-gmacs)
    -   [Updated functions](#updated-functions-7)
-   [gmr 1.2.0](#gmr-1.2.0)
    -   [New functions](#new-functions-5)
    -   [Updated functions](#updated-functions-8)
-   [gmr 0.1.1](#gmr-0.1.1)
    -   [New functions](#new-functions-6)
-   [gmr 0.1.0](#gmr-0.1.0)

# gmr 1.3.7.9001

------------------------------------------------------------------------

Here are some of the items for the next release:

-   [ ] Add examples and finish documenting functions.
-   [ ] Check all plotting functions
-   [ ] Create vignettes to build and run gmacs, make comparisons
    between code versions and between models settings
-   [ ] Create SAFE document templates
-   [ ] Add data and implement examples in the package

# gmr 1.3.7

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new version includes implementations linked to GMACS version
2.10.01, which involved changes to the read/write functions of GMACS
input files, implementation of a new workflow to update/upgrade Gmacs
(development module). This version also includes a better documentation
of most functions with examples.

## New functions

### Functions to deal with Gmacs input/output files

-   `clean_Inputfiles ()`: clean Gmacs input files in a folder.
-   `get_Param_name()`: get the name of all parameters. Useful to write
    .par/.pin files.
-   `getInpOutFiles()`: read all the Gmacs input files (‘gmacs.dat’,
    ‘model.dat’, ‘model.ctl’, ‘model.prj’) and and if they exist, the
    Gmacs output files (‘Gmacsall.out’, ‘gmacs.par’, ‘gmacs.rep’,
    ‘simdata.out’).

### Functions related to the development module

-   `set_DevStruct()`: Set up structure folder for the development of a
    new code version of Gmacs.
-   `CompareCodeVersion()`: run tests of comparison between two code
    versions of Gmacs.

## Updated functions

### Functions to deal with Gmacs input/output files

-   `clean_files()`: add the possibility to give specific file names and
    improve the process of deletion.
-   `copy_GMACSinputs()`: improve the process for copying files and
    checks.
-   `readGMACS.dat()`: add the read of the units.
-   `readGMACSallOUT()`: add new implementation of Gmacs and names for
    all parameters.
-   `readGMACSctl()`: add the maturity probability.
-   `readGMACSpar()`: add the description of each parameter and update
    stuff about maturity probability, environmental impact on selex.
-   `writeGmacs.dat()`: add stuff related to the development module.
-   `writeGmacsctlfile()`: add stuff related to the development module
    and the maturity probability.
-   `writeGmacsdatfile()`: add stuff related to the development module
    and new format for catch, size frequency and survey data frames.
-   `writeGmacsPAR()`: update to account for the number of digits when
    writing the ‘.par’/‘.pin’ files.
-   `writeGmacsprjfile()`: add stuff related to the development module.
-   `write_Gmacs_InputFiles()`: write all the stock-specific Gmacs input
    files

### Functions to deal Gmacs code, executables and updates

-   `.GetGmacsExe()`: add the possibility to specify the path to the
    repertory that holds the ‘.tpl’ file.
-   `insertTime()`;`insertTime2()`: add a development option.
-   `NewGMACSFeat()`: improve the way of adding new features in the
    ‘gmacsbase.tpl’.
-   `UpdateGMACS()`: change the header to update/upgrade Gmacs, add
    cleaning options related to the development module.
-   `write_TPL()`: add stuff about the development module.

# gmr 1.3.6

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new version includes implementations linked to GMACS version
2.01.M.09, which involved changes to the read/write functions of GMACS
input files. Changes have also been made to the GMACS compilation
functions to create a temporary folder when compiling GMACS, making it
easier to resolve errors in the code (#26).

## New functions

### Functions to copy all GMACS input files

-   `Copy_GMACS_Input_files()`: copy one or multiple GMACS input
    file(s).

## Updated functions

### Functions to read GMACS input/output files

-   `read_GMACS_dat()`: incorporating 1) stock specification (weight
    unit and stock name), 2) other controls settings (maximum phase,
    number of function calls, calculation of reference points, use of
    pin file, and verbose).
-   `readGMACSctl()`: 1) incorporating the maturity specific
    length-weight relationship, 2) the functional maturity terminally
    molting, 3) removing the other controls settings (maximum phase,
    number of function calls, calculation of reference points, use of
    pin file, and verbose).
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
    length-weight relationship, 2) the functional maturity terminally
    molting, and 3) removing the other controls settings.
-   `writeGmacsdatfile()`: account for the new format of input catch,
    relative abundance indices and size composition.
-   `writeGmacsprjfile()`: account for the new options for 1) the
    projection specifications controls, and 2) the Harvest Control rules
    settings.

### Functions to update and release a new version of GMACS

The updates improve the workflow for updating GMACS.

-   `GetGmacsExe()`: A temporary folder is now created when compiling
    GMACS. If errors when converting ‘gmacs.tpl’ to gmacs.cpp or
    compiling gmacs, either a “Error_convertion.txt” or a
    “Error_compilation.txt” file is created and indicates the source of
    the errors.
-   `clean_root`: delete (if applicable) the “Error_convertion.txt”
    and/or “Error_compilation.txt” files when updating GMACS.

# gmr 1.3.5

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release includes implementations related to the incorporation
of i) environmental impacts and possibility of random walk in the
vulnerability parameters (selectivity and retention parameters) (#24).

## Updated functions

### Functions to read GMACS input/output files

-   `readGMACSdat()`: incorporating the read of environmental indices.
-   `readGMACSctl()`: 1) change in the selectivity and retention
    matrices to allow environmental impacts and random walk; 2) Now read
    phase for random walk deviations; 3) new internal function to count
    the number of random deviation parameters to be estimated.
-   `readGMACSpar()`: Accounting for the environmental parameters.
-   `readGMACSallOUT()`: Reading environmental parameters and deviations
    in vulnerability.
-   `writeGmacsctlfile()`: account for environmental parameters and
    random walk deviations + formatting.
-   `writeGmacsdatfile()`: account for environmental data and
    formatting.
-   `writeGmacsPAR()`: account for environmental parameters and
    deviations in vulnerability + formatting.

### Functions to update and release a new version of GMACS

The updates improve the workflow for updating GMACS. \* `Do_Comp()` \*
`Do_GMACS()` \* `GMACS()` \* `UpdateGMACS()` \* `getVerGMACS()` \*
`NewGMACSFeat()`

# gmr 1.3.4

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new release includes implementations related to the CPT modelling
workshop hold in January 2023. Major implementations includes updates of
i) plotting functions, ii) functions allowing to read and write GMACS
input/output files, and iii) functions used when implementing and
releasing a new version of GMACS (#20).

## New functions

### Functions to plot GMACS outputs

-   `BaseThemeGMR()`: Specify the basic theme of `gmr` when plotting
    outputs

### Functions to read GMACS output files

-   `readGMACSallOUT()`: reads the `gmacsall.out` file
-   `readGMACSrep()`: read the `gmacs.rep` file

### Function to update GMACS

\*`NewGMACSFeat()`: creates a summary file detailing the latest
implementations in the gmacsbase.TPL file, when they’ve been done and by
whom.

## Updated functions

### Functions to plot GMACS outputs

-   `.get_F_df()`
-   `.get_cpue_df()`
-   `plot_catch()`
-   `plot_recruitment()`
-   `.get_sizeComps_df()`
-   `plot_basicOutput()`
-   `plot_catch()`

### Functions to read/write GMACS input/output files

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

### Functions to update and release a new version of GMACS

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

### Functions to read GMACS input files

-   `readGMACSfiles()`: Function to read all the GMACS input files
-   `readGMACS.dat()`: Function to read the gmacs.dat file
-   `readGMACSdat()`: Function to read the the data file
-   `readGMACSctl()`: Function to read the control file
-   `readGMACSprj`: Function to read the projection file

All these functions return the input files as a named list.

### Functions to write the GMACS input/output files

*`writeGmacs.dat()`: function to write the gmacs.dat file
*`writeGmacsdatfile()`: function to write data file
*`writeGmacsctlfile()`: function to write the control file
*`writeGmacsprjfile()`: function to write the projection file
*`writeGmacsPAR()`: function to write the gmacs.par file *
`GMACSversion()`: Function to extract the Version number of GMACS

### Generate simulation using GMACS

*`prepSim()`: function to get the original input files for the
simulation approach *`GenSimFiles()`: function to get generate the input
files to get the simulated data for each simulation *`SimData()`:
Function to generate the data *`SaveSimFiles()`: Function to save the
outputs of each simulation-estimation run *`Gen_GmacsSim()`: Function to
generates the simulated data for a set of simulation *`RunGmacsSim()`:
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
-   `read.OUT()`, which is designed to read the ‘gmacsall.OUT’ (output
    of GMACS) to find estimates of specific management/biological
    quantities.

Other functions (called within the ones described above) have also been
added to the `gmr` package but are internal functions.

# gmr 0.1.0

------------------------------------------------------------------------

This is the status of the gmr package in March 2020. It will serve as
the basis for the futrure developments of the `gmr` package.
