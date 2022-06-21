-   [gmr 0.1.1.9000](#gmr-0.1.1.9000)
-   [gmr 0.1.1](#gmr-0.1.1)
    -   [New functions](#new-functions)
-   [gmr 0.1.0](#gmr-0.1.0)

# gmr 0.1.1.9000

------------------------------------------------------------------------

Here are some of the items for the next release:

-   [x] Build a website
-   [ ] Check all plotting functions
-   [ ] Improve code
-   [ ] Create vignettes to build and run gmacs, make comparison
-   [ ] Create SAFE document templates
-   [ ] Add data and implement examples in the package

# gmr 0.1.1

------------------------------------------------------------------------

(GitHub issue/PR numbers in parentheses)

This new version allows to directly run GMACS within R instead of going
through the various windows prompt (#1).

## New functions

-   `GMACS()`, which allows you to compile, run and make comparison of
    different versions of GMACS for one or several stocks. This is the
    main function to work with the GMACS model with the `gmr` package.
-   `Do_GMACS`, which is called by the `GMACS()` function. It allows to
    build the GMACS executable, run the model and make comparison
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
