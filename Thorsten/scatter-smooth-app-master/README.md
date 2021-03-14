In this exercise you write the *work horse functions* for a small shiny
app. The app will be a tool to illustrate various smoothing approaches.
I provide a skeleton for the user interface `ui.R` and the server
`server.R` functions of the app. All you have to do is to collect the
*work horse functions* is a script call `scatter-smooth-functions.R`.

The *work horse functions* are

------------------------------------------------------------------------

#### `simdata()`

This function simulates the toy data.

**Input**

-   `n` – interger, number of observations. Default `200`.
-   `sd` – positive numeric, standard deviation of normal distribution.
    Default `0.2`.
-   `true_effect` – a function, giving the true effect. Default
    sin (2 ⋅ (4 ⋅ `x` − 2)) + 2 ⋅ exp ( − 256 ⋅ (`x` − 0.5)<sup>2</sup>).
-   `seed` – integer, a seed to be set before simulation. Default
    `NULL`.

**Body**

-   Generate covariate `x` as sequence from `0` to `1` with length `n`.
-   Simulate reponse `y` by drawing `n` samples from a normal
    distribution with `true_effect` evaluated at `x` as mean and
    standard deviation `sd`.

**Output**

-   A dataframe with columns `x` and `y`.

------------------------------------------------------------------------

#### `scatter_smooth()`

This functions smoothes the data and visualizes the smoothing.

**Input**

-   `formula` – a formula specifying the smoothing model,
    e.g. `y ~ 0 + tp(x)`.
-   `data` – a dataframe containing the variables `x` and `y`.
-   `true_effect` – a function (optional), giving the true effect.

**Body**

-   Fit a linear model `lm` with `formula` and `data`
-   Scatter plot that explains `y` by `x`.
-   Add the fitted effect, and optionally the true effect.

**Output**

-   Return invisibly the linear model `fm`.

------------------------------------------------------------------------

Further, the script `scatter-smooth-functions.R` could collect
additional functions that could serves, e.g., as basis expansion such as
`tp()`.

------------------------------------------------------------------------

After you coded these functions start a fresh R session and `source()`
your script. Now, simulate some data, and try different smoothings via
the formula that goes into `scatter_smooth()`, e.g.,
`y ~ 1 + poly(x, 11, raw = TRUE)` or `y ~ 0 + tp(x, 3, 11)` to make sure
that your functions work.

When everything works you can run the app. All three R scripts have to
be in the same directory. You can start running the app by

-   using the button *Run App* in RStudio’s editor window when `ui.R` or
    `server.R` is opened in the editor, or

-   by calling `shiny::runApp()` in your R session when your working
    directory is the same as the directory with the R scripts.

This script `scatter-smooth-functions.R` will be sourced by `server.R`.
Thus, the app will show your visialization of the data, the (smooth)
fit, and the true effect. Play around with the given options.

------------------------------------------------------------------------

Feel free to extend the app. Here are some ideas:

-   Add more basis expansions, such as thin plate regression splines, …

-   Add an option to allow for regularized estimation (penalized least
    squares) of the effect.

-   Allow different datasets (e.g., `mcycle`), …

-   Visualize raw and weighted basis funcitons, …

There is an upload directory in OLAT. If you want to share your solution
or app either

-   upload your work horse functions as
    `scatter-smooth-functions-<surname>.R`, or

-   if you adapted `ui.R` and/or `server.R` upload all your files zipped
    as `scatter-smooth-app-<surname>.zip`.
