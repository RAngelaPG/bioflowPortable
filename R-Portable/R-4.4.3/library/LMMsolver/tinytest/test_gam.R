## Use the barley data from a uniformity field trial.
data("piepho.barley.uniformity", package = "agridat")

# Remove NA to prevent spurious warnings.
dat <- piepho.barley.uniformity[!is.na(piepho.barley.uniformity[["yield"]]), ]

## Some tests on correct input checking for combinations of splines.

expect_error(LMMsolve(fixed = yield ~ 1,
                      spline = ~spl1D(x = roww, nseg = 36) +
                        spl1D(x = col, nseg = 30),
                      data = dat),
             "in the spline part of the model are not in the data")
expect_error(LMMsolve(fixed = yield ~ 1,
                      spline = ~spl1D(x = row, nseg = 36) +
                        spl1D(x = coll, nseg = 30),
                      data = dat),
             "in the spline part of the model are not in the data")
expect_error(LMMsolve(fixed = yield ~ 1,
                      spline = ~spl1D(x = roww, nseg = 36) +
                        spl1D(x = coll, nseg = 30),
                      data = dat),
             "in the spline part of the model are not in the data")

## Only splines with different variables allowed.
expect_error(LMMsolve(fixed = yield ~ 1,
                      spline = ~spl1D(x = row, nseg = 36) +
                        spl2D(x1 = col, x2 = row, nseg = c(30, 36)),
                      data = dat),
             "variables in splines should be unique")
expect_error(LMMsolve(fixed = yield ~ 1,
                      spline = ~spl1D(x = row, nseg = 36) +
                        spl1D(x = row, nseg = 30),
                      data = dat),
             "x variables in 1D splines should be unique")

## 1D splines for rows and columns.
obj0 <- LMMsolve(fixed = yield ~ 1,
                 spline = ~spl1D(x = row, nseg = 4) +
                   spl1D(x = col, nseg = 3),
                 data = dat)

## From R 4.3 there is an extra item in the family output.
## This gives problems with the comparison.
## Therefore it is removed first.

obj0$family$dispersion <- NULL

## Check that full LMM solve object is correct.
expect_equivalent_to_reference(obj0, "gam1DFull")

