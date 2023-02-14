Space Curves
================

``` r
gen_df = function(x_fn, y_fn, z_fn, 
                  n=1000, 
                  lb=-1,
                  ub=1) {
  # Generate a 3D dataframe of n samples.
  # Inputs generated from range [lb, ub]
  # x_fn, y_fn, z_fn applied to each respective dimension
  # Returns a matrix
  
  data = c(
    seq(lb, ub, length = n),
    seq(lb, ub, length = n),
    seq(lb, ub, length = n)
  )
  
  res = matrix(data, nrow = n, ncol = 3)
  res[,1] = sapply(res[,1], x_fn)
  res[,2] = sapply(res[,2], y_fn)
  res[,3] = sapply(res[,3], z_fn)
  
  return(res)
}



plot_3d_lines = function(data) {
  # Simple Interactive 3D Line Plot
  fig = plot_ly(
    data.frame(data), x = data[,1], y = data[,2], z = data[,3],
    type = "scatter3d", mode = "lines"
  )
  fig
}
```

## Simple Helix

``` r
helix = gen_df(
  x_fn = \(t) cos(t),
  y_fn = \(t) sin(t),
  z_fn = \(t) t,
  lb=-8, ub=8, n = 2000
)
plot_3d_lines(helix)
```

![](space_curves_files/figure-commonmark/unnamed-chunk-3-1.png)

## Twisted Cubic

``` r
twisted_cubic = gen_df(
  x_fn = \(t) return(t),
  y_fn = \(t) return(t**2),
  z_fn = \(t) return(t**3),
  lb = -5, ub = 5, n = 2000
)
plot_3d_lines(twisted_cubic)
```

![](space_curves_files/figure-commonmark/unnamed-chunk-4-1.png)

## Toroidal Spiral

``` r
toroidal_spiral = function(coef_1, coef_2) {
  gen_df(
    x_fn = \(t) (coef_1 + sin(coef_2*t))*cos(t),
    y_fn = \(t) (coef_1 + sin(coef_2*t))*sin(t),
    z_fn = \(t) cos(coef_2*t),
    lb = -10, ub = 10, n = 5000
  )
}

plot_3d_lines(toroidal_spiral(5, 20))
```

![](space_curves_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
plot_3d_lines(toroidal_spiral(1, 50))
```

![](space_curves_files/figure-commonmark/unnamed-chunk-5-2.png)

``` r
plot_3d_lines(toroidal_spiral(3, pi))
```

![](space_curves_files/figure-commonmark/unnamed-chunk-5-3.png)

## Trefoil Knot

``` r
trefoil_knot = function(coef_1, coef_2) {
  gen_df(
    x_fn = \(t) (coef_1 + cos(coef_2*t))*cos(t),
    y_fn = \(t) (coef_1 + cos(coef_2*t))*sin(t),
    z_fn = \(t) sin(coef_2*t),
    lb = -8, ub = 8, n = 2000
  )
}

plot_3d_lines(trefoil_knot(2, 1.5))
```

![](space_curves_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
plot_3d_lines(trefoil_knot(1, 10))
```

![](space_curves_files/figure-commonmark/unnamed-chunk-6-2.png)

``` r
plot_3d_lines(trefoil_knot(pi, pi))
```

![](space_curves_files/figure-commonmark/unnamed-chunk-6-3.png)
