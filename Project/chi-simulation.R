## estimate column probabilities
estimate_p = function(x)
{
  ctotals = apply(x,2,sum)
  ttotal = sum(x)
  p0 = ctotals / ttotal
  return ( p0 )
}

## get row totals
row_totals = function(x)
{
  return ( apply(x,1,sum) )
}

## simulate a table given row totals and null column probabilites
simulate_one_table = function(rsums,p0)
{
  ncols = length(p0)
  nrows = length(rsums)
  x = matrix(0,nrows,ncols)
  for ( i in seq_len(nrows) )
    x[i,] = tabulate(sample(ncols, size = rsums[i], prob = p0, replace = TRUE), nbins = ncols)
  return(x)
}

## get expected counts
get_expected = function(x)
{
  p0 = estimate_p(x)
  rtotals = row_totals(x)
  expected = rtotals %o% p0
  return ( expected )
}

## calculate chi-square-statistic
calc_chi_sq = function(x)
{
  expected = get_expected(x)
  x2 = sum( (x-expected)^2/expected )
  return ( x2 )
}

## simulate null sampling distribution

simulate_null_independence = function(B,x)
{
  out = numeric(B)
  rsums = row_totals(x)
  p0 = estimate_p(x)
  for ( i in seq_len(B) )
    out[[i]] = calc_chi_sq( simulate_one_table(rsums,p0) )
  return ( out )
}
