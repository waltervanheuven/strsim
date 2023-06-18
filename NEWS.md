## strsim 1.2.2

- Improved descriptions.
- Renamed several functions.
- Added check in `old()` to ensure it can be calculated.
- Checked output of `old20()` against `old20()` in the [vwr::old20()] [vwr R package](http://crr.ugent.be/archives/480), and 
previous versions of the `strsim` package. Output values are identical.

## strsim 1.2.1

- Fixed `mclapply()`.

## strsim 1.2.0

- Improved `old20()`.
- Progress bar is now optional.
- Parallel execution is now optional.

## strsim 1.1.0

- Added neighborhood density and neighbourhood frequency functions. Neighborhood density
  function is way faster (~44x) than the previous calculate_coltheartN() function.
- Removed `calculate_coltheartN()` and `coltheartN()` functions.
- `old20()` uses now data.table.

## strsim 1.0.0

- First release.
