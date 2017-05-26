# RWDataPlyr 0.5.0.9000

# RWDataPlyr 0.5.0

*Released May 26, 2017*

## Major new features

* Modified all aggregation methods that check if the data are <, <=, >, or >= a threshold. Previously, these aggregation methods returned either 0 (FALSE) or 100 (TRUE), now they return 1 for TRUE. This allows us to keep data in its original state before plotting with  `scales::percent`, and keep us from having to multiply/divide by 100 if we compute other averages outside of RWDataPlyr. **However, this will cause any plotting code that is expecting percentages between 0 and 100 instead of between 0 and 1 to break (or at least look weird).** (#53)
* Added `findAllSlots` boolean parameter to `getDataForAllScens()`
    * If this is `TRUE` an error will post if one or more of the slots cannot be found. If it is `FALSE`, then it will fill in the data frame with `-99`, but not fail. (#38)
* New function: `makeAllScenNames()` will create scenario names for vectors of multiple dimensions.
* New function: `getWYFromYearmon()` will take a `yearmon` object and determine the water year it falls into.
* Added `WYMaxLTE` as a valid aggregation method. This will compare the maximum value in a water year to a threshold and determine if it is less than or equal to the threshold.

## Minor improvements

* Better error messages in `createSlotAggList()` (#45)
* Improved the error message if an invalid aggregation method is passed to `processSlots()` (#42)
* Ensure that `getDataForAllScens()` (and interal function `processSlots()`) work with rdf files that only include one trace of data (#40)
* `getDataForAllScens()` through `processSlots()` now returns the same type/class for `Year` and `Variable` columns for both annual and monthly data. `Yea`r is a numeric and `Variable` is always a character. **This could affect code that does not read the data frame back in and assumes that Variable is a factor. (#54)**

## Under-the-hood improvements

* Improved tests for `createSlotAggList()`
* Added tests for `RWDataPlyr:::processSlots()`
* Now check for invalid aggregation methods in `createSlotAggList()`, so it will not wait until `processSlots()` is called to throw the error. 
