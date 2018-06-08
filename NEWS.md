# RWDataPlyr 0.6.0.9000

*In development*

* `rw_scen_aggregate()` and `rdf_aggregate()` gain `cpp` arguments. If `TRUE`, then `rdf_to_rwtbl2()` is used instead of `rdf_to_rwtbl()`
* new function: `rdf_to_rwtbl2()`
* `read_rdf()` and `read.rdf()` gained an `rdf` argument. If `TRUE`, it returns an `rdf` object, otherwise it returns a character vector.
* in `rdf_to_rwtbl()`, `scenario` is coerced into a character
* deprecating `rdf_to_rwtbl()` in favor of `rdf_to_rwtbl2()`
* improved `read_rdf()` error messages (#86)
* `rw_scen_aggregate()` will now work with unnamed `scenarios` and `NULL` `scen_names` arguments (#81)
* `rwslot_*` functions now error if the data passed to them are not regular (January - December or October - September) (#83)
    - as part of this, the matrix returned by `rdf_get_slot()` now has a `"timespan"` attribute that corresponds to the start and end values of the rdf.
* Updated documentation for columns returned by `rdf_aggregate()` and `rw_scen_aggregate()` (#84)
* `rdf_aggregate()` and `rw_scen_aggregate()` gain a `verbose` parameter (#82)


# RWDataPlyr 0.6.0

*Released April 10, 2018*

RWDataPlyr v0.6.0 includes a major revamping of how scenarios are processed and defines several new classes.

## Major new features

* New functions `rw_scen_aggregate()` and `rdf_aggregate()` added to upgrade existing `getDataForAllScens()` function, which processes multiple scenarios at a time. (#51)
    - These two functions rely on the new `rwd_agg` class, which upgrades the exising "slot aggregation list". The advantage of the new class is a much more flexible way to summarize and aggregate RiverWare slots. (#68)
        - This class includes methods for `rbind()`, `cbind()`, `as`, and `is`.
    - Helper functions to print all slots that exist in the tibble (`rwtbl_slot_names()`), get the original scenario folders (`rwtbl_get_scen_folder()`), and get RiverWare slots names from the saved variable name (`rwtbl_var_to_slot()`) were added. (#50)
 * `getDataForAllScens()` now always returns data invisibly, so the `retFile` arguement is deprecated. This function is also deprecated in favor of `rw_scen_aggregate()` and/or `rdf_aggregate()`. (#66)
 * Formalized the list returned by `createSlotAggList()` as a `slot_agg_list` class. Created applicable constructor, which deprecates `createSlotAggList()`. Includes `print()`, `summary()` and `is.`/`is_` methods and functions. (#67)
    - Added check in creation to ensure all variables are unique (if specified) (#64, #62)
    - However, `slot_agg_list` objects work with the deprecated `getDataForAllScens()`, so `rwd_agg` objects are preferable. 
* Added`read_rw_csv()` to read csv files created from RiverWare or RiverSMART. (#30)
* Added`rdf_to_rwtbl()` to convert rdf objects (read in from `read.rdf()`) to tibble objects. (#30)
* Added `rdf` class
    - `read.rdf()` now returns an object with an rdf class
    - Functions that expected rdf lists now expect an rdf object, e.g., `rdfSlotToMatrix()`.
* Switched to snake_case for all new functions and replaced existing functions with snake_case versions. (#76)

    | Old Function | New Function |
    | ------------ | ------------ |
    | `getSlotsInRdf()` |  `rdf_slot_names()` |
    | `makeAllScenNames()` | `rw_scen_gen_names()` |
    | `rdfSlotToMatrix()` |  `rdf_get_slot()` |
    | `sumMonth2Annual()` | `rwslot_annual_sum()` |
    | `getTimeSpan()` | `rdf_get_timespan()` |
    | `getWYFromYearmon()` | `ym_get_wateryear()` |
    | `getMaxAnnValue()` | `rwslot_annual_max()` | 
    | `getMinAnnValue()` | `rwslot_annual_min()` | 
    | `flowWeightedAvgAnnConc()` | `rwslot_fwaac()` |

    - For `flowWeightedAvgAnnConc()`, because it is rarely used it was converted to an internal function: `trace_fwaac()`. Then created `rwslot_fwaac()` that is exported and follows the input/output format of `rwslot_annual_min()` and `rwslot_annual_max()`. 
* Added the `rwdataplyr-workflow` vignette. Browse with `vignette("rwdataplyr-workflow", package = "RWDataPlyr")`. 

## Minor improvements

* For monthly data, `getDataForAllScens()` will use full month name in the `Month` column. This change could break existing code if there are checks for particular months. (#20)
* Thresholds < 1 will now work for aggregation methods that compute <=/>= for `slot_agg_list` and `getDataForAllScens()`. (#14)
* Removed dependency on reshape2 and converted to use dplyr where appropriate. 
* Got rid of `read.rdf()`'s original implementation. Now `read.rdf2()` (the faster implementation) is named `read.rdf()` and `read.rdf2()` is deprecated. (#63)
    - `read.rdf()` now works with rdf files that contain scalar slots (#52)
    - `read_rdf()` is added as an alias.

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
