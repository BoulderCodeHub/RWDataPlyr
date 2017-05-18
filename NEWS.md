# RWDataPlyr 0.4.1.1.9999

* Added `WYMaxLTE` as a valid aggregation method
* Added a `NEWS.md` file to track changes to the package.
* Better error messages in `createSlotAggList()` (#45)
* Improved tests for `createSlotAggList()`
* Added tests for `RWDataPlyr:::processSlots()`
* Modified all aggregation methods that check if the data are <, <=, >, or >= a threshold. Previously, these aggregation methods returned either 0 (FALSE) or 100 (TRUE), now they return 1 for TRUE. This will allows us to keep data in its original state before plotting with  `scales::percent`, and keep us from having to multiply/divide by 100 if we compute other averages outside of RWDataPlyr. **However, this will cause any plotting code that is expecting percentages between 0 and 100 instad of between 0 and 1 to break (or at least look weird).** (#53)
* Improved the error message if an invalid aggregation method is passed to `processSlots` (#42)
    * Also, now check for invalid aggregation methods in `createSlotAggList`, so it will not wait until `processSlots` is called to throw the error. 
* ensured that `getDataForAllScens()` (and interal function `processSlots()`) work with rdf files that only include one trace of data (#40)
    * improved tests for this case
* new function: `makeAllScenNames()`
