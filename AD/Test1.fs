module Test1
open CompanyValuation
open DiffSharp.AD.Float64

let values0 =
    toDV [
        1000.   // past sales                               0
        150.    //pastAccumulatedRetainedEarnings           1
        80.     //``past cash and marketable securities``   2
        770.    // pastNetFixedAssets                       3
        -300.   // pastFixedAssetsDepreciation              4
        320.    // past debt                                5
        450.    // past stock                               6

        0.1     // sales growth                             7
        0.15    // current assets/Sales                     8
        0.08    // current liabilities/Sales                9
        0.77    // net fixed assets/Sales                   10
        0.5     // costs of goods sold/Sales                11
        0.1     // depreciation rate                        12
        0.1     // interest rate on debt                    13
        0.08    // interest paid on cash and marketable securities   14
        0.4     // tax rate                                 15
        0.4     // dividend payout ratio                    16
        0.2     // wacc                                     17
        0.05     // long run growth rate                     18
    ]

[<EntryPoint>]
let main argv =
    // csvGradients values0
    simulation values0
    0