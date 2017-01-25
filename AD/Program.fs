module Main

open DiffSharp.AD.Float64

let values =
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
    ]

let pastSalesIndex = 0
let pastAccumulatedRetainedEarningsIndex = 1
let ``past cash and marketable securitiesIndex`` = 2
let pastNetFixedAssetsIndex = 3
let pastFixedAssetsDepreciationIndex = 4
let ``past debt index`` = 5
let pastStockIndex = 6

let ``sales growth index`` = 7
let ``current assets/Sales index`` = 8
let ``current liabilities/Sales index`` = 9
let ``net fixed assets/Sales index`` = 10
let ``costs of goods sold/Sales index`` = 11
let ``depreciation rate index`` = 12
let ``interest rate on debt index`` = 13
let ``interest paid on cash and marketable securities index`` = 14
let ``tax rate index`` = 15
let ``dividend payout ratio index`` = 16

let sales
    (values:DV) =
    let pastSales = values.[pastSalesIndex]
    let salesGrowth = values.[``sales growth index``]
    (1 + salesGrowth) * pastSales

let netFixedAssets
    (values:DV) =
    sales values * values.[``net fixed assets/Sales index``]

let depreciation
    (values:DV)
    =
    let pastNetFixedAssets = values.[pastNetFixedAssetsIndex]
    let pastFixedAssetsDepreciation = values.[pastFixedAssetsDepreciationIndex]
    let depreciationRate = values.[``depreciation rate index``]

    let pastFixedAssetsAtCost = pastNetFixedAssets - pastFixedAssetsDepreciation

    let netFixedAssets = netFixedAssets values
    let fixedAssetsAtCost' = netFixedAssets - pastFixedAssetsDepreciation

    let factori = depreciationRate / 2.
    (*
    let factor : float =
        (Seq.ofList [1. .. 5.])
        |> Seq.map (fun i -> factori ** i)
        |> Seq.sum
    *)
    let factor = factori ** 1. + factori ** 2. + factori ** 3. + factori ** 4. + factori ** 5.
    factor * (- fixedAssetsAtCost' - pastFixedAssetsAtCost)

let fixedAssetsDepreciation
    (values:DV) =
    let pastFixedAssetsDepreciation = values.[pastFixedAssetsDepreciationIndex]
    let depreciation = depreciation values
    pastFixedAssetsDepreciation + depreciation

let fixedAssetsAtCost
    (values:DV) =
    netFixedAssets values - fixedAssetsDepreciation values

let costsOfGoodsSold
    (values:DV) =
    let ``costs of goods sold/Sales`` = values.[``costs of goods sold/Sales index``]

    let sales = sales values
    -sales * ``costs of goods sold/Sales``

let debt (values:DV) = values.[``past debt index``]

let interestPaymentOnDebt (values:DV) =
    let ``interest rate on debt`` = values.[``interest rate on debt index``]
    let pastDebt = values.[``past debt index``]
    let debt = debt values
    -``interest rate on debt`` * (pastDebt + debt) / 2.

let currentLiabilities (values:DV) =
    let ``current liabilities/Sales`` = values.[``current liabilities/Sales index``]
    let sales = sales values
    sales * ``current liabilities/Sales``

let stock (values:DV) =
    values.[pastStockIndex]

let currentAssets (values:DV) =
    let sales = sales values
    let ``current assets/Sales`` = values.[``current assets/Sales index``]
    sales * ``current assets/Sales``

let profitBeforeTax
    (values:DV)
    =
    let pastAccumulatedRetainedEarnings = values.[pastAccumulatedRetainedEarningsIndex]
    let ``past cash and marketable securities`` = values.[``past cash and marketable securitiesIndex``]
    let interestPaidOnCashAndMarketableSecurities = values.[``interest paid on cash and marketable securities index``]
    let dividendPayoutRatio = values.[``dividend payout ratio index``]
    let taxRate = values.[``tax rate index``]

    let netFixedAssets = netFixedAssets values
    let currentAssets = currentAssets values
    let stock = stock values
    let currentLiabilities = currentLiabilities values
    let debt = debt values
    let interestPaymentsOnDebt = interestPaymentOnDebt values
    let costsOfGoodsSold = costsOfGoodsSold values
    let sales = sales values

    let depreciation = depreciation values

    let ``profit before tax - interest earned on cash and marketable securities`` = sales + costsOfGoodsSold + interestPaymentsOnDebt + depreciation
    let ``weigthed  past interest earned on cash and marketable securities`` =
            ``past cash and marketable securities``
            * interestPaidOnCashAndMarketableSecurities
            / 2.
    let ``cash and marketable securities`` = currentLiabilities + debt + stock + pastAccumulatedRetainedEarnings - currentAssets - netFixedAssets
    let ``weigthed interest earned on cash and marketable securities`` =
            ``cash and marketable securities``
            * interestPaidOnCashAndMarketableSecurities
            / 2.
    let ``interest earned on cash and marketable securities`` = 
            ``weigthed  past interest earned on cash and marketable securities``
          + ``weigthed interest earned on cash and marketable securities``

    let factori = interestPaidOnCashAndMarketableSecurities * (1. - dividendPayoutRatio)*(1. - taxRate)/2.
    (*
    let factor : float =
        (Seq.ofList [0. .. 5.])
        |> Seq.map (fun i -> factori ** i)
        |> Seq.sum
    *)
    let factor = factori ** 0. + factori ** 1. + factori ** 2. + factori ** 3. + factori ** 4. + factori ** 5.
    factor * (``profit before tax - interest earned on cash and marketable securities`` + ``interest earned on cash and marketable securities``)

[<EntryPoint>]
let main argv =
    let pft = profitBeforeTax values
    let gradient = grad profitBeforeTax values
    0