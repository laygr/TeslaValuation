module CompanyValuation

open DiffSharp.AD.Float64

let salesIndex = 0
let accumulatedRetainedEarningsIndex = 1
let ``cash and marketable securitiesIndex`` = 2
let netFixedAssetsIndex = 3
let fixedAssetsDepreciationIndex = 4
let ``debt index`` = 5
let stockIndex = 6

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
let ``wacc index`` = 17
let ``long run growth rate index`` = 18

let sales (pastSales:D) (salesGrowth:D) =
    (1 + salesGrowth) * pastSales

let netFixedAssets (sales:D) (``net fixed assets/Sales``) =
    sales * ``net fixed assets/Sales``

let pastFixedAssetsAtCost (pastNetFixedAssets:D) (pastFixedAssetsDepreciation:D) =
    pastNetFixedAssets - pastFixedAssetsDepreciation

let depreciation
    (pastNetFixedAssets:D) (pastFixedAssetsDepreciation:D)
    (depreciationRate:D) (netFixedAssets:D) (pastFixedAssetsAtCost:D)
    =
    let fixedAssetsAtCost' = netFixedAssets - pastFixedAssetsDepreciation

    let factori = depreciationRate / 2.
    let factor = factori ** 1. + factori ** 2. + factori ** 3. + factori ** 4. + factori ** 5.
    factor * (- fixedAssetsAtCost' - pastFixedAssetsAtCost)

let debt (values:DV) =
    values.[``debt index``]

let stock (values:DV) =
    values.[stockIndex]

let fixedAssetsDepreciation (pastFixedAssetsDepreciation:D) (depreciation:D) =
    pastFixedAssetsDepreciation + depreciation

let fixedAssetsAtCost (netFixedAssets:D) (fixedAssetsDepreciation:D) =
    netFixedAssets - fixedAssetsDepreciation

let costsOfGoodsSold (``costs of goods sold/Sales``:D) (sales:D) =
    -sales * ``costs of goods sold/Sales``

let interestPaymentOnDebt (``interest rate on debt``:D) (pastDebt:D) (debt:D) =
    -``interest rate on debt`` * (pastDebt + debt) / 2.

let currentLiabilities ``current liabilities/Sales`` sales =
    sales * ``current liabilities/Sales``

let currentAssets sales ``current assets/Sales`` =
    sales * ``current assets/Sales``

let accumulatedRetainedEarnings (pastRetainedEarnings:D) (retainedEarnings:D)
    = pastRetainedEarnings + retainedEarnings

let totalLiabilities
    (currentLiabilities:D) (debt:D) (stock:D) (accumulatedRetainedEarnings:D) =
    currentLiabilities + debt + stock + accumulatedRetainedEarnings

let ``cash and marketable securities`` totalLiabilities currentAssets netFixedAssets =
    totalLiabilities - currentAssets - netFixedAssets

let ``interest earned on cash and marketable securities``
        (``past cash and marketable securities``:D)
        (``cash and marketable securities``:D)
        (interestPaidOnCashAndMarketableSecurities:D) =

    let ``weigthed past interest earned on cash and marketable securities`` =
        ``past cash and marketable securities``
        * interestPaidOnCashAndMarketableSecurities
        / 2.

    let ``weigthed interest earned on cash and marketable securities`` =
        ``cash and marketable securities``
        * interestPaidOnCashAndMarketableSecurities
        / 2.

    ``weigthed past interest earned on cash and marketable securities``
    + ``weigthed interest earned on cash and marketable securities``

let profitBeforeTax
    (sales:D) (costsOfGoodsSold:D) (interestPaymentsOnDebt:D) (depreciation:D)
    (``past cash and marketable securities``:D) (interestPaidOnCashAndMarketableSecurities:D)
    (currentLiabilities:D) (debt:D) (stock:D) (pastAccumulatedRetainedEarnings:D) (currentAssets:D) (netFixedAssets:D)
    (dividendPayoutRatio:D) (taxRate:D)
    =

    let ``profit before tax - interest earned on cash and marketable securities`` = sales + costsOfGoodsSold + interestPaymentsOnDebt + depreciation

    let ``weigthed past interest earned on cash and marketable securities`` =
        ``past cash and marketable securities``
        * interestPaidOnCashAndMarketableSecurities
        / 2.

    let ``cash and marketable securities'`` = // cash and marketable securities without current accumulated retained earnings
        currentLiabilities + debt + stock + pastAccumulatedRetainedEarnings - currentAssets - netFixedAssets 

    let ``weigthed interest earned on cash and marketable securities`` =
        ``cash and marketable securities'``
        * interestPaidOnCashAndMarketableSecurities
        / 2.

    let ``interest earned on cash and marketable securities'`` =
        ``weigthed past interest earned on cash and marketable securities``
        + ``weigthed interest earned on cash and marketable securities``

    let factori = interestPaidOnCashAndMarketableSecurities * (1. - dividendPayoutRatio) * (1. - taxRate) / 2.

    let factor = factori ** 0. + factori ** 1. + factori ** 2. + factori ** 3. + factori ** 4. + factori ** 5.
    factor * (``profit before tax - interest earned on cash and marketable securities`` + ``interest earned on cash and marketable securities'``)

let freeCashFlow
    profitAfterTax depreciation deltaCurrentAssets deltaCurrentLiabilities
    deltaFixedAssetsAtCost (taxRate:D) (interestPaymentOnDebt:D) (``interest earned on cash and marketable securities``:D)
    =
    profitAfterTax
    - depreciation                              // add back depreciation
    - deltaCurrentAssets                        // subtract increase in current assets
    + deltaCurrentLiabilities                   // add back increase in current liabilities
    - deltaFixedAssetsAtCost                    // subtract increase in fixed assets at cost
    - (1.-taxRate)*interestPaymentOnDebt        // Add back after-tax interest on debt
    - (1.-taxRate) * ``interest earned on cash and marketable securities``   // Subtract after-tax interest on cash and+A14 mkt. securities

let ``cash flow from operating activities`` = 1
let ``cash flow from investing activities`` = 1
let ``cash flow from financing activities`` = 1

let profitAfterTax
    (profitBeforeTax:D)
    (taxes:D)
    =
    profitBeforeTax + taxes

let retainedEarnings
    (profitAfterTax:D)
    (dividends:D)
    =
    profitAfterTax + dividends

let taxes (profitBeforeTax:D) (taxRate:D) =
    - profitBeforeTax * taxRate

let dividends (profitAfterTax:D) (dividendPayoutRatio:D) =
    - profitAfterTax * dividendPayoutRatio

let nextPeriod (values:DV) =
    let pastSales = values.[salesIndex]
    let pastAccumulatedRetainedEarnings = values.[accumulatedRetainedEarningsIndex]
    let ``past cash and marketable securities`` = values.[``cash and marketable securitiesIndex``]
    let pastNetFixedAssets = values.[netFixedAssetsIndex]
    let pastFixedAssetsDepreciation = values.[fixedAssetsDepreciationIndex]
    let pastDebt = values.[``debt index``]
    let pastStock = values.[stockIndex]

    //rates 
    let salesGrowth = values.[``sales growth index``]
    let ``current assets/Sales`` = values.[``current assets/Sales index``]
    let ``current liabilities/Sales`` = values.[``current liabilities/Sales index``]
    let ``net fixed assets/Sales`` = values.[``net fixed assets/Sales index``]
    let ``costs of goods sold/Sales`` = values.[``costs of goods sold/Sales index``]
    let depreciationRate = values.[``depreciation rate index``]
    let ``interest rate on debt`` = values.[``interest rate on debt index``]
    let interestPaidOnCashAndMarketableSecurities = values.[``interest paid on cash and marketable securities index``]
    let taxRate = values.[``tax rate index``]
    let dividendPayoutRatio = values.[``dividend payout ratio index``]

    //past values
    let pastFixedAssetsAtCost = fixedAssetsAtCost pastNetFixedAssets pastFixedAssetsDepreciation
    let pastCurrentAssets = currentAssets pastSales ``current assets/Sales``
    let pastCurrentLiabilities = currentLiabilities ``current liabilities/Sales`` pastSales

    let sales = sales pastSales salesGrowth
    let netFixedAssets = netFixedAssets sales ``net fixed assets/Sales``
    let currentAssets = currentAssets sales ``current assets/Sales``
    let stock = stock values
    let currentLiabilities = currentLiabilities ``current liabilities/Sales`` sales
    let debt = debt values
    let interestPaymentsOnDebt = interestPaymentOnDebt ``interest rate on debt`` pastDebt debt
    let costsOfGoodsSold = costsOfGoodsSold ``costs of goods sold/Sales`` sales
    let depreciation = depreciation pastNetFixedAssets pastFixedAssetsDepreciation depreciationRate netFixedAssets pastFixedAssetsAtCost

    let profitBeforeTax =
        profitBeforeTax
            sales costsOfGoodsSold interestPaymentsOnDebt depreciation
            ``past cash and marketable securities`` interestPaidOnCashAndMarketableSecurities
            currentLiabilities debt stock pastAccumulatedRetainedEarnings currentAssets netFixedAssets
            dividendPayoutRatio taxRate
    
    
    let fixedAssetsDepreciation = fixedAssetsDepreciation pastFixedAssetsDepreciation depreciation
    let fixedAssetsAtCost = fixedAssetsAtCost netFixedAssets fixedAssetsDepreciation
    let interestPaymentOnDebt = interestPaymentOnDebt ``interest rate on debt`` pastDebt debt:D

    let taxes = taxes profitBeforeTax taxRate
    let profitAfterTax = profitAfterTax profitBeforeTax taxes
    let dividends = dividends profitAfterTax dividendPayoutRatio
    let retainedEarnings = retainedEarnings profitAfterTax dividends
    let accumulatedRetainedEarnings = accumulatedRetainedEarnings pastAccumulatedRetainedEarnings retainedEarnings
    let totalLiabilities = totalLiabilities currentLiabilities debt stock accumulatedRetainedEarnings
    let cashAndMarketableSecurities = ``cash and marketable securities`` totalLiabilities currentAssets netFixedAssets

    let interestEarnedOnCashAndMarketableSecurities =
        ``interest earned on cash and marketable securities`` ``past cash and marketable securities`` cashAndMarketableSecurities interestPaidOnCashAndMarketableSecurities

    let deltaCurrentAssets = currentAssets - pastCurrentAssets
    let deltaCurrentLiabilities = currentLiabilities - pastCurrentLiabilities
    let deltaFixedAssetsAtCost = fixedAssetsAtCost - pastFixedAssetsAtCost
    let freeCashFlow =
        freeCashFlow profitAfterTax depreciation deltaCurrentAssets deltaCurrentLiabilities
                     deltaFixedAssetsAtCost taxRate interestPaymentOnDebt interestEarnedOnCashAndMarketableSecurities
    (toDV [
        // company state:
        sales
        accumulatedRetainedEarnings
        cashAndMarketableSecurities
        netFixedAssets
        fixedAssetsDepreciation
        debt
        stock
        // rates:
        salesGrowth
        ``current assets/Sales``
        ``current liabilities/Sales``
        ``net fixed assets/Sales``
        ``costs of goods sold/Sales``
        depreciationRate
        ``interest rate on debt``
        interestPaidOnCashAndMarketableSecurities
        taxRate
        dividendPayoutRatio
        values.[``wacc index``]
        values.[``long run growth rate index``]
    ]), freeCashFlow

let enterpriseValue1Years (values0:DV) =
    let wacc = values0.[``wacc index``]
    let longRunGrowthRate = values0.[``long run growth rate index``]

    let values1,fcf1 = nextPeriod values0
    
    let w = 1 + wacc
    let terminalValue = (1 + longRunGrowthRate) / (wacc - longRunGrowthRate) * fcf1

    (fcf1 + terminalValue) / w**1

let enterpriseValue2Years (values0:DV) =
    let wacc = values0.[``wacc index``]
    let longRunGrowthRate = values0.[``long run growth rate index``]

    let values1,fcf1 = nextPeriod values0
    let values2,fcf2 = nextPeriod values1
    
    let w = 1 + wacc
    let terminalValue = (1 + longRunGrowthRate) / (wacc - longRunGrowthRate) * fcf2

    fcf1 / w**1
    + (fcf2 + terminalValue) / w**2

let enterpriseValue3Years (values0:DV) =
    let wacc = values0.[``wacc index``]
    let longRunGrowthRate = values0.[``long run growth rate index``]

    let values1,fcf1 = nextPeriod values0
    let values2,fcf2 = nextPeriod values1
    let values3,fcf3 = nextPeriod values2
    
    let w = 1 + wacc
    let terminalValue = (1 + longRunGrowthRate) / (wacc - longRunGrowthRate) * fcf3

    fcf1 / w**1
    + fcf2 / w**2
    + (fcf3 + terminalValue) / w**3

let enterpriseValue4Years (values0:DV) =
    let wacc = values0.[``wacc index``]
    let longRunGrowthRate = values0.[``long run growth rate index``]

    let values1,fcf1 = nextPeriod values0
    let values2,fcf2 = nextPeriod values1
    let values3,fcf3 = nextPeriod values2
    let values4,fcf4 = nextPeriod values3
    
    let w = 1 + wacc
    let terminalValue = (1 + longRunGrowthRate) / (wacc - longRunGrowthRate) * fcf4

    fcf1 / w**1
    + fcf2 / w**2
    + fcf3 / w**3
    + (fcf4 + terminalValue) / w**4

let enterpriseValue5Years (values0:DV) =
    let wacc = values0.[``wacc index``]
    let longRunGrowthRate = values0.[``long run growth rate index``]

    let values1,fcf1 = nextPeriod values0
    let values2,fcf2 = nextPeriod values1
    let values3,fcf3 = nextPeriod values2
    let values4,fcf4 = nextPeriod values3
    let values5,fcf5 = nextPeriod values4
    
    let w = 1 + wacc
    let terminalValue = (1 + longRunGrowthRate) / (wacc - longRunGrowthRate) * fcf5

    fcf1 / w**1
    + fcf2 / w**2
    + fcf3 / w**3
    + fcf4 / w**4
    + (fcf5 + terminalValue) / w**5

let stockPrice enterpriseValue stocks (values0:DV) =
    let ``cash and marketable securities`` = values0.[``cash and marketable securitiesIndex``]
    let debt = values0.[``debt index``]
    let yearZeroNetDebt = ``cash and marketable securities`` - debt
    let equityValue = enterpriseValue + yearZeroNetDebt
    equityValue / stocks

type gradientsCSVType = FSharp.Data.CsvProvider<Schema = "Variable (string), year 1 (float), year 2 (float), year 3 (float), year 4 (float), year 5 (float)", HasHeaders = false>

let csvGradients (values0:DV) =
    let gradientToFloatArray (gradient:DV) = gradient.ToArray() |> Array.map (fun d -> float d)
    let y1g = grad enterpriseValue1Years values0 |> gradientToFloatArray
    let y2g = grad enterpriseValue2Years values0 |> gradientToFloatArray
    let y3g = grad enterpriseValue3Years values0 |> gradientToFloatArray
    let y4g = grad enterpriseValue4Years values0 |> gradientToFloatArray
    let y5g = grad enterpriseValue5Years values0 |> gradientToFloatArray

    let rowForVariable variableName variableIndex =
        gradientsCSVType.Row(variableName, y1g.[variableIndex], y2g.[variableIndex], y3g.[variableIndex], y4g.[variableIndex], y5g.[variableIndex])

    let csv =
        new gradientsCSVType(
            [
                rowForVariable "Sales growth" ``sales growth index``
                rowForVariable "Current assets/Sales" ``current assets/Sales index``
                rowForVariable "Current liabilities/Sales" ``current liabilities/Sales index``
                rowForVariable "Net fixed assets/Sales" ``net fixed assets/Sales index``
                rowForVariable "Costs of goods sold/Sales index" ``costs of goods sold/Sales index``
                rowForVariable "Depreciation rate" ``depreciation rate index``
                rowForVariable "Interest rate on debt" ``interest rate on debt index``
                rowForVariable "Interest paid on cash and marketable securities" ``interest paid on cash and marketable securities index``
                rowForVariable "Tax rate" ``tax rate index``
                rowForVariable "Dividend payout ratio" ``dividend payout ratio index``
                rowForVariable "WACC" ``wacc index``
                rowForVariable "Long run growth rate" ``long run growth rate index``
            ])
    csv.Save("Gradients.csv")

open MathNet.Numerics.Statistics
open FSharp.Charting
let simulation (values0:DV) =
    let normal = MathNet.Numerics.Distributions.Normal(0., 0.02)

    let addTo (vector:float[]) index quantity =
        vector.[index] <- vector.[index] + quantity

    let values () =
        let newValues = (values0.Copy()).ToArray() |> Array.map float
        for i in [``sales growth index``..``long run growth rate index``] do
            addTo newValues i (normal.Sample())
        toDV newValues

    let equityValues = [| for _ in 1 .. 1000000 -> enterpriseValue5Years (values()) |> float |]

    let ds = MathNet.Numerics.Statistics.DescriptiveStatistics(equityValues, true)
    printfn "mean - %A\nstdev - %A\nskewness - %A\nkurtosis - %A" ds.Mean ds.StandardDeviation ds.Skewness ds.Kurtosis

    Chart.Histogram(equityValues,LowerBound=equityValues.Minimum(),UpperBound=equityValues.Maximum(),Intervals=40.)
    |> Chart.Show
    