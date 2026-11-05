--  Copyright (c) 2026 Andrey Dubovik <andrei at dubovik dot eu>

-- Simulation scenario
data Scenario = Low | Medium | High deriving Show

-- Period index, a year in our case
type Index = Int

-- Account value, in million euros or as a ratio
type Value = Double

-- A function signature for any simulated series
type ForecastSeries = Scenario -> Index -> Value

-- A function signature for any historical series
type HistoricalSeries = Index -> Maybe Value

-- A function signature for combined historical and simulated series
type CombinedSeries = Scenario -> Index -> Value

-- Concatenate historical and simulated series
withHistory :: HistoricalSeries -> ForecastSeries -> CombinedSeries
withHistory h f s t = case h t of
  Just x -> x
  Nothing -> f s t

-- Debt modelling
debtFc :: ForecastSeries
debtFc s t = debt s (t-1) - surplus s (t-1)

debt :: CombinedSeries
debt = withHistory debtHs debtFc

-- Mock surplus series
surplus :: ForecastSeries
surplus s t = -80_000

-- Mock historical series for debt
debtHs :: HistoricalSeries
debtHs t = if t >= 2027 then Nothing else Just 3_500_000

-- Example usage
main :: IO ()
main = do
  let d = debt Medium 2028
  putStrLn $ show d
