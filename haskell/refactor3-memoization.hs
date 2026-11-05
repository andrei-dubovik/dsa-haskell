--  Copyright (c) 2026 Andrey Dubovik <andrei at dubovik dot eu>

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.Memo
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON, throwDecodeStrict)
import Data.Array.Unboxed
import Data.ByteString qualified as BS
import Data.List
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics
import Statistics.Quantile qualified as Quantile
import Text.Printf

-- Tags for distinguishing caches of combined series
data CacheTag =
  Debt
  | Gdp
  | PrimarySurplus
  | PrimarySurplusGdp
  | Rate
  | Rho
  | ShareMaturing
  deriving (Eq, Ord, Show)

-- Simulation scenario
data Scenario = Low | Medium | High deriving (Eq, Ord, Show)

instance PrintfArg Scenario where
  formatArg x = formatArg (show x)

-- Period index, a year in our case
type Index = Int

-- Account value, in million euros or as a ratio
type Value = Double

-- Data for historical series
type Table = UArray Index Value

-- A dataset with all historical data
data DataSet = DataSet {
  debt           :: Table,
  debtMaturing   :: Table,
  gdp            :: Table,
  primarySurplus :: Table,
  rate           :: Table,
  rho            :: Table
} deriving Show

-- A cached dataset context
type DS = MemoT (CacheTag, Scenario, Index) Value (Reader DataSet)

-- Mabye values inside a dataset context
type DSM = MaybeT (Reader DataSet)

-- Algebraic operations over values in a dataset context
instance Num (DS Value) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance Fractional (DS Value) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational

instance Num (DSM Value) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger

instance Fractional (DSM Value) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational

-- A function signature for any historical series
type HistoricalSeries = Index -> DSM Value

-- A function signature for any simulated series
type ForecastSeries = Scenario -> Index -> DS Value

-- A function signature for a specific forecast
-- (This is a forecast that can return Nothing for years where it is not defined.)
type SpecificForecast = Scenario -> Index -> DSM Value

-- A function signature for combined historical and simulated series
type CombinedSeries = Scenario -> Index -> DS Value

-- A function signature for accounting identities
type AccountingIdentity = Scenario -> Index -> DS Value

-- Raw JSON value
data RawValue = RawValue {
  year :: Int,
  value :: Double
} deriving (Generic, Show)

instance FromJSON RawValue

-- Raw JSON series
data RawSeries = RawSeries {
  values :: [RawValue]
} deriving (Generic, Show)

instance FromJSON RawSeries

-- Construct a table from JSON data
valuesArray :: [RawValue] -> Table
valuesArray values = array (tmin, tmax) pairs
  where tmin = minimum years
        tmax = maximum years
        years = [v.year | v <- values]
        pairs = [(v.year, v.value) | v <- values]

-- Load a table from a JSON file
loadTable :: FilePath -> IO (Table)
loadTable path = do
  content <- BS.readFile path
  rawSeries <- throwDecodeStrict @RawSeries content
  pure $ valuesArray rawSeries.values

-- Load all historical data
loadDataset :: IO (DataSet)
loadDataset = do
  debt           <- loadTable "data/debt.json"
  debtMaturing   <- loadTable "data/debt-maturing.json"
  gdp            <- loadTable "data/gdp.json"
  primarySurplus <- loadTable "data/primary-surplus.json"
  rate           <- loadTable "data/rate-newdebt.json"
  rho            <- loadTable "data/rate-implicit.json"
  pure $ DataSet { debt, debtMaturing, gdp, primarySurplus, rate, rho }

-- Get a table element or Nothing if out of bounds on the right
indexMaybe :: (Ix i, IArray UArray e) => UArray i e -> i -> Maybe e
indexMaybe a i
  | i < fst (bounds a) = error "left out-of-bounds"
  | i <= snd (bounds a) = Just (a ! i)
  | otherwise = Nothing

-- Construct a function representing historical series
fromTable :: (DataSet -> Table) -> HistoricalSeries
fromTable getter t = MaybeT $ do
  table <- asks getter
  pure $ indexMaybe table t

-- Concatenate historical and simulated series
withHistoryNocache :: HistoricalSeries -> ForecastSeries -> CombinedSeries
withHistoryNocache h f s t = do
  x <- lift $ runMaybeT $ h t
  case x of
    Just x -> pure x
    Nothing -> f s t

withHistory :: CacheTag -> HistoricalSeries -> ForecastSeries -> CombinedSeries
withHistory tag h f s t = memo (\(tag, s, t) -> withHistoryNocache h f s t) (tag, s, t)

-- Stack forecast series (this is a variation of withHistory, in a sense)
stackForecasts :: SpecificForecast -> ForecastSeries -> ForecastSeries
stackForecasts f1 f2 s t = do
  x <- lift $ runMaybeT $ f1 s t
  case x of
    Just x -> pure x
    Nothing -> f2 s t


-- Statistics functions

-- Construct a vector backwards (for early short-circuiting on Nothing)
generateRM :: (Monad m, Vector.Unbox a) => Int -> (Int -> m a) -> m (Vector a)
generateRM n f = Vector.reverse <$> Vector.generateM n (f . (n0 -))
  where n0 = n - 1

-- Convert historical series to a Vector
toVectorHs :: HistoricalSeries -> Index -> Index -> DSM (Vector Value)
toVectorHs h start end = generateRM (end - start + 1) (h . (+ start))

-- Compute quantiles over historical series
quantile :: HistoricalSeries -> Index -> Index -> Int -> Int -> DSM Value
quantile h start end k q = fmap f v
  where f = Quantile.quantile Quantile.s k q
        v = toVectorHs h start end


-- Outstanding debt

debtHs :: HistoricalSeries
debtHs = fromTable (.debt)

debtFc :: ForecastSeries
debtFc s t = debt s (t-1) - surplus s (t-1)

debt :: CombinedSeries
debt = withHistory Debt debtHs debtFc


-- Maturing debt

debtMaturingHs :: HistoricalSeries
debtMaturingHs = fromTable (.debtMaturing)

shareMaturingHs :: HistoricalSeries
shareMaturingHs t = debtMaturingHs t / debtHs t

shareMaturingFc :: ForecastSeries
shareMaturingFc s t = shareMaturing s (t-1)

shareMaturing :: CombinedSeries
shareMaturing = withHistory ShareMaturing shareMaturingHs shareMaturingFc


-- Interest rate on new debt

rateHs :: HistoricalSeries
rateHs = fromTable (.rate)

rateFc :: ForecastSeries
rateFc s t = undefined

rate :: CombinedSeries
rate = withHistory Rate rateHs rateFc


-- GDP

gdpHs :: HistoricalSeries
gdpHs = fromTable (.gdp)

gdpFc :: ForecastSeries
gdpFc s t = gdp s (t-1) * nominalGrowth
  where nominalGrowth = 1.03

gdp :: CombinedSeries
gdp = withHistory Gdp gdpHs gdpFc


-- Primary budget surplus (nominal)

primarySurplusHs :: HistoricalSeries
primarySurplusHs = fromTable (.primarySurplus)

primarySurplusFc :: ForecastSeries
primarySurplusFc s t = primarySurplusGdp s t * gdp s t

primarySurplus :: CombinedSeries
primarySurplus = withHistory PrimarySurplus primarySurplusHs primarySurplusFc


-- Primary budget surplus (share of GDP)

primarySurplusGdpHs :: HistoricalSeries
primarySurplusGdpHs t = primarySurplusHs t / gdpHs t

primarySurplusGdpFc1 :: SpecificForecast
primarySurplusGdpFc1 s t = quantile primarySurplusGdpHs (t-window) (t-1) k 10
  where window = 30
        k = case s of
          Low -> 1
          Medium -> 5
          High -> 9

primarySurplusGdpFc :: ForecastSeries
primarySurplusGdpFc s t = primarySurplusGdp s (t-1)

primarySurplusGdp :: CombinedSeries
primarySurplusGdp = withHistory
  PrimarySurplusGdp
  primarySurplusGdpHs
  (stackForecasts primarySurplusGdpFc1 primarySurplusGdpFc)


-- Budget surplus

surplus :: AccountingIdentity
surplus s t = primarySurplus s t - debt s t * rho s t


-- Implicit interest rate

rhoHs :: HistoricalSeries
rhoHs = fromTable (.rho)

rhoFc :: ForecastSeries
rhoFc s t = (interestDeferred + interestNew)/debtTotal
  where interestDeferred = debtDeferred * rho s (t-1)
        interestNew = (debtTotal - debtDeferred) * rate s t
        debtDeferred = (1 - shareMaturing s (t-1)) * debt s (t-1)
        debtTotal = debt s t

rho :: CombinedSeries
rho = withHistory Rho rhoHs rhoFc


-- Auxiliary series

-- End-of-year debt to GDP (aligns with official reporting)
debtGdp :: AccountingIdentity
debtGdp s t = debt s (t+1) / gdp s t


-- Reporting

-- A row in a summary table
data SummaryRow = SummaryRow {
  year   :: Index,
  low    :: Value,
  medium :: Value,
  high   :: Value
}

instance Show SummaryRow where
  show row = printf "%4d%12.1f%12.1f%12.1f"
    row.year
    (100 * row.low)
    (100 * row.medium)
    (100 * row.high)

-- A summary table
newtype Summary = Summary [SummaryRow]

instance Show Summary where
  show (Summary rows) =
    printf "%4s%12s%12s%12s\n" "Year" Low Medium High
    ++ intercalate "\n" (map show rows)

-- Compute a summary table
summary :: Index -> Index -> DS Summary
summary start end = fmap Summary $ traverse row [start..end] where
  row t =
    SummaryRow t
    <$> debtGdp Low t
    <*> debtGdp Medium t
    <*> debtGdp High t


-- Example usage
main :: IO ()
main = do
  ds <- loadDataset
  putStrLn $ show $ (runReader . startEvalMemoT) (summary 2025 2035) ds
