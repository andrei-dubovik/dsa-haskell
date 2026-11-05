--  Copyright (c) 2026 Andrey Dubovik <andrei at dubovik dot eu>

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON, throwDecodeStrict)
import Data.Array.Unboxed
import Data.ByteString qualified as BS
import GHC.Generics

-- Simulation scenario
data Scenario = Low | Medium | High deriving Show

-- Period index, a year in our case
type Index = Int

-- Account value, in million euros or as a ratio
type Value = Double

-- Data for historical series
type Table = UArray Index Value

-- A dataset with all historical data
data DataSet = DataSet {
  debt :: Table
} deriving Show

-- A dataset context
type DS = Reader DataSet

-- Mabye values inside a dataset context
type DSM = MaybeT DS

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

-- A function signature for combined historical and simulated series
type CombinedSeries = Scenario -> Index -> DS Value

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
withHistory :: HistoricalSeries -> ForecastSeries -> CombinedSeries
withHistory h f s t = do
  x <- runMaybeT $ h t
  case x of
    Just x -> pure x
    Nothing -> f s t

-- Outstanding debt
debtHs :: HistoricalSeries
debtHs = fromTable (.debt)

debtFc :: ForecastSeries
debtFc s t = debt s (t-1) - surplus s (t-1)

debt :: CombinedSeries
debt = withHistory debtHs debtFc

-- Mock surplus series
surplus :: ForecastSeries
surplus s t = -80_000

-- Example usage
main :: IO ()
main = do
  debtTbl <- loadTable "data/debt.json"
  let ds = DataSet { debt = debtTbl }
  putStrLn $ show $ runReader (debt Medium 2028) ds
