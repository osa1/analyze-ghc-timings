#!/usr/bin/env stack
-- stack --resolver lts-6.0 --install-ghc runghc --package parsec --package wl-pprint

--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Data.Bifunctor          (second)
import           Data.List               (foldl', sortOn)
import           Data.Maybe              (fromMaybe)
import           Text.Printf             (printf)

import           Text.Parsec
import           Text.Parsec.Language    (haskellDef)
import           Text.Parsec.String
import           Text.Parsec.Token       (float, makeTokenParser)

import qualified Data.Map.Strict         as M

import qualified Text.PrettyPrint.Leijen as PP

--------------------------------------------------------------------------------

type Module   = String
type StepName = String

data ModuleTiming = ModuleTiming
  { mtName       :: !Module
  , mtSteps      :: !(M.Map StepName Step)
  , mtTotalTime  :: !Double -- ^ 0 before the post-processing step
  , mtTotalAlloc :: !Double -- ^ 0 before the post-processing step
  } deriving (Show)

initModuleTiming :: Module -> M.Map StepName Step -> ModuleTiming
initModuleTiming mod steps = ModuleTiming mod steps 0 0

data Step = Step
  { stepModName :: !Module
  , stepName    :: !StepName
  , stepTime    :: !Double -- ^ in milliseconds
  , stepAlloc   :: !Double -- ^ in megabytes
  , stepSteps   :: !(M.Map StepName Step) -- ^ sub-steps
  , stepOfTotal :: !Double
      -- ^ how much of the total time is spent on this step? 0 before the
      -- post-processing step.
  } deriving (Show)

data StepBeginning = StepBeginning !Module !StepName
  deriving (Show)

flattenSteps :: ModuleTiming -> [Step]
flattenSteps (ModuleTiming _ steps _ _) = concatMap flattenSteps' (M.elems steps)

flattenSteps' :: Step -> [Step]
flattenSteps' s@(Step _ _ _ _ steps _) = s : concatMap flattenSteps' (M.elems steps)

--------------------------------------------------------------------------------

parseStepBeginning :: String -> Either ParseError StepBeginning
parseStepBeginning = runParser parser () "<stdin>"
  where
    parser :: Parser StepBeginning
    parser = do
      string "*** "
      stepName <- manyTill anyChar (try (string " ["))
      modName  <- manyTill anyChar (char ']')
      char ':'
      eof
      return (StepBeginning modName stepName)

parseStep :: String -> Either ParseError Step
parseStep = runParser parser () "<stdin>"
  where
    notDigit :: Parser ()
    notDigit =
      optionMaybe (lookAhead digit) >>= \case
        Nothing -> anyChar >> return ()
        Just _  -> fail "notDigit had to read digit"

    parser :: Parser Step
    parser = do
      string "!!! "
      stepName <- manyTill anyChar (try (string " ["))
      modName  <- manyTill anyChar (char ']')
      many notDigit
      time <- float (makeTokenParser haskellDef)
      string "milliseconds"
      many notDigit
      alloc <- float (makeTokenParser haskellDef)
      string "megabytes"
      eof
      return (Step modName stepName time alloc M.empty 0.0)

--------------------------------------------------------------------------------

parseLines :: [String] -> M.Map Module ModuleTiming -> [StepName] -> M.Map Module ModuleTiming
parseLines []       ts [] = ts
parseLines []       _  _  = error "@_@" -- TODO: Describe the problem
parseLines (l : ls) ts ss =
    case parseStepBeginning l of
      Right s ->
        let (ts', ss') = beginStep ts ss s
         in parseLines ls ts' ss'
      Left _ ->
        case parseStep l of
          Right step ->
            let (ts', ss') = addStep ts ss step
             in parseLines ls ts' ss'

          Left _ -> parseLines ls ts ss

beginStep :: M.Map Module ModuleTiming -> [StepName] -> StepBeginning
          -> (M.Map Module ModuleTiming, [StepName])
beginStep ts ss (StepBeginning _ stepName) = (ts, stepName : ss)

addStep :: M.Map Module ModuleTiming -> [StepName] -> Step -> (M.Map Module ModuleTiming, [StepName])
addStep ts stk@(s : ss) step@(Step mn sn _ _ _ _)
  | s == sn
  = let
      moduleSteps :: M.Map StepName Step
      moduleSteps = fromMaybe M.empty (fmap mtSteps (M.lookup mn ts))
    in
      ( M.insert mn (initModuleTiming mn (addStep' moduleSteps ss step)) ts
      , ss )
  | otherwise
  = error ("Unexpected step in the stack: " ++ show stk ++ " " ++ show step)

addStep' :: M.Map StepName Step -> [StepName] -> Step -> M.Map StepName Step
addStep' steps [] newStep = M.alter alterStep (stepName newStep) steps
  where
    alterStep :: Maybe Step -> Maybe Step
    alterStep Nothing     = Just newStep
    alterStep (Just step) = Just (joinSteps step newStep)

    joinSteps :: Step -> Step -> Step
    joinSteps s1 s2 =
      -- TODO: Make sure module and step names are the same
      Step (stepModName s1) (stepName s2)
           (stepTime s1 + stepTime s2)
           (stepAlloc s1 + stepAlloc s2)
           (M.unionWith joinSteps (stepSteps s1) (stepSteps s2))
           0.0

addStep' steps (s : ss) newStep =
    let
      subStep         = fromMaybe (Step (stepModName newStep) s 0 0 M.empty 0.0) (M.lookup s steps)
      subStepSubSteps = addStep' (stepSteps subStep) ss newStep
    in
      M.insert s subStep{ stepSteps = subStepSubSteps } steps


postProcessSteps :: ModuleTiming -> ModuleTiming
postProcessSteps mod@(ModuleTiming modName steps _ _) =
    (postProcessSteps' total_time mod) { mtTotalTime = total_time, mtTotalAlloc = total_alloc }
  where
    total_time  = totalTime mod
    total_alloc = totalAlloc mod

    totalTime :: ModuleTiming -> Double
    totalTime (ModuleTiming _ steps _ _) = M.foldl (\total (Step _ _ t _ _ _) -> total + t) 0.0 steps

    totalAlloc :: ModuleTiming -> Double
    totalAlloc (ModuleTiming _ steps _ _) = M.foldl (\total (Step _ _ _ a _ _) -> total + a) 0.0 steps

postProcessSteps' :: Double -> ModuleTiming -> ModuleTiming
postProcessSteps' total (ModuleTiming modName steps _ _) =
    ModuleTiming modName (M.map (postProcessSteps'' total) steps) 0 0

postProcessSteps'' :: Double -> Step -> Step
postProcessSteps'' total s =
    s { stepSteps   = M.map (postProcessSteps'' total) (stepSteps s)
      , stepOfTotal = if total == 0.0 then 0.0 else 100 * (stepTime s) / total
      }

--------------------------------------------------------------------------------

showDouble :: Int -> Double -> String
showDouble p = printf ("%." ++ show p ++ "f")

type StepSorter = [Step] -> [Step]

stepSortTimes = sortOn stepTime
stepSortAlloc = sortOn stepAlloc

renderModuleTiming :: StepSorter -> ModuleTiming -> PP.Doc
renderModuleTiming sorter mt@(ModuleTiming modName steps totalTime totalAlloc) =
    let
      sortedSteps = reverse (sorter (M.elems steps))
    in
      PP.line PP.<$>
      PP.text (centerFill modName '=' 85) PP.<$>
        PP.vcat (concatMap (renderStep sorter 0) sortedSteps) PP.<$>
      PP.text (replicate 85 '-') PP.<$>
      renderLine 0 "Total" totalTime totalAlloc 100.0

renderStep :: StepSorter -> Int -> Step -> [PP.Doc]
renderStep sorter nesting (Step _ step time alloc subSteps p) =
    let
      sortedSteps = reverse (sorter (M.elems subSteps))
    in
      renderLine nesting step time alloc p
        : concatMap (renderStep sorter (nesting + 1)) sortedSteps

renderLine :: Int -> String -> Double -> Double -> Double -> PP.Doc
renderLine nesting name time alloc p =
    PP.hcat [ PP.fill 38 (PP.text (replicate nesting '-') PP.<> PP.text name)
            , PP.text (rightAlignFill (showDouble 2 time) ' ' 10)
            , PP.text " ms"
            , PP.text (rightAlignFill (showDouble 2 alloc) ' ' 10)
            , PP.text " mb"
            , PP.text (rightAlignFill (showDouble 1 p) ' ' 6)
            , PP.text "% of total time"
            ]

centerFill :: String -> Char -> Int -> String
centerFill msg filler len =
    let
      fill_amt   = len - length msg
      left_fill  = fill_amt `div` 2
      right_fill = left_fill + (fill_amt `mod` 2)
    in
      replicate left_fill filler ++ msg ++ replicate right_fill filler

rightAlignFill :: String -> Char -> Int -> String
rightAlignFill msg filler len =
    replicate (len - length msg) filler ++ msg

--------------------------------------------------------------------------------

render :: PP.Doc -> String
render doc = PP.displayS (PP.renderPretty 1.0 100 doc) ""

main :: IO ()
main = do
    ls <- lines <$> getContents
    let modTimings =
          reverse $
            sortOn mtTotalTime $
              map postProcessSteps $
                M.elems (parseLines ls M.empty [])

    -- Render module tables
    forM_ modTimings $ \modTiming ->
      putStrLn (render (renderModuleTiming stepSortTimes modTiming))

    -- Render final stats
    let
      all_steps :: [Step]
      all_steps   = concatMap flattenSteps modTimings

      step_groups :: M.Map StepName Double
      step_groups =
        let
          -- or just use Monoid (Sum a)
          alterMap t1 Nothing   = Just t1
          alterMap t1 (Just t2) = Just (t1 + t2)
        in
          foldl' (\m step -> M.alter (alterMap (stepTime step)) (stepName step) m)
                 M.empty all_steps

      total_time  = sum (M.elems step_groups)

      step_percentages =
        M.map (\stepTotal -> if total_time == 0.0
                               then 0.0
                               else 100.0 * stepTotal / total_time) step_groups

      sorted = reverse (sortOn snd (M.toList step_percentages))

    putStrLn ""

    putStrLn $ render $ PP.vcat $
      PP.text (centerFill "Total" '=' 49) :
        (flip map sorted $ \(stepName, per) ->
           PP.fill 38 (PP.text stepName) PP.<>
             PP.text (rightAlignFill (showDouble 2 per) ' ' 10) PP.<>
               PP.char '%')
