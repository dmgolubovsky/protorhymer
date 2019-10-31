{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import WithCli
import Paths_protorhymer

import qualified System.IO.Strict as SIO
import System.IO
import System.Process
import System.Environment
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Word
import Data.Bool
import Data.List
import Data.List.Split
import Data.Hash
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Identity
import Control.Exception
import GHC.Conc
import qualified Control.Concurrent.Thread.Group as G
import qualified Control.Concurrent.Thread as T

import qualified Data.Map as M
import qualified Data.Set as S

-- Represent an IPA token

data IPA = IPA [Word32] deriving (Eq, Ord, Show)
 
toIPA :: String -> IPA

toIPA s = IPA $ map (fromIntegral . ord) s

fromIPA :: IPA -> String

fromIPA (IPA i) = map (chr . fromIntegral) i

-- Run espeak to extract IPA from the utterance with given voice

getIPA :: String -> String -> IO [IPA]

getIPA voice utter = do
  let esp = proc "espeak" [
        "-q",
        "--ipa",
        "--sep=_",
        "-v", voice,
        "--load",
        utter
        ] 
  (_, mbhout, _, p) <- createProcess $ esp { std_out = CreatePipe }
  case mbhout of
    Nothing -> error "Cannot get espeak handle"
    Just hout -> do
      s <- SIO.hGetContents hout >>= return . filter (`notElem` [' ', '\n'])
      waitForProcess p
      let splits = splitOneOf "_" s
      return $ map toIPA splits
  

data IPAWord = IPAWord {
  word :: String                     -- word proper
 ,ipa :: [IPA]                       -- original transcription
 ,ipa' :: [IPA]                      -- stress and long marks removed
 ,rfd :: String                      -- refined IPA (expressed in chars)
 ,rfdpat :: String                   -- vowel-consonant pattern for refined IPA
 ,rhyfd :: String                    -- refined IPA for rhyming (consonants condensed)
 ,numvow :: Int                      -- number of vowels
 ,stress :: Int                      -- stressed vowel position from end
} deriving (Ord, Eq, Show)

-- Initially construct an IPA word with minimal refinement that does not require
-- an attribute map.

ipaword :: String -> [IPA] -> IPAWord

ipaword w i = let i' = filter (not . hollow) i in IPAWord {
  word = w
 ,ipa = i'
 ,ipa' = map clean i'
 ,rfd = ""
 ,rfdpat = ""
 ,rhyfd = ""
 ,numvow = -1
 ,stress = -1
} where clean (IPA is) = IPA $ filter (`notElem` [716, 712, 720]) is
        hollow (IPA []) = True
        hollow _ = False

-- Refine an IPA word using the attribute map. If an uncategorized IPA symbol
-- occurs it is considered a consonant and goes into the refined IPA by taking
-- the first symbol of itself.

iparefine :: Bool -> IPAMap -> IPAWord -> IPAWord

iparefine sec mp ipaw = ipaw {
  rfd = rf
 ,rhyfd = map snd rhy
 ,rfdpat = pat
 ,numvow = length $ filter (== 'V') pat
 ,stress = length $ takeWhile (not . stressed . fst) $ vowrev
} where
  stressed (IPA is) = (712 `elem` is) || (sec && (716 `elem` is))
  vowrev = filter ((== 'V') . snd) $ zip (reverse $ ipa ipaw) (reverse pat)
  rf = map (refined . attr) (ipa' ipaw)
  pat = map (bool 'C' 'V' . isVowel . attr) (ipa' ipaw)
  refpat = zip pat rf
  conc [] = []
  conc (('V', c):vcs) = ('V', c):conc vcs
  conc (('C', c):vcs) = ('C', c):(conc $ dropWhile ((== 'C') . fst) vcs)
  rhyz = reverse $ conc refpat
  rhy = case rhyz of
    (('V', c):_) -> ('C', '_'):rhyz
    _ -> rhyz
  attr (IPA []) = error $ "kuku "
  attr i@(IPA is) = case M.lookup i mp of
    n | n == Nothing || n == Just IPAUnCat -> IPAAttr {
                                                isVowel = False
                                               ,refined = chr $ fromIntegral $ head is
                                              }
    Just ia -> ia


data IPAAttr = IPAUnCat | IPAAttr {
  isVowel :: Bool                    -- is this a vowel?
 ,refined :: Char                    -- equivalent character used for approx. rhyming
} deriving (Eq, Show)

type IPAMap = M.Map IPA IPAAttr

-- Build a refined IPA map from given corpus of words and possibly nonempty existing IPA map

mkIPAMap :: [IPAWord] -> IPAMap -> IPAMap

mkIPAMap ws prev = m where
  allipa = nub $ concatMap ipa' ws
  m = t allipa M.empty
  t [] mp = mp
  t (i:is) mp = M.fromList (z i) `M.union` t is mp where
  z (IPA []) = []
  z i = case M.lookup i prev of
    Nothing -> [(i, IPAUnCat)]
    Just attr -> [(i, attr)]

-- Print an IPA map to the given handle
-- a:V:a
-- b:C:b

prtIPAMap :: Handle -> IPAMap -> IO ()

prtIPAMap h mp = w >> hFlush h where
  w = do
    let lst = M.toList mp
    hPutStrLn h "#:Please populate the uncategorized entries"
    mapM_ z lst where
      z (i, a) = l i >> r a
      l i = hPutStr h (fromIPA i) >> hPutStr h ":"
      r a = case a of
        IPAUnCat -> hPutStrLn h ""
        IPAAttr _ _ -> do
          case isVowel a of
            True -> hPutStr h "V"
            False -> hPutStr h "C"
          hPutStrLn h $ ":" ++ [refined a]

-- Split a line into IPA and attributes

l2a :: String -> Maybe (IPA, IPAAttr)

l2a s = x where
  toks = splitOneOf ":" s
  x = case toks of
    ("#":_ ) -> Nothing
    (i:vc:r:_) | i /= [] && r /= [] -> Just (toIPA i, IPAAttr iv rf) where
      iv = vc `elem` ["v", "V"]
      rf = head r
    (i:_) | i /= [] -> Just (toIPA i, IPAUnCat)
    _ -> Nothing

-- Read an IPA map from the given handle

readIPAMap :: Handle -> IO IPAMap

readIPAMap h = do
  ls <- hGetContents h >>= return . lines
  let attrs = map l2a ls
  return $ M.fromList $ catMaybes attrs

-- Words with their IPAs sorted by rhyming patterns grouped by stress position

type RhymeMap = M.Map Int (V.Vector IPAWord)

mkRhymeMap :: [IPAWord] -> RhymeMap

mkRhymeMap ipws = M.fromList z where
  ipws' = filter ((> 0) . length . rhyfd) ipws
  strps = nub $ map stress ipws'
  swrd s = sortOn rhyfd $ filter ((== s) . stress) ipws'
  swrds = map swrd strps
  z = zip strps (map V.fromList swrds)

-- Print rhyme map on the given handle

prtRhymeMap :: Handle -> RhymeMap -> IO ()

prtRhymeMap h rm = w >> hFlush h where
  sts = M.keys rm
  w = mapM_ p sts
  p s = do
    hPutStrLn h $ "Words with stress position at " ++ show s
    let ws = fromMaybe V.empty $ M.lookup s rm
    let prtw w = word w ++ " [" ++ (concatMap fromIPA) (ipa w) ++ "] " ++ reverse (rhyfd w)
    mapM_ (hPutStrLn h . prtw) ws

-- Select words from the corpus for the given stress pattern and seed word.
-- If the seed word is not provided then the pattern will be used on the first
-- iteration. On subsequent iterations the word selected will be used.
-- If the seed word is provided it also goes into the accumulator to become
-- the last word of the line

mkLine :: String -> RhymeMap ->  Maybe IPAWord -> [IPAWord]

mkLine "." _ (Just iw) = [iw]

mkLine pat rm mbseed = mkl pat0 0 (maybeToList mbseed) hash0 where
  hash0 = hash $ pat ++ (fromMaybe pat $ fmap word mbseed)
  pat0 = drop (fromMaybe 0 $ fmap numvow mbseed) $ reverse pat
  ipahash = hash . word
  mkl [] rem acc h | rem == 0 = acc
                   | rem > 0 = (findword 0 (replicate rem 's') h) : acc
  mkl (c:tailpat) need acc h | (not . isUpper) c = mkl tailpat (need + 1) acc h
  mkl (c:tailpat) need acc h | isUpper c = let fw = findword need (c:tailpat) h in
    mkl (drop (numvow fw - need - 1) tailpat) 0 (fw : acc) (ipahash fw)
  findword need tp h = case M.lookup need rm of
    Nothing -> error $ "no words with stress position " ++ show need
    Just x -> let found = x V.! (((fromIntegral $ asWord64 h) `mod` (length x - 1))) in
                  case (numvow found) <= (need + length tp) of
                    True -> found
                    False -> findword need tp (ipahash found)

mkIPAWord :: Bool -> IPAMap -> String -> String -> IO IPAWord

mkIPAWord sec im vc w = do
  ipa <- getIPA vc $ map toLower w
  let ipaw = ipaword w ipa
  return $ iparefine sec im ipaw

-- Range search

rangeSearchM :: (Monad m, Num r, Integral r) => r -> (r, r) -> (r -> m Ordering) -> m (r, r)

rangeSearchM delta (lo, hi) fun | hi - lo <= delta = return (lo, hi)

rangeSearchM delta (lo, hi) fun = do
  let mid = (lo + hi) `div` 2
  res <- fun mid
  case res of
    EQ -> return (mid, mid)
    LT -> rangeSearchM delta (lo, mid) fun
    GT -> rangeSearchM delta (mid, hi) fun

-- Find up to the given number of words rhyming with this word

findRhymes :: RhymeMap -> IPAWord -> Int -> [IPAWord]

findRhymes _ _ 0 = []

findRhymes rm iw n = runIdentity $ do
  let iws = M.lookup (stress iw) rm
  case iws of
    Nothing -> return []
    Just iwds -> do
      (lo, hi) <- rangeSearchM 1 (0, length iwds) $ \mid ->
        return $ compare (rhyfd iw) (rhyfd $ iwds V.! mid)
      let lo' = max 0 (lo - n `div` 2)
          hi' = min (length iwds - 1) (lo' + n - 1)
      return $ V.toList $ V.slice lo' (hi' - lo' + 1) iwds


mapMPar :: (a -> IO b) -> [a] -> IO [b]

mapMPar fun lst = do
  ncr <- getNumProcessors
  case ncr of
    _ | ncr <= 2 -> mapM fun lst
    _ -> do
      setNumCapabilities (ncr + 1)
      let nthr = ncr
      let slices = chunksOf (length lst `div` nthr) lst
      tg <- G.new
      thrs <- forM slices $ \sl -> G.forkOS tg (mapM fun sl)
      G.wait tg
      ipas <- forM thrs $ (T.result =<<) . snd
      return $ concat ipas
        
      


main :: IO ()

main = withCliModified mods main'

main' :: TextFile -> RhyPat -> Options -> IO ()

main' (TextFile file) (RhyPat rhypat) opts = do
  let vc = fromMaybe "default" $ voice opts
  w0 <- readFile file
  let w = filter ((> 3) . length) . nub $ map (filter isAlpha) $ words $ map toLower w0
  ipax <- mapMPar (getIPA vc) w
  let ipaw = zipWith ipaword w ipax
  let icpath = "/espvs/share/prhymer/ipacat.txt"
  mprev <- (openFile "ipacat.txt" ReadMode `catch` 
            \ (e :: IOException) -> openFile icpath ReadMode) >>= readIPAMap
  let mp = mkIPAMap ipaw mprev
      uncat = M.filter (== IPAUnCat) mp
  if length uncat > 0
    then openFile "ipauncat.txt" WriteMode >>= flip prtIPAMap uncat
    else return ()
  if ipadump opts
    then openFile "ipacat.txt" WriteMode >>= flip prtIPAMap mp
    else return ()
  let rfipaw = map (iparefine (second opts) mp) ipaw
  let rm = mkRhymeMap rfipaw
  mbiw <- maybe (return [Nothing]) ( \w -> do
    ipw <- mkIPAWord (second opts) mp vc w
    let rhs = (findRhymes $! rm) ipw (fromMaybe 0 $ rhymes opts)
    let iwrhs = S.toList $ S.fromList (ipw:rhs)
    return $ map Just iwrhs) (endw opts)
  let sm = case mbiw of
                  [Nothing] -> let line0 = mkLine rhypat rm Nothing
                                   lastw = head $ reverse line0
                                   rhms = findRhymes rm lastw (fromMaybe 0 $ rhymes opts)
                                   lines = map (mkLine rhypat rm . Just) rhms
                               in  line0 : lines
                  _ -> map (mkLine rhypat rm) mbiw
  forM sm $ \s -> do
    putStrLn $ concatMap (\w -> word w ++ " ") s
    if (ipa'' opts) then
      putStrLn $ "[" ++ concatMap (\w -> (concatMap fromIPA $ ipa w) ++ " ") s ++ "]"
    else
      return ()
  return ()

data TextFile = TextFile FilePath

instance Argument TextFile where
  argumentType Proxy = "text-file"
  parseArgument f = Just (TextFile f)

instance HasArguments TextFile where
  argumentsParser = atomicArgumentsParser


data RhyPat = RhyPat String

instance Argument RhyPat where
  argumentType Proxy = "rhyme-pattern"
  parseArgument f = Just (RhyPat f)

instance HasArguments RhyPat where
  argumentsParser = atomicArgumentsParser


data Options = Options {
  endw :: Maybe String
 ,voice :: Maybe String
 ,ipa'' :: Bool
 ,rhymes :: Maybe Int
 ,ipadump :: Bool
 ,second :: Bool
} deriving (Show, Generic, HasArguments)

mods :: [Modifier]

mods = [
  AddShortOption "voice" 'v'
 ,AddOptionHelp  "voice" "Espeak voice name to use"
 ,AddShortOption "endw" 'w'
 ,AddOptionHelp  "endw" "Ending word of the line"
 ,AddShortOption "ipa''" 'i'
 ,AddOptionHelp  "ipa''" "Print IPA transcription"
 ,AddShortOption "rhymes" 'r'
 ,AddOptionHelp  "rhymes" "Number of rhymed lines to produce"
 ,AddShortOption "second" 's'
 ,AddOptionHelp  "second" "Treat secondary stresses as primary"
       ]
