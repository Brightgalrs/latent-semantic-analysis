module LSA
    ( compress
    ) where

import Data.List
import Data.Char
import Numeric.LinearAlgebra
--import Data.Packed.Matrix

title :: String
titles = [ "The Neatest Little Guide to Stock Market Investing"
         , "Investing For Dummies, 4th Edition"
         , "The Little Book of Common Sense Investing: The Only Way to Guarantee Your Fair Share of Stock Market Returns"
         , "The Little Book of Value Investing"
         , "Value Investing: From Graham to Buffett and Beyond"
         , "Rich Dad's Guide to Investing: What the Rich Invest in, That the Poor and the Middle Class Do Not!"
         , "Investing in Real Estate, 5th Edition"
         , "Stock Investing For Dummies"
         , "Rich Dad's Advisors: The ABC's of Real Estate Investing: The Secrets of Finding Hidden Profits Most Investors Miss"
         ]

stopwords :: String
stopwords = [ "and"
            , "edition"
            , "for"
            , "in"
            , "little"
            , "of"
            , "the"
            , "to"
            ]

-- Preps input
-- All letters to lowercase, remove all punctionation, remove stop words
docs :: [[String]]
docs = map (filter (not . (`elem` stopwords))) $
        map words $
          lines $
            filter (\x -> isAlpha x || isSpace x) $
              map toLower (unlines titles)

-- Finds word frequency over all documents
-- Removes words that appear only once, for some reason
tf :: [(String, Int)]
tf = filter (\(_,f) -> f>1) $ map (\l@(x:xs) -> (x,length l)) . group . sort $ concat docs

--Number of times term t is in document d
doc_freq :: Int -> String -> Int
doc_freq d t = length (filter (==t) (docs !! d))

--
mat :: Matrix Double
mat = buildMatrix (length tf) (length docs) ( \(term, doc) ->
          let occurances = fromIntegral $ doc_freq doc $ fst $ tf !! term -- occurance count
              docLength = length $ docs !! doc                            -- words per doc
              numDocs = length docs                                       -- number of docs
              commonness = fromIntegral $ snd $ tf !! term                -- number of docs this word occurs in
          in (occurances / docLength * log (numDocs / commonness))
      )

compress :: Int -> Matrix Double -> Matrix Double
compress k = u_k <> sigma_k <> v_k where
	(u,sigma,v) = full svd mat		                -- get SVD
	sigma_k = (takeColumns k . takeRows k) sigma	-- keep k values of Σ
	u_k = takeColumns k u				                  -- keep k columns of U
	v_k = takeRows k $ trans v			              -- keep k rows of v
