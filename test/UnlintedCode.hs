module UnlintedCode where

-- This has a HLint issue but we'll bypass the hook
badCode :: [Int] -> [Int]
badCode xs = concat (map (\x -> [x]) xs)
