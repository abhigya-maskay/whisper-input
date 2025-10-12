module WithHLintIssues (redundant) where

-- Fixed: using concatMap instead of concat . map, with eta reduction
redundant :: [String] -> String
redundant = concatMap reverse
