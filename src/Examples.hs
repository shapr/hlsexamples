module Examples where

import Data.List

-- moving cursor to line above should show three lens choices
-- Remove import
-- Remove redundant imports
-- Make all imports explicit

-- these next lines fire two hlint hints, "eta reduce" and "use maximum"
biggest :: (Foldable t, Ord a) => t a -> a
biggest items = foldr1 max items

-- code evaluation code lens: as long as lens mode is on, you'll see "Evaluate..." above the >>> line

-- | Triple a list
-- >>> triple "a"
triple l = l ++ l

-- moving cursor to the line above should show a lens
-- add signature: triple :: [a] -> [a]

data Foo a = Foo a

-- uncomment the instance line below, move cursor to the end, should see hls-class-plugin code lens:
-- "Add placeholders for 'pure', '<*>' "
-- instance Applicative Foo

doExamples :: [Char]
doExamples = "Examples"
