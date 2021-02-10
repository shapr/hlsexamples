module Examples where

-- Note: The different options here are exposed as "code actions" and "code lenses" via the language server protocol.
-- If you have trouble applying some of these examples make sure your editor is configured to use both functionalities
-- and make yourself familiar with how to use them. (e.g. keybindings).

import Data.List
import Test.QuickCheck

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
triple l = l ++ l ++ l

-- moving cursor to the line above should show a lens
-- add signature: triple :: [a] -> [a]

-- uncomment the instance line below, move cursor to the end, should see hls-class-plugin code lens:
-- "Add placeholders for 'pure', '<*>' "
-- instance Applicative Foo

doExamples :: [Char]
doExamples = "Examples"

-- tactics plugin demo

-- uncomment the two lines below, click on the underscore. click on "Introduce lambda"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = _

-- uncomment the lines, click on the type hole, then "Case split on ma"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = (\ ma famb -> _)

-- this time, click on the first type hole, "attempt to fill hole"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = (\ma famb ->  (case ma of
--    Nothing -> _
--    (Just a) -> _))

-- now click on the type hole
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = (\ma famb ->  (case ma of
--    Nothing -> Nothing
--    (Just a) -> _))

-- now replace the type hole with "famb _"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = (\ma famb ->  (case ma of
--    Nothing -> Nothing
--    (Just a) -> _))

-- attempt to fill hole, and the function is done!
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = (\ma famb ->  (case ma of
--    Nothing -> Nothing
--    (Just a) -> (famb _)))

-- this is the final form! (though you could "Apply all hints" to get rid of too many parentheses!)
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = (\ma famb ->  (case ma of
--    Nothing -> Nothing
--    (Just a) -> (famb a)))

data Foo a = Foo a | Bar

-- tactics can generate an Arbitrary instance for you
-- uncomment the two lines below, select the type hole, then "Attempt to fill hole" the hole fill is show below
-- instance (Arbitrary a) => Arbitrary (Foo a) where
--   arbitrary = _

-- instance (Arbitrary a) => Arbitrary (Foo a) where
--   arbitrary = (let terminal = [Foo <$> arbitrary, pure Bar]
--                in
--                  sized
--                    $ (\ n
--                         -> case n <= 1 of
--                              True -> oneof terminal
--                              False -> oneof $ ([] <> terminal)))
