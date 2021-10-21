module Examples where

-- Note: The different options here are exposed as "code actions" and "code lenses" via the language server protocol.
-- If you have trouble applying some of these examples make sure your editor is configured to use both functionalities
-- and make yourself familiar with how to use them. (e.g. keybindings).

-- move cursor to the next line, should see 'Remove all redundant imports' , 'Remove import' , 'Make all imports explicit'
import Data.Char
import Data.Text (Text)
import Test.QuickCheck

-- the line below should show 'Generate signature comments' so you can easily write haddocks
biggest :: (Foldable t, Ord a) => t a -> a
biggest items = foldr1 max items

-- the line above will show two hlint hints, "eta reduce" and "use maximum"

-- code evaluation code lens: as long as lens mode is on, you'll see "Evaluate..." on the >>> line below

-- | Triple a list
-- >>> triple "a"
triple l = l ++ l ++ l

-- moving cursor to the line above should show a lens
-- add signature: triple :: [a] -> [a]

doExamples :: [Char]
doExamples = "Examples"

-- call hierarchy
-- fold / unfold
thisthing = biggest $ triple doExamples

-- add missing imports
-- otherthing = groupBy

-- add missing pragmas
-- foo :: Text
-- foo = "foo"

-- wingman / tactics plugin demo

-- uncomment the two lines below, click on the underscore. click on "Attempt to fill hole"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = _

-- this is the result!
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe Nothing _ = Nothing
-- applyMaybe (Just a) f = f a

-- but wait, let's do it step by step!
-- uncomment the lines, click on the type hole, then "Introduce lambda"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe = _

-- now uncomment and "Case split on m_a"
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe m_a f = _wr

-- uncomment and fill the two holes _ws and _wt
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe Nothing f = _ws
-- applyMaybe (Just a) f = _wt

-- steps that will NOT work: uncomment and "Homomorphic case split on m_a" - extra credit, why doesn't that work?
-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe m_a f = _wr

data ADT = One Int | Two String | Three | Four Bool ADT

-- Want to see a cool case split? uncomment and choose "Introduce and destruct term"
-- awesome :: ADT -> Int
-- awesome = _

-- this is the result
-- awesome :: ADT -> Int
-- awesome (One n) = _wz
-- awesome (Two s) = _wA
-- awesome Three = _wB
-- awesome (Four b adt') = _wC

data Foo a = Foo a | Bar

-- tactics can generate an Arbitrary instance for you
-- uncomment the two lines below, select the type hole, then "Attempt to fill hole" the hole fill is show below
-- instance (Arbitrary a) => Arbitrary (Foo a) where
--   arbitrary = _

-- this is the result!
-- instance (Arbitrary a) => Arbitrary (Foo a) where
--   arbitrary
--     = let terminal = [Foo <$> arbitrary, pure Bar]
--       in
--         sized
--           $ (\ n
--                -> case n <= 1 of
--                     True -> oneof terminal
--                     False -> oneof $ ([] <> terminal))

-- if there's plenty of time left over, we talk about custom wingman tactics
data Baz s a = Baz s a (s -> a) | Quux [(Int, a)]

-- instance Functor (Baz s) where
--   fmap = _

-- replace the underscore with a custom tactic: [wingman| intros |]
-- running that custom tactic will get you this next line
--  fmap f x = _wV

-- instance Functor (Baz s) where
--   fmap = [wingman| intros f x, destruct x, ctor Baz, assumption, application, assumption |]

-- instance Functor (Baz s) where
--  fmap = [wingman| intros f x, homo x |]

-- comma means "run a tactic at the next hole that exists", semicolon means "run a tactic at every hole that exists"

-- instance Functor (Baz s) where
--   fmap f (Baz s a fsa) = _wa
--   fmap f (Quux x1) = _wb

-- instance (Arbitrary a, Arbitrary s) => Arbitrary (Baz s a) where
--   arbitrary = _

-- this doesn't work right for reasons unknown?
-- uncomment the instance line below, move cursor to the end, should see hls-class-plugin actions:
-- "Add placeholders for 'pure', '<*>' " and "Add `Functor Foo` to the context of this instance declaration"
-- instance Applicative Foo
