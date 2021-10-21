module Examples where

-- Note: The different options here are exposed as "code actions" and "code lenses" via the language server protocol.
-- If you have trouble applying some of these examples make sure your editor is configured to use both functionalities
-- and make yourself familiar with how to use them. (e.g. keybindings).

-- move cursor to the next line, should see 'Remove all redundant imports' , 'Remove import' , 'Make all imports explicit'
import Data.Char
import Data.Monoid
import Data.Text (Text, unpack)
import Test.QuickCheck

-- the line below should show 'add signature' once you click that, 'Generate signature comments' will fill in empty haddock comments
biggest items = foldr1 max items

-- the line above will show two hlint hints, "eta reduce" and "use maximum"

-- code evaluation code lens: as long as lens mode is on, you'll see "Evaluate..." on the >>> line below

-- | Triple a list
-- >>> triple "a"
triple l = l ++ l ++ l

-- call hierarchy
-- fold / unfold
thisthing = biggest $ triple doExamples

doExamples :: [Char]
doExamples = "Examples"

-- add missing imports
-- otherthing = groupBy

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
-- for bonus fun, do a case split on the string in Two

data Foo a = Foo a | Bar

-- wingman can generate an typeclass instances for you
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

-- attempt to fill hole
-- instance Functor Foo where
--   fmap = _

-- as usual, attempt to fill hole
-- instance Applicative Foo where
--   pure = _
--   liftA2 = _

-- if there's plenty of time left over, we talk about custom wingman tactics
data Baz s a = Baz s a (s -> a) | Quux [(Int, a)]

-- instance Functor (Baz s) where
--   fmap = [wingman| intros |]

-- replace the underscore with a custom tactic: [wingman| intros |]
-- running that custom tactic will get you this next line
--  fmap f x = _wV

-- instance Functor (Baz s) where
--   fmap = [wingman| intros f x, destruct x, ctor Baz, assumption, application, assumption |]

-- instance Functor (Baz s) where
--  fmap = [wingman| intros f x
--                  , homo x
--                  ; assumption
--                  | (with_arg, assumption, nested fmap)
--                  , assume f |]
-- in Sandy's video, the custom tactic above correctly produces the right Functor instance even when Baz changes greatly

-- comma means "run a tactic at the next hole that exists", semicolon means "run a tactic at every hole that exists"

-- "attempt to fill hole" does work here, with one extra step
-- instance (Arbitrary a, Arbitrary s) => Arbitrary (Baz s a) where
--   arbitrary = _

data Zoop = Zoop (Sum Int) String Any
  deriving (Eq, Ord, Show)

-- 1. refine hole 2. split all function arguments 3. attempt to fill hole
-- instance Semigroup Zoop where
--   (Zoop sum s any) <> (Zoop sum' str any') =
--     Zoop (sum <> sum') (s <> str) (any <> any')

-- instance Semigroup Zoop where
--     (<>) = [wingman| intros x1 x2 , destruct all , split ; pointwise (use (<>)) ; assumption |]

-- instance Monoid Zoop where
--   mempty = [wingman| split; use mempty |]

-- things below don't work for reasons unknown

-- uncomment the instance line below, move cursor to the end, should see hls-class-plugin actions:
-- "Add placeholders for 'pure', '<*>' " and "Add `Functor Foo` to the context of this instance declaration"
-- instance Applicative Foo

-- add missing pragmas
-- foo :: String
-- foo = show (unpack "foo")

-- bugs happen, make sure you know how to restart haskell-language-server from inside your editor
