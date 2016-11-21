---
title: More fun with Associated Types
date: 2016-08-11
---

In this example will cover a basic example inspired from the haskell wiki but slightly different. Overloading type constructors is the core concept again. Will see this time how it will provide a means to abstract (overload) a common operation with collection like structures.

We are all familiar with lists, arrays, vectors, dictionaries, maps etc. An array and a dictionary are really very similar, dictionaries on one hand are indexed by strings (more generally with EQ types) while arrays and lists get indexed by integers.

What we would like to do is overload the common api for all these structures. Specifically **getting** an element, **inserting** an element and create an **empty** structure. In this example however I will only overload retrieving an element from the Collection like structure.



```haskell
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE UndecidableInstances#-}
import qualified Prelude
import Prelude hiding (lookup)
import Data.Matrix hiding ((!))

-- Some Helpers
castToInt :: (RealFrac a) => a -> Int
castToInt = fromIntegral . round

toInt :: Integral a => a -> Int
toInt = fromIntegral
```
This is the typeclass that will permit overloading a lookup function.
``` haskell
class Mappable k where
    type GMap k v :: *
    lookup :: k -> GMap k v -> Maybe v

```
Now lets provide some instances. So when a key is an Int we should expect to be dealing with a list, but when the key is a string we think of a dictionary container. Even better what kind of structure do you think should be indexed by a tuple of integers? There might be more than one answer to this but i like to think its a matrix.


``` haskell
--instance Integral k => Mappable k where
instance Mappable Int where
    type GMap Int a = [a]
    lookup k l = if k >= length l then Nothing else Just $ l !! (toInt k)

--instance (Integral k, Integral j) => Mappable (k,j) where
instance Mappable (Int,Int) where
    type GMap (Int,Int) a = Matrix a
    lookup (x,y) = uncurry safeGet (x,y)

newtype Dict k v = Dict {fromDictionary :: [(k,v)]} deriving (Show)

instance Mappable String where
    type GMap String v = Dict String v
    lookup s = Prelude.lookup s . fromDictionary  
            
-- Its more handy to have a infix operator to index into structures
(!) = flip lookup
```
We can see how the type signature of the lookup function changes depending on the indexing type.
``` haskell 
(x,y) = (2,3) :: (Int,Int)
lookup "someKey" :: Dict String v -> Maybe v
lookup x :: [v] -> Maybe v
lookup (x,y) :: Matrix v -> Maybe v
```
Finally some examples: 

``` haskell
pos = (2,3) :: (Int,Int)
ex1 = lookup pos $ zero 5 5
ex2 = [1..4] ! (3::Int)

```
