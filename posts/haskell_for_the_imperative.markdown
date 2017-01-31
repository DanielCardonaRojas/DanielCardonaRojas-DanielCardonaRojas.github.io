---
title: Haskell for the imperative
date: 2016-10-11
---

In this post i would like to show how in functional languages like Haskell with support for higher 
order functions and currying imperative like control structures can be me defined. But finally point out
how functional programming has other tooling different from the imperative versions, that makes us think 
differently.


Lets try to construct a for loop just using functions. A simple for loop will almost look identical
in any c like programming language.

```c
for (i = 0; i < 9; i++){
    //Do some side effecting operations
}
```

We can identify for parts in this control structures:

- An initial value
- Some predicate to decide continuing the loop
- An updating function in this case a simple increment.


Lets model this in Haskell, with the above information, lets give "for" a type:

```haskell
for' :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
```

for just returns an IO action that doesn't yield any value, i.e a pure side effecting IO value.
So lets try defining this function: 

```haskell

for' :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for' i p f code = if p i then code i >> for' (f i) p f code  else return ()
            
```

A simple one liner, very cool.

Lets try generalizing this for a bit more. Wouldn't it be nice if the code inside the for loop could return a 
value in each iteration and those values be collected as a final result ?

Well we'll do just that:

```haskell 
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO b) -> IO [b]
```

The implementation is very easy as well:

```haskell
for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO b) -> IO [b]
for i p f code = 
    case p i of 
        True -> do
            iterationResult <- code i
            rest <- for (f i) p f code
            return $ iterationResult : rest
        False -> return []

```


Lets try using it to get a feel for it:

```haskell
example = for 1 (< 3) (+ 1) $ 
            \i -> do
                putStrLn $ "Iteration values is " ++ (show i)
                return (i*3) -- the value that will be collected 
```

**Currying**

Because of currying defining new utilities is very simple for example lets define a new control function that just repeats an 
action a number of times:

```haskell
repeatN n = for' 0 (< n) (+ 1) 
```

Now we can use it trivially like this:


```haskell
example2 = repeatN 3 (\_ -> putStrLn "Hello")
```

**Generalizing to Monad**

Finally we can go a step further to generalize this for not just to do IO but any other 
monadic effect just by changing the type signature and no more.

```haskell
for :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m b) -> m [b]
```

Similar functions can be achieved for "if" and "while" control structures and can be generalized too.

Modern languages have variations of the classical for loop which suits better in object oriented contexts, like "for each"
or "for in ". In Haskell we have map for pure computations, mapM. And some other variations.


Although it is very interesting to see how easy it is to emulate for loops and other type of imperative
structures, the definitions above would typically be written differently in a more declarative and idiomatic way.


```haskell
ffor :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m b) -> m [b]
ffor i p m f = mapM f $ filter p $ iterate m i

ffor' :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
ffor' i p m f = mapM_ f $ filter p $ iterate m i
```

Notice how reasoning about this definition is very different from the imperative version even though they do the same.

Describing the for loop in C (first snippet) would be something like this:

`for` is a control structure that:
1) Starts a loop variable with an initial value
2) Checks if a condition holds using logical operators (if it doesn't exit/return)
3) Executes some user defined code
4) Modifies the loop variable (usually an increment)
5) Repeats steps 2) and 3) until exit.

Describing the Haskell version of the for loop might go something like this: 

ffor is a function that collects the results of mapping a monadic function over a filtered list in which the list
is obtained by iterating a modifying function over an initial value.
