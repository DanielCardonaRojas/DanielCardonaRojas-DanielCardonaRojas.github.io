---
title: Understanding Basics of Lenses
date: 2017-02-01
---


Lenses is perhaps on of the toughest topics learning Haskell, even though they accomplish a very simple task, one thats trivial 
in imperative languages. The reason for this is that for a purely functional programming language with inmutability this is hard to get around.

In this post I'll try to skim the basics of what I've learned.

The metaphor for lenses is that lenses can be stacked up (composed) like a microscope to see deeper into a structure. The nice this is that you can use 
a lens to either see down into the structure or modify it.

So you migh have heard a lens is a setter and a getter bundled up in a single thing. So given a set of setter and getter for some data type 
we should be abble to construct a lens.


## Derivation
We'll start defining the naive lens as is shown in other tutorials like (this)[] 


**TODO**


## Creating a Lens from Setter and Getter

The derivation of the shape of the lense  is kind of magic and I don't fully understand how on would arrive at it. So I'll just state it here 
and then show we can transform a lense into a setter getter and some other useful things.

A lens is:
```haskell
type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)
```
you can think of it as focusing on an "a" inside a structure "s". Or somthing that give an "s" can focus on an "a".


So given a setter and a getter we can build a lens

## Extracting the getter (view) (^.)

```haskell
view :: Lens' s a -> s -> a
view l s = getConst $ l Const s
```

## Extracting Setter (over) (%~)

```haskell
over :: Lens' s a -> (a -> a) -> s -> s
over l f s = runIdentity $ l (Identity . f) s
```

## Defining Utilities


## Further reading

Lenses work on product types but prism extend the idea to sum types.
