# Even function composition is lazy in Haskell

I was reading section 8.3 of the [Gentle Introduction to Haskell](https://www.haskell.org/tutorial/stdclasses.html) about `Show` and `Read`, and my intuition got things wrong again.

There's an example of writing a `Show` instance for a tree by concatenating strings:

```
showTree                :: (Show a) => Tree a -> String
showTree (Leaf x)       =  show x
showTree (Branch l r)   =  "<" ++ showTree l ++ "|" ++ showTree r ++ ">"
```

And there's a different implementation using composition of functions:

```
showsTree               :: (Show a) => Tree a -> ShowS
showsTree (Leaf x)      =  shows x
showsTree (Branch l r)  =  ('<':) . showsTree l . ('|':) . showsTree r . ('>':)
```

That looked cool, but my intuition said that this approach wouldn't work for infinite data structures because it would have to start at the end, on the right side, and build from there.  That was WRONG.

The composition operator, `(.)`, is right associative.  So if you have an expression `a . b .c . d`, it means:

```
('<':) . (showsTree l . (('|':) . (showsTree r . ('>':))))
```

Now, what if we want just the first bit of the answer.  Can we compute it without building the complete result?

The definition of `(.)` looks like this:

```
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)
```

Let's start with a call to `shows` and go step by step to see what happens

```
shows (Branch (Leaf 1) (Leaf 2)) ""

(('<':) . showsTree l . ('|':) . showsTree r . ('>':)) (Branch (Leaf 1) (Leaf 2))

(\x -> ('<' :) ((showsTree l . ('|':) . showsTree r . ('>':)) x) ""

('<' :) ((showsTree l . ('|':) . showsTree r . ('>':)) "") 

['<' : ((showsTree l . ('|':) . showsTree r . ('>':)) "")]
```

Cool!  We've generated part of the answer, and run just the first of the functions in the list being composed.

My starting intuition was procedural: start with the value passed in, give it to the rightmost function, then the next, etc.

Now I'm thinking about it from the answer side.  How do you get the answer?  The answer is the result of appling the leftmost function to something.  So you invoke the leftmost function, and only compute the inputs that it needs more details about.  In the case of a `ShowS` function, you don't need the details until you get to displaying that part of the string.  If your application is going to take just the first part, you'll never have to compute the rest.