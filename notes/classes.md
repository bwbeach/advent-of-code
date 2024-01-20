# Type Class Cheat Sheet

![Type Hierarchy](classes.png)

Semigroup
 - `<>` (with `sconcat`)

Monoid
 - `mempty`
 - `mappend` (with `mconcat`)

Functor
 - `fmap` (or `<$>`)

Applicative
 - `<*>`

Monad 
 - `return`
 - `>>=`
