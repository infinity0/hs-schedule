{-|

A "reservation" data structure is one that allows inserts by many writers, but
only allows deletes by the writer that originally performed each insert. That
is, in some sense, each insert is "owned" by the writer that performed it. This
is achieved by returning a unique handle from an insert operation, that must be
supplied to the deletion operation.

== Items that know their own handles

If you need to store the handle together with the item, e.g. so that the item
can delete itself, this should be possible by the standard "tying the knot"
technique that takes advantage of Haskell's lazy semantics. See
<test/Data-Rsv-ExampleRecursive.html Data.Rsv.ExampleRecursive> for an example.

-}

module Data.Rsv where
