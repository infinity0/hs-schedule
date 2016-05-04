{-|

= Reservation data structures

A "reservation" data structure is one that allows inserts by many writers, but
only allows deletes by the writer that originally performed each insert. That
is, in some sense, each insert is "owned" by the writer that performed it.

This is achieved via the following pattern: the structure offers only a public
"insert" function, and no other write-functions. Instead, the insert function
returns a private "delete" function restricted to that insertion, to be called
by the writer later. This way, different writers cannot clobber each other.

Further, writes are "insert" not "item" oriented: the item associated with an
insert is irrelevant. For example if "the same item" is inserted twice, each
insert still returns two distinct delete functions, that act differently. On
the other hand, reads are still "item" oriented - readers cannot know if any
two items were inserted by "the same writer".

This sort of access pattern is useful for e.g. dynamic callback registries,
where many different clients might want to "hook in" to something that you
emit, but you can't statically know what these clients might be.

= Potential future extensions

== Allow "change" operatinos

Support a 'Change' handle instead of 'Delete':

> type Change _ a = _ -> Maybe a -> RStruct _ a -> (Maybe a, RStruct _ a)

== Items that know their own handles

Store the handle with the item, so that the item can e.g. delete itself.
Currently to get this behaviour, you have to use quite a bit of boilerplate -
see <src/Data-Rsv-ExampleRecursive.html Data.Rsv.ExampleRecursive> for an
example.

-}

module Data.Rsv where
