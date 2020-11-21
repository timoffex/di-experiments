# di-experiments

A repository for experimenting with dependency injection concepts in
Haskell. Note: I am abusing the term "dependency injection". What I
really mean is "stuff enabled by DI frameworks in other languages".

My first attempt is under hlist-attempt; there I have a much longer
README that describes a particular problem solved by DI. That
repository tries to define a "DI module" as a type parameterized by
the list of types that it "binds". I use heterogeneous lists to
accomplish this.

I was very excited about the HList approach when I first came up with
it, but I now have some better ideas, which is why I called the HList
approach "hlist-*attempt*". I have something in mind that uses
`fused-effects` and is essentially the RIO pattern (ReaderT + IO) but
hopefully even better!

I would like to eventually compare all of these approaches with each
other and with how one designs programs using the Guice DI framework
(in Java).
