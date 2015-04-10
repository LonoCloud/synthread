# Syntax Threading Library

Extensions of Clojure's standard -> macro used to build code blocks that
operate on a threaded topic.

# Installation

SynThread is available from clojars. Add it to your project dependencies:
[![Clojars Project](http://clojars.org/lonocloud/synthread/latest-version.svg)](http://clojars.org/lonocloud/synthread)

# Usage

See [unit tests](test/lonocloud/synthread/test.cljc) for specific examples of each macro.

Read the [usage guide](https://github.com/LonoCloud/synthread/wiki/usage) to learn about conventions and tips.

The [quick reference](https://github.com/LonoCloud/synthread/wiki/quickref) summarizes the macros and functions provided. 

# Related work

This is not the first library to have sailed these waters. Some that
we were aware of during the design of SynThread include:

- Clojure 1.5: `cond->`, `as->`, `when->`
- [Pallet thread-expr](https://github.com/pallet/thread-expr): `arg->`, `for->`, `when->`, `let->`, etc.
- [Sierra's Syntactic Pipelines](http://stuartsierra.com/2012/09/12/when-to-write-a-macro): `defpipe`
- [Prismatic plumbing](https://github.com/Prismatic/plumbing/blob/master/src/plumbing/core.clj#L280): `fn->`
- [Levy's swiss-arrows](https://github.com/rplevy/swiss-arrows): `-<>`, `-?<>`, `-<><:p`, etc.
- [`core.incubator`](https://github.com/clojure/core.incubator): `-?>`, `-?>>`, `.?.`, etc.

# Example walkthrough

http://www.infoq.com/presentations/Macros-Monads

# Copyright

© 2013-2015 LonoCloud.
© 2015 Hoang Minh Thang.

All rights reserved.
The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this
distribution. By using this software in any fashion, you are agreeing
to be bound by the terms of this license. You must not remove this
notice, or any other, from this software.
