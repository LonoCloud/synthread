# Syntax Threading Library

Extensions of Clojure's standard -> macro used to build code blocks that
operate on a threaded topic.

# Including in your project.clj

SynThread is available from clojars. Add it to your Leiningen project.clj:

```clojure
   [lonocloud/synthread "1.0.5"]
```

# Usage

See [unit tests](http://github.com/LonoCloud/synthread/blob/master/test/lonocloud/synthread/test.clj#L11)
for specific examples of each macro.

Some general guidelines:

1. Require SynThread with the alias `->` like this:
```clojure
   (ns your.ns.here
     (:require [lonocloud.synthread :as ->]))
```

2. Always start a threaded block with Clojure's standard `->` macro or `->/do` (see next point).
This clearly identifies the topic to be threaded.
```clojure
   ;; good
   (-> {:first "John" :last "Smith"}
       (->/assoc :last first)) ;; replace last name with last initial

   ;; bad
   (->/assoc {:first "John" :last "Smith"}
             :last first) ;; replace last name with last initial
```

3. Don't change the type (or shape) of the topic as it flows through. In our
experience, the threading macros are used to either dig into a deep data
structure massaging the topic the deeper it goes or are used to build a
result by describing a pipeline of operations. The main difference between
digging and building is that the type and shape of the threaded topic is
changing or is constant respectively. We use synthread macros for
building as a general rule. The `->/do` macro will behave like `->`
but will also check that the topic is maintained through a threaded
block.
```clojure
   ;; good
   (-> {:a 1 :b 2}
     (->/assoc :a inc)
     (->/in [:b]
       (->/for [n (range 3)]
         (inc n))))
   ;; returns {:a 2 :b 5}

   ;; bad
   (-> {:a 1 :b 2}
     (->/assoc :a inc)
     :b ;; type changed from map to number
     inc)
   ;; returns 3
```

4. Use `->/as` to put the threaded value (or "topic") into a named
local variable. This is useful when you need to call a function or
macro that needs access to the topic in some parameter slot other than
the first:
```clojure
   (-> {:a 1 :b 2}
     (->/assoc :a inc)
     (->/as topic                             ;; label topic
       (->/when (> (:b topic) 10)
         (assoc :large-b true))))
```
Standard destructuring is supported by `->/as`:
```clojure
   (-> {:a 1 :b 2}
     (->/assoc :a inc)
     (->/as {:keys [b]}                       ;; destructure topic
       (->/when (> b 10)
         (assoc :large-b true))))
```
Additionally, a special destructuring form is supported allowing the
use of functions. Passing a threaded form will implicity insert the
topic at the front and use the last argument as the binding label. For
example:
```clojure
   (-> {:a 1 :b 2}
     (->/as (-> vals (->/apply max) max-val)  ;; use functions on topic
       (->/when (> max-val 10)
         (assoc :large-val true))))
```

5. Clojure's `do` and `doto` macros are useful in these threading
contexts, so don't be afraid to use them. `do` lets you stop
threading, and yet pass a result to the next threaded step:
```clojure
   (-> {:a 1 :b 2}
     (->/assoc :a inc)
     (->/when we-should-reset?
       (do {:a 0 :b 0}))  ;; see also ->/reset function
     (->/assoc :b inc))
```
This can be particularly useful in conjunction with `->/as`:
```clojure
   (-> {10 :orig, 20 :orig}
     (->/as topic
       (do
         (reduce #(assoc %1 %2 :default) topic (range 5)))))
```
On the other hand, `doto` is nice when you do *not* want to pass a
result to the next step:
```clojure
   (-> {:a 1 :b 2}
     (doto prn)
     (->/assoc :a inc))
```
The debugging `prn` above works, but the patten rapidly becomes
awkward if you want to provide a label to the prn. That would actually
require a combination of `->/as` to label it and `do` to prevent the
topic from being printed before the label because of threading. This
is exactly the purpose of `->/aside`:
```clojure
   (-> {:a 1 :b 2}
     (->/aside topic        ;; note the body is entirely unthreaded
       (prn :topic topic)
       (println "Note: b is currently" (:b topic))) ;; return value is ignored
     (->/assoc :a inc))
```

6. In addition to the threading macros, two helper functions are also
available: `->/reset` and `->/apply`. Use `->/reset` to set the value
of the threaded topic (similar to Clojure's `reset!`) and use
`->/apply` to call a function with the threaded topic as its first
argument (similar to Clojure's `apply`).
```clojure
   ;; example of ->/reset
   (-> false
     (->/reset true))
   ;; returns true

   ;; example of ->/apply
   (-> [0]
     (->/apply conj [1 2 3])  ;; => (conj [0] 1 2 3)
     (->/apply [conj 4 5 6])) ;; also works!
   ;; returns [0 1 2 3 4 5 6]
```

7. Some of the macros are marked EXPERIMENTAL to reflect the fact that
they have seen little or no use in our live code.

# Related work

This is not the first library to have sailed these waters. Some that
we were aware of during the design of SynThread include:

- Clojure 1.5: `cond->`, `as->`, `when->`
- [Pallet thread-expr](https://github.com/pallet/thread-expr): `arg->`, `for->`, `when->`, `let->`, etc.
- [Sierra's Syntactic Pipelines](http://stuartsierra.com/2012/09/12/when-to-write-a-macro): `defpipe`
- [Prismatic plumbing](https://github.com/Prismatic/plumbing/blob/master/src/plumbing/core.clj#L280): `fn->`
- [Levy's swiss-arrows](https://github.com/rplevy/swiss-arrows): `-<>`, `-?<>`, `-<><:p`, etc.
- [`core.incubator`](https://github.com/clojure/core.incubator): `-?>`, `-?>>`, `.?.`, etc.

# Copyright
© LonoCloud. All rights reserved.
The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
You must not remove this notice, or any other, from this software.

