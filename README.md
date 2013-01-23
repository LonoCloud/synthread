# Syntax Threading Library

Extentions of Clojure's standard -> macro used to build code blocks that
operate on a threaded topic. See also: test-> in recent Clojure versions.

# Including in your project.clj

SynThread is available from clojars. Add it to your Leiningen project.clj:

```clojure
[lonocloud/synthread "1.0.0"]
```

# Usage

See [unit tests](http://github.com/LonoCloud/synthread/blob/master/test/lonocloud/synthread/test.clj#L11)
for specific examples of each macro.

There are three style guidelines we recommend:

1. Require SynThread with the alias `->` like this:
```clojure
   (ns your.ns.here
     (:require [lonocloud.synthread :as ->]))

2. Always start a threaded block with Clojure's standard `->` macro.
This clearly identifies the topic to be threaded.
```clojure
   ;; good
   (-> {:first "John" :last "Smith"}
       (->/assoc :last first))

   ;; bad
   (->/assoc {:first "John" :last "Smith"}
             :last first)
```

3. Don't change the type (or shape) of the topic as it flows through.
In our experience, -> macros are used to either dig into a deep data
structure massaging the topic the deeper it goes or -> macros are
used to build a result by descrbing a pipeline of operations. The main
difference between digging and building is that the type and shape of
the threaded topic is changing or is constant respectively. We use
these macros for builders as a general rule.
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
