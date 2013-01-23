# Syntax Threading Library

Extentions of Clojure's standard -> macro used to build code blocks that
operate on a threaded topic. See also: test-> in recent Clojure versions.

# Including in your project.clj

SynThread is available from clojars. Add it to your Leiningen project.clj:

[lonocloud/synthread "1.0.0"]

# Usage

See unit tests for specific examples of each macro.

There are two style guidelines we recommend:

1. Always start a threaded block with Clojure's standard '->' macro.
This clearly identifies the topic to be threaded.
```
   ;; good
   (-> {:first "John" :last "Smith"}
       (->/assoc :last first))

   ;; bad
   (->/assoc {:first "John" :last "Smith"}
             :last first)
```

2. Don't change the type (or shape) of the topic as it flows through.
In our experience, -> macros are used to either dig into a deep data
structure massaging the topic the deeper it goes or -> macros are
used to build a result by descrbing a pipeline of operations. The main
difference between digging and building is that the type and shape of
the threaded topic is changing or is constant respectively. We use
these macros for builders as a general rule.
```
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