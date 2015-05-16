(ns lonocloud.synthread.binding
  (:require [lonocloud.synthread.cljsutil :as cljsutil]))

(defn get-label
  "Return outer binding label (that has been attached to the meta data
  of form."
  [form]
  (or
   (::label (meta form))
   (throw (Exception. (str "Unable to expand " (first form)
                           " in non-binding block context.")))))

(defn binding?
  "Return true if sym resolves to a var tagged with :binding."
  [env sym]
  (:binding (cljsutil/resolve-macro-meta env sym)
            false))

(declare expand)

(defn expand-1
  "Try to expand a binding macro given a binding pair."
  [env [label form :as binding]]
  (if (and (list? form)
           (symbol? (first form))
           (binding? env (first form)))
    (expand env
            (cljsutil/macroexpand-1 env
                                    (vary-meta form assoc ::label label)))
    binding))

(defn expand
  "Try to expand binding macros given a vector of binding pairs."
  [env bindings]
  (->> bindings
       (partition 2)
       (mapcat expand-1 (repeat env))))
