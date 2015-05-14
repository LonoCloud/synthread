(ns lonocloud.synthread.bindmacro
  (:require cljs.analyzer
            [lonocloud.synthread.impl :refer [cljs-env?]]))

(defn get-label
  "Return outer binding label (that has been attached to the meta data
  of form."
  [form]
  (or
   (::label (meta form))
   (throw (Exception. (str "Unable to expand " (first form)
                           " in non-binding block context.")))))

(defn bindmacro?
  "Return true if sym resolves to a var tagged with :bindmacro."
  [env sym]
  (:bindmacro (if (cljs-env? env)
                (cljs.analyzer/resolve-macro-var env sym)
                (meta (resolve sym)))
              false))

(declare expand)

(defn expand-1
  "Try to expand a binding macro given a binding pair."
  [env [label form :as binding]]
  (if (and (list? form)
           (symbol? (first form))
           (bindmacro? env (first form)))
    (expand env
            (let [labeled-form (vary-meta form assoc ::label label)]
              (if (cljs-env? env)
                (cljs.analyzer/macroexpand-1 env labeled-form)
                (macroexpand-1 labeled-form))))
    binding))

(defn expand
  "Try to expand binding macros given a vector of binding pairs."
  [env bindings]
  (->> bindings
       (partition 2)
       (mapcat expand-1 (repeat env))))
