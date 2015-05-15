(ns lonocloud.synthread.cloaking
  (:require [lonocloud.synthread.cljsutil :as cljsutil])
  (:refer-clojure :exclude [defmacro]))
(alias 'clj 'clojure.core)

(defn var-name?
  "Return true if (qualified) sym matches both the namespace and name of var."
  [var ns-sym name-sym]
  (and (= (ns-name (.ns var)) ns-sym)
       (= (.sym var) name-sym)))

(defn seek-var
  "Return the first var found in ANY namespace with a fully qualified
  name matching both ns-sym and name-sym."
  [ns-sym name-sym]
  (some identity
        (eduction (mapcat ns-map)
                  (map val)
                  (filter var?)
                  (filter #(var-name? % ns-sym name-sym))
                  (all-ns))))

(defn cloak-sym
  "Return a cloaked symbol based on s."
  [s]
  (with-meta (symbol (str "__" (name s)))
    (meta s)))

(defn cloak-name
  "Return a cloaked name if n has been tagged as ^:cloaked."
  [n]
  (if (:cloaked (meta n))
    (cloak-sym n)
    n))

(clj/defmacro defmacro
  "Add ability to cloak macro so that macro-name is hidden until
  'decloaked' at a later point in the file. Use decloak to reveal the
  hidden name."
  [macro-name & tail]
  ;; expand into cljsutil's defmacro for cljs compatibility.
  `(cljsutil/defmacro ~(cloak-name macro-name) ~@tail))

(clj/defmacro decloak-1
  "Decloak sym by defining a var with the same name."
  [sym]
  ;; if sym is a macro and we are compiling in a cljs env, refer it
  ;; since it was already decloaked on the clj side.
  (if (and (cljsutil/cljs-env? &env) (cljsutil/macro? &env sym))
    (let [ns-name (cljsutil/current-cljs-ns-name &env)]
      `^:merge (~'ns ~ns-name (:require-macros [~ns-name :refer [~sym]])))

    (let [var-form `(var ~(cloak-sym sym))]
      `(do
         ;; unmap sym in current ns to avoid warnings.
         (ns-unmap '~(cljsutil/current-ns-name &env) '~sym)

         ;; clj env only! Refer a found prior-var back into current ns
         ;; so that it can be recycled by the def below. This allows
         ;; for error free reloading of namespaces (in Clojure) that
         ;; use :refer to import a decloaked var. Such a mess :-/
         ~(when-not (cljsutil/cljs-env? &env)
            `(when-let [prior-var# (seek-var '~(ns-name *ns*) '~sym)]
               (.refer ~'*ns* '~sym prior-var#)))

         ;; Define the decloaked var, transfer relevant meta data from
         ;; the cloaked var and mark the cloaked var as private.
         (def ~sym (deref ~var-form))
         (alter-meta! (var ~sym)
                      merge
                      (dissoc (meta ~var-form)
                              :name :cloaked))

         ;; Return the decloaked var.
         (var ~sym)))))

(clj/defmacro decloak
  "Decloak syms by defining vars named sym and referencing the cloaked values."
  [& syms]
  `(do
     ~@(for [sym syms]
         `(decloak-1 ~sym))))

(defmacro ^:cloaked defn
  "Add ability to cloak a function. Use uncloak macro to reveal
  cloaked functions."
  [fn-name & tail]
  `(defn ~(cloak-name fn-name) ~@tail))
(decloak defn)
