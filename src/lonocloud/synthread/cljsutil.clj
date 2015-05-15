(ns lonocloud.synthread.cljsutil
  (:refer-clojure :exclude [defmacro macroexpand-1])
  (:require [cljs.analyzer :as cljs]))
(alias 'clj 'clojure.core)

(defn cljs-env?
  "Return true when compiling in a cljs enviroment."
  [env]
  (contains? env :ns))

(defn current-cljs-ns-name
  "Return the current cljs namespace name or nil when not compiling
  compiling in a cljs environment."
  [env]
  (:name (:ns env)))

(defn current-ns-name
  "Return the name of the current namespace. Works in clj and cljs
  compilation environments."
  [env]
  (:name (:ns env) (ns-name *ns*)))

(defn macro?
  "Return true if sym is the name of a macro in the current
  clojurescript namespace."
  [env sym]
  (contains? (:macros (:ns env)) sym))

(defn resolve-macro-meta
  "Resolve sym to the meta data of a macro. Works in clj and cljs
  compilation environments."
  [env sym]
  (if (cljs-env? env)
    (cljs/resolve-macro-var env sym)
    (meta (resolve sym))))

(defn macroexpand-1
  "Expand form 1 time. Works in clj and cljs compilation
  environments."
  [env form]
  (if (cljs-env? env)
    (cljs/macroexpand-1 env form)
    (clj/macroexpand-1 form)))

;; Macro convenience for cljc files.
(clj/defmacro defmacro
  "Clojurescript compatible defmacro that should work in cljc files
  without protecting the invocation with a reader conditional."
  [macro-name & tail]
  (if-let [ns-name (current-cljs-ns-name &env)]
    `^:merge (~'ns ~ns-name (:require-macros [~ns-name :refer [~macro-name]]))
    `(clj/defmacro ~macro-name ~@tail)))
