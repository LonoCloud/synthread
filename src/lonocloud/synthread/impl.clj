(ns lonocloud.synthread.impl
  (:require cljs.env
            cljs.analyzer)
  (:refer-clojure :exclude [defmacro]))
(alias 'clj 'clojure.core)

;; Macro convenience for cljc files.
(clj/defmacro defmacro-cljc
  "Clojurescript compatible defmacro that should work in cljc files
  without protecting the invocation with a reader conditional."
  [macro-name & tail]
  (if-let [ns-name (:name (:ns &env))]
    ;; The presence of ns-name indicates this is a cljs compilation
    `^:merge (~'ns ~ns-name (:require-macros [~ns-name :refer [~macro-name]]))
    `(clj/defmacro ~macro-name ~@tail)))

;; namespace related functions and macros

(defn cljs-env?
  "Return true when compiling in a clojurescript enviroment."
  [env]
  (contains? env :ns))

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

(defn ns-current-name
  "Return the name of the current namespace."
  [env]
  (:name (:ns env) (ns-name *ns*)))

;; Cloaking logic
(defn ^:private cloaked-sym
  "Return a cloaked symbol based on s."
  [s]
  (with-meta (symbol (str "__" (name s)))
    (meta s)))

(defn ^:private uncloaked-sym
  "Return an uncloaked symbol from s."
  [s]
  (symbol (subs (name s) 2)))

(defn ^:private cloaked-name
  "Return a cloaked name if n has been tagged as ^:cloaked."
  [n]
  (if (:cloaked (meta n))
    (cloaked-sym n)
    n))

(clj/defmacro defmacro
  "Add ability to cloak the macro's name so as to not override a
  prexisting name. See uncloak macro for revealing cloaked macros."
  [macro-name & tail]
  `(defmacro-cljc ~(cloaked-name macro-name) ~@tail))

(clj/defmacro cloaked
  "Expand to the cloaked value associated with sym."
  [sym]
  (cloaked-sym sym))

(clj/defmacro decloak
  "Decloak sym by defining a var with the same name."
  [sym]
  ;; if sym is a macro and we are compiling in a cljs env, refer it
  ;; since it was already decloaked on the clj side.
  (if (contains? (:macros (:ns &env)) sym)
    (let [ns-name (:name (:ns &env))]
      `^:merge (~'ns ~ns-name (:require-macros [~ns-name :refer [~sym]])))

    (let [var-form `(var ~(cloaked-sym sym))]
      `(do
         ;; unmap sym in current ns to avoid warnings.
         (ns-unmap '~(ns-current-name &env) '~sym)

         ;; clj env only! Refer a found prior-var back into current ns
         ;; so that it can be recycled by the def below. This allows
         ;; for error free reloading of namespaces (in Clojure) that
         ;; use :refer to import a decloaked var. Such a mess :-/
         ~(when-not (cljs-env? &env)
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

#_(clj/defmacro decloak-macro
  "Decloak sym by ..."
  [sym]
  (when-let [ns-name ]
    ;; The presence of ns-name indicates this is a cljs compilation
    ))

(defmacro ^:cloaked defn
  "Add ability to cloak a function. Use uncloak macro to
  reveal cloaked functions."
  [fn-name & tail]
  `(defn ~(cloaked-name fn-name) ~@tail))
(decloak defn)
