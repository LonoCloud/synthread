(ns lonocloud.synthread.isolate)

(defn copy-ns-from [from-ns]
  (doseq [[sym target] (ns-map from-ns)]
    (if (var? target)
      (.refer *ns* sym target)
      (.importClass *ns* sym target)))
  (doseq [[sym ns] (ns-aliases from-ns)]
    (alias sym ns)))

(defn split-def [def-macro [def-name & def-tail]]
  `(do
     (~def-macro v# ~@def-tail)
     (ns-unmap 'lonocloud.synthread '~def-name)
     (intern 'lonocloud.synthread (with-meta '~def-name (meta #'v#))
             @#'v#)))

(defmacro isolate-ns
  "Isolate the body of each definition (defaulting to defn's and
  defmacro's) in the rest of this namespace from the vars being
  defined. Access to these vars is available through the alias given
  to the optional :as parameter. Useful only for namespaces that
  provide many vars whose names conflict with clojure.core."
  [& {:keys [as macros]}]
  (let [orig-ns (symbol (str *ns*))
        macros (or macros `[defn defmacro])]
    `(do
       (in-ns '~(symbol (str *ns* ".isolated")))
       (copy-ns-from '~orig-ns)
       ~(when as
          `(alias '~as '~(symbol (str *ns*))))
       ~@(for [macro macros
               :let [unqualified-macro (symbol (name macro))]]
           `(do
              (ns-unmap *ns* '~unqualified-macro)
              (clojure.core/defmacro ~unqualified-macro [& everything#]
                (split-def '~macro everything#)))))))
