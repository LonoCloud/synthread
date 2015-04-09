(ns lonocloud.synthread
  (:refer-clojure :exclude [apply])
  (:require #?(:cljs lonocloud.synthread.impl
               :clj  lonocloud.synthread.core))
  #?(:cljs (:require-macros [lonocloud.synthread :refer [publish-vars]])))

#?(:clj
   (defmacro ^:private publish-vars
     "Publish synthread vars (vars named with a leading >) from src-ns
  into this ns, stripping the leading > from the published vars and
  preserving metadata from the original vars. At call time, this macro
  will unmap any previously defined vars to avoid warnings."
     [src-ns]
     `(do
        ~@(for [[orig-sym var] (ns-publics src-ns)
                :when (.startsWith (name orig-sym) ">")
                :let [sym (symbol (subs (name orig-sym) 1))]]
            `(do
               (ns-unmap 'lonocloud.synthread '~sym)
               (let [orig-var# (var ~(symbol (name src-ns) (name orig-sym)))]
                 (def ~sym (deref orig-var#))
                 (alter-meta! (var ~sym) merge (meta orig-var#))))))))

#?(:clj (publish-vars lonocloud.synthread.core))

(publish-vars lonocloud.synthread.impl)
