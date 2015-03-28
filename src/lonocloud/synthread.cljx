(ns lonocloud.synthread
  (:refer-clojure
   :exclude [do if let if-let when when-not when-let for first
             second nth take drop last butlast rest let
             fn cond apply])
  #+clj
  (:require [lonocloud.synthread.core :refer :all])
  #+cljs
  (:require [lonocloud.synthread.core :refer [>reset >apply]]
            [lonocloud.synthread.impl]))

#+clj
(def ^{:private true} macro-template
  `(defmacro mname [& args#]
     `(mname ~@args#)))

#+clj
(comment
  ;; macro-template is actually expanded by reader as something like
  ;; this:
  (defmacro mname [& args#]
    (seq (concat (list (quote mname)) args__3564__auto__))))

#+clj
(defn- real-macro [macro-name]
  (symbol (str "lonocloud.synthread.core/>" macro-name)))

#+clj
(defn- alias-macro [macro-name]
  (eval (>do macro-template
             (>second
              (>reset macro-name))
             (>last
              (>second
               (>second
                (>second
                 (>second
                  (>reset (real-macro macro-name))))))))))

#+clj
(doseq [macro-name '[do if let if-let when when-not when-let for first
                    second nth take drop last butlast rest update let
                    fn as aside side each each-as cond in]]
  (alias-macro macro-name))

;; alias functions
(def reset >reset)

(def apply >apply)
