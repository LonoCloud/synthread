(ns lonocloud.synthread
  (:refer-clojure :exclude [defn defmacro])

  #?(:clj (:require [lonocloud.synthread.binding :as binding]))

  (#?(:clj :require :cljs :require-macros)
     [lonocloud.synthread.cloaking :refer [defn defmacro decloak]]))

;; Section 0: special syntax support for updating and getting from a
;; sub-path.

(defmacro ^:bindmacro upget
  "Expand into a threaded update-form followed by a threaded get-form both of
  which act on the value found in the current topic (bound to <>) under
  context. The resulting updated context value is then added back into <> and
  the result of get-form is bound to the bind-label (see below).

  This is a binding macro."
  [context update-form get-form]
  (when-let [label (binding/get-label &form)]
    `[a# (get ~'<> ~context)
      a# (-> a# ~update-form)
      ~'<> (assoc ~'<> ~context a#)
      ~label (-> a# ~get-form)]))

(defmacro ^:bindmacro getup
  "Expand into a threaded get-form followed by a threaded update-form both of
  which act on the value found in the current topic (bound to <>) under
  context. The resulting updated context value is then added back into <> and
  the result of get-form is bound to the bind-label (see below).

  This is binding macro."
  [context get-form update-form]
  (when-let [label (binding/get-label &form)]
    `[a# (get ~'<> ~context)
      ~label (-> a# ~get-form)
      a# (-> a# ~update-form)
      ~'<> (assoc ~'<> ~context a#)]))

;; Section 1: macros that do not update the topic.
;;            Generally control flow macros.
(defmacro ^:cloaked do
  "Thread x through body. Semantically identical to -> with the
  extra feature that the symbol <> is bound to the new value of x
  between each form.

  Note that the old marking constraint has been removed as it was
  unduly restrictive."
  [x & body]
  (if (empty? body)
    x
    `(let [~'<> ~x]
       (__do (-> ~'<> ~(first body))
             ~@(rest body)))))

(defmacro ^:cloaked if
  "If pred is true, thread x through the then form, otherwise through
  the else form.
  (-> 5 (>if should-inc? inc dec))"
  [x pred then else]
  `(let [~'<> ~x]
     (if ~pred
       (__do ~'<> ~then)
       (__do ~'<> ~else))))

(defmacro ^:cloaked let
  "Thread x through body (with bindings available as usual).
  (>let 4 [x 3] (+ x) (- x)) ;; returns 4"
  [x bindings & body]
  `(let [~'<> ~x
         ~@(binding/expand &env bindings)]
     (__do ~'<> ~@body)))

;; Section 2: Macros that access or update the topic.

#?(:clj
   (defn replace-content
     [o n]
     (condp instance? o
       (type n)                           (if (instance? clojure.lang.IObj o)
                                            (with-meta n (meta o))
                                            n)
       clojure.lang.IMapEntry             (vec n)
       clojure.lang.IRecord               (with-meta
                                            (merge o (if (map? n) n
                                                         (into {} (map vec n))))
                                            (meta o))
       clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
       clojure.lang.IPersistentMap        (into (empty o) (map vec n))

       clojure.lang.ISeq                  (with-meta (doall n) (meta o))
       clojure.lang.IPersistentCollection (into (empty o) n)

       clojure.lang.IObj                  (with-meta n (meta o))
       n))

   :cljs
   (defn replace-content
     [o n]
     (cond
      (satisfies? IMapEntry o)      (vec n)
      (satisfies? IRecord o)        (with-meta (merge o (if (map? n) n
                                                            (into {} (map vec n))))
                                      (meta o))
      (satisfies? IList o)          (with-meta (apply list n) (meta o))
      (satisfies? IMap o)           (into (empty o) (map vec n))

      (satisfies? ISeq o)           (with-meta (doall n) (meta o))
      (satisfies? ICollection o)    (into (empty o) n)

      :else n)))

(defmacro ^:cloaked last
    "EXPERIMENTAL Thread the last element of x through body.
  (->/last [1 2 3] inc -) ;; returns [1 2 -4]"
    [x & body]
    `(let [x# ~x]
       (replace-content x# (concat (drop-last 1 x#)
                                   [(__do (last x#) ~@body)]))))

(defn ^:cloaked apply
  "Apply f to x and args."
  [x & f-args]
  (let [[f & args] (concat (drop-last f-args) (last f-args))]
    (apply f x args)))

(defn reset
  "Replace x with y."
  [x y] y)

;; Extra pre/postwalk functions... Not sure why these exist anymore.
(defn map-or-identity [f x]
  (if (coll? x)
    (map f x)
    x))

(defn prewalk [f form]
  ;; (prn :form form)
  (replace-content form (map-or-identity (partial prewalk f) (f form))))

(defn postwalk [f form]
  (f (replace-content form (map-or-identity (partial postwalk f) form))))

;; Decloak any forms that were hidden during macro/fn definition
;; Warning: keep this at the bottom of the namespace.
(decloak do if let)
(decloak last)
(decloak apply)
