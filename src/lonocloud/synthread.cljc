(ns lonocloud.synthread
  (:refer-clojure :exclude [defn defmacro])

  #?(:clj (:require [lonocloud.synthread.binding :as binding]))

  (#?(:clj :require :cljs :require-macros)
     [lonocloud.synthread.cloaking :refer [defn defmacro decloak]]))

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
  (-> 5 (->/if should-inc? inc dec))"
  [x pred then else]
  `(let [~'<> ~x]
     (if ~pred
       (__do ~'<> ~then)
       (__do ~'<> ~else))))

(defmacro ^:cloaked let
  "Thread x through body (with bindings available as usual).
  (-> 4 (->/let [x 3] (+ x) (- x))) ;=> 4"
  [x bindings & body]
  `(let [~'<> ~x
         ~@(binding/expand &env bindings)]
     (__do ~'<> ~@body)))

(defmacro ^:cloaked if-let
  "Thread x through then or else depending on the value of pred. If
  pred is true, bind local to pred.
  (-> {}
    (>if-let [x :bar]
      (assoc :foo x)
      (assoc :was-bar false)))
  ;; returns {:foo :bar}"
  [x [local pred :as binding] then else]
  `(let [~'<> ~x]
     (if-let [~@(binding/expand &env binding)]
       (__do ~'<> ~then)
       (__do ~'<> ~else))))

(defmacro ^:cloaked when
  "If pred is true, thread x through body, otherwise return x unchanged.
  (-> 5 (->/when should-inc? inc))"
  [x pred & body]
  `(let [~'<> ~x]
     (if ~pred
       (__do ~'<> ~@body)
       ~'<>)))

(defmacro ^:cloaked when-not
  "If pred is false, thread x through body, otherwise return x unchanged.
  (-> 5 (->/when-not should-inc? inc))"
  [x pred & body]
  `(let [~'<> ~x]
     (if ~pred
       ~'<>
       (__do ~'<> ~@body))))

(defmacro ^:cloaked when-let
  "If bound values are true in bindings, thread x through the body,
  otherwise return x unchanged.
  (-> 5 (->/when-let [amount (:amount foo)] (+ amount)))"
  [x [local pred :as binding] & forms]
  `(let [~'<> ~x
         ~@(binding/expand &env binding)]
     (if ~local
       (__do ~'<> ~@forms)
       ~'<>)))

(defmacro ^:cloaked cond
  "Thread x through forms in each clause. Return x if no test matches.
  (-> [1 2] (->/cond true (conj 3) false pop))"
  [x & test-form-pairs]
  `(let [~'<> ~x]
     (cond ~@(mapcat (fn [[test form]] `[~test (__do ~'<> ~form)])
                     (partition 2 test-form-pairs))
           :else ~'<>)))

(defmacro ^:cloaked for
  "Thread x through each iteration of body. Uses standard looping
  binding syntax for iterating.
  (-> 4 (->/for [x [1 2 3]] (+ x))) ;=> 10"
  [x seq-exprs & body]
  `(let [box# (atom ~x)
         ~'<> @box#]
     (doseq ~seq-exprs
       (reset! box# (__do @box# ~@body)))
     @box#))

(defmacro ^:cloaked fn
  "Thread x into body of fn. (inspired by Prismatic's fn->).
  (let [add-n (>fn [n] (+ n))]
    (-> 1 (add-n 2))) ;=> 3"
  [args & body]
  `(fn [~'<> ~@args] (__do ~'<> ~@body)))

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

(defmacro ^:cloaked first
  "Thread the first element of x through body.
  (-> [1 2 3] (->/first inc -)) ;=> [-2 2 3]"
  [x & body]
  `(let [x# ~x]
     (replace-content x# (cons (__do (first x#) ~@body)
                               (rest x#)))))
(defmacro ^:cloaked second
  "Thread the second element of x through body.
  (-> [1 2 3] (->/second inc -)) ;=> [1 -3 3]"
  [x & body]
  `(let [x# ~x]
     (replace-content x# (cons (first x#)
                               (cons (__do (second x#) ~@body)
                                     (drop 2 x#))))))

(defmacro ^:cloaked last
  "Thread the last element of x through body.
  (-> [1 2 3] (->/last inc -)) ;=> [1 2 -4]"
    [x & body]
    `(let [x# ~x]
       (replace-content x# (concat (drop-last 1 x#)
                                   [(__do (last x#) ~@body)]))))

(defmacro ^:cloaked nth
  "Thread the nth element of x through body.
  (-> [1 2 3] (->/nth 1 inc -)) ;=> [1 -3 3]"
  [x n & body]
  `(let [x# ~x
         n# ~n]
     (replace-content x# (concat (take n# x#)
                                 (cons (__do (nth x# n#) ~@body)
                                       (drop (inc n#) x#))))))

(defmacro ^:cloaked take
  "Thread the first n elements of x through body.
  (-> [1 2 3] (->/take 2 reverse)) ;=> [2 1 3]"
  [x n & body]
  `(let [x# ~x
         n# ~n]
     (replace-content x# (concat (__do (take n# x#) ~@body)
                                 (drop n# x#)))))

(defmacro ^:cloaked drop
  "Thread all but the first n elements of x through body.
  (-> [1 2 3] (->/drop 1 reverse) ;=> [1 3 2]"
  [x n & body]
  `(let [x# ~x
         n# ~n]
     (replace-content x# (concat (take n# x#)
                                 (__do (drop n# x#) ~@body)))))

(defmacro ^:cloaked butlast
  "Thread all but the last item in x through body."
  [x & body]
  `(let [x# ~x]
     (replace-content x# (concat (__do (drop-last 1 x#) ~@body)
                                 [(last x#)]))))

(defmacro ^:cloaked rest
  "Thread the rest of items in x through body."
  [x & body]
  `(let [x# ~x]
     (replace-content x# (cons (first x#)
                               (__do (rest x#) ~@body)))))

(defmacro ^:cloaked update
  "Thread the value at each key through the pair form where each form
  must be a function which accepts the value of the key as its first
  argument.
  (-> {:a 1} (->/update :a inc)) ;=> {:a 2}"
  [x & key-form-pairs]
  (let [xx (gensym)]
    `(let [~xx ~x]
       (assoc ~xx
         ~@(->> key-form-pairs
                (partition 2)
                (mapcat (fn [[key form]]
                          [key `(__do (get ~xx ~key) ~form)])))))))


(defmacro in
  "Thread the portion of x specified by path through body.
  (>in {:a 1, :b 2} [:a] (+ 2)) ;; = {:a 3, :b 2}"
  [x path & body]
  `(let [x# ~x
         path# ~path
         f# (fn [topic#] (__do topic# ~@body))]
     (if (empty? path#)
       (f# x#)
       (update-in x# path# f#))))

(defmacro as
  "Bind value of x and thread x through body.
   EXPERIMENTALLY supports arbitrary threading form in place of binding form."
  [x binding & body]
  (if (seq? binding)
    `(let [~'<> ~x
           ~(last binding) (-> ~'<> ~(drop-last binding))]
       (__do ~'<> ~@body))
    `(let [~'<> ~x
           ~binding ~'<>]
       (__do ~'<> ~@body))))

(defmacro aside
  "Bind value of x, evaluate unthreaded body and return x."
  [x binding & body]
  `(doto ~x (as ~binding (do ~@body))))

(defmacro side
  "Evaluate unthreaded body and return unchanged x."
  [x & body]
  `(let [~'<> ~x]
     ~@body
     ~'<>))

(defmacro each
  "EXPERIMENTAL Thread each item in x through body."
  [x & body]
  `(let [x# ~x]
     (replace-content x# (map #(__do % ~@body) x#))))

(defmacro each-as
  "EXPERIMENTAL Thread each item in x through body and apply binding to each item."
  [x binding & body]
  `(each ~x (as ~binding ~@body)))

(defmacro ^:cloaked meta
  "Thread meta data on x through body."
  [x & body]
  `(vary-meta ~x (fn [topic#] (__do topic# ~@body))))

(defn ^:cloaked apply
  "Apply f to x and args."
  [x & f-args]
  (let [[f & args] (concat (drop-last f-args) (last f-args))]
    (apply f x args)))

(defn reset
  "Replace x with y."
  [x y] y)

;; Section 3: special binding macros for both updating a subtopic
;; and binding a result based on the subtopic.

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

;; Decloak any forms that were hidden during macro/fn definition
;; Warning: keep this at the bottom of the namespace.
(decloak do if let if-let when when-not when-let cond for fn)
(decloak first second last nth take drop butlast rest update meta)
(decloak apply)
