(ns lonocloud.synthread
  (:refer-clojure :exclude [defn defmacro])

  #?(:clj (:require [lonocloud.synthread.binding :as binding]))

  (#?(:clj :require :cljs :require-macros)
     [lonocloud.synthread.cloaking :refer [defn defmacro decloak]]))

;; Section 1: macros that do not update the topic.
;;            Generally control flow macros.
(defmacro ^:cloaked do
  "Thread topic through body. Semantically identical to -> with the
  extra feature that the symbol <> is bound to the new value of topic
  between each form.
  (->/do 3 inc) ;=> 4"
  [topic & body]
  (if (empty? body)
    topic
    `(let [~'<> ~topic]
       (__do (-> ~'<> ~(first body))
             ~@(rest body)))))

(defmacro ^:cloaked if
  "If pred is true, thread topic through the then form, otherwise
  through the else form.
  (->/do 5 (->/if true inc dec)) ;=> 6"
  [topic pred then else]
  `(let [~'<> ~topic]
     (if ~pred
       (__do ~'<> ~then)
       (__do ~'<> ~else))))

(defmacro ^:cloaked let
  "Thread topic through body (with bindings available as usual).
  (->/do 4 (->/let [x 3] (+ x) (- x))) ;=> 4"
  [topic bindings & body]
  `(let [~'<> ~topic
         ~@(binding/expand &env bindings)]
     (__do ~'<> ~@body)))

(defmacro ^:cloaked if-let
  "Thread topic through then or else depending on the value of pred.
  If pred is true, bind local to pred.
  (->/do {}
    (>if-let [x :bar]
      (assoc :foo x)
      (assoc :was-bar false)))
  ;=> {:foo :bar}"
  [topic [local pred :as binding] then else]
  `(let [~'<> ~topic]
     (if-let [~@(binding/expand &env binding)]
       (__do ~'<> ~then)
       (__do ~'<> ~else))))

(defmacro ^:cloaked when
  "If pred is true, thread topic through body, otherwise return topic
  unchanged.
  (->/do 5 (->/when true inc)) ;=> 6"
  [topic pred & body]
  `(let [~'<> ~topic]
     (if ~pred
       (__do ~'<> ~@body)
       ~'<>)))

(defmacro ^:cloaked when-not
  "If pred is false, thread topic through body, otherwise return topic
  unchanged.
  (->/do 5 (->/when-not false inc)) ;=> 6"
  [topic pred & body]
  `(let [~'<> ~topic]
     (if ~pred
       ~'<>
       (__do ~'<> ~@body))))

(defmacro ^:cloaked when-let
  "If value bound to label is true in binding, thread topic through
  the body, otherwise return topic unchanged.
  (->/do 6 (->/when-let [amount 4] (+ amount))) ;=> 10"
  [topic [label :as binding] & forms]
  `(let [~'<> ~topic
         ~@(binding/expand &env binding)]
     (if ~label
       (__do ~'<> ~@forms)
       ~'<>)))

(defmacro ^:cloaked cond
  "Thread topic through forms in test-pairs. Return topic if no test
  matches.
  (->/do [1 2] (->/cond true (conj 3) false pop)) ;=> [1 2 3]"
  [topic & test-pairs]
  `(let [~'<> ~topic]
     (cond ~@(mapcat (fn [[test form]] `[~test (__do ~'<> ~form)])
                     (partition 2 test-pairs))
           :else ~'<>)))

(defmacro ^:cloaked for
  "Thread topic through each iteration of body. Uses standard looping
  binding syntax for iterating.
  (->/do 4 (->/for [x [1 2 3]] (+ x))) ;=> 10"
  [topic seq-exprs & body]
  `(let [box# (atom ~topic)
         ~'<> @box#]
     (doseq ~seq-exprs
       (reset! box# (__do @box# ~@body)))
     @box#))

(defmacro ^:cloaked fn
  "Thread topic into body of fn. (inspired by Prismatic's fn->).
  (let [add-n (->/fn [n] (+ n))]
    (->/do 1 (add-n 2))) ;=> 3"
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
  "Thread the first element of topic through body.
  (->/do [1 2 3] (->/first inc -)) ;=> [-2 2 3]"
  [topic & body]
  `(let [topic# ~topic]
     (replace-content topic# (cons (__do (first topic#) ~@body)
                                   (rest topic#)))))
(defmacro ^:cloaked second
  "Thread the second element of topic through body.
  (->/do [1 2 3] (->/second inc -)) ;=> [1 -3 3]"
  [topic & body]
  `(let [topic# ~topic]
     (replace-content topic# (cons (first topic#)
                                   (cons (__do (second topic#) ~@body)
                                         (drop 2 topic#))))))

(defmacro ^:cloaked last
  "Thread the last element of topic through body.
  (->/do [1 2 3] (->/last inc -)) ;=> [1 2 -4]"
    [topic & body]
    `(let [topic# ~topic]
       (replace-content topic# (concat (drop-last 1 topic#)
                                       [(__do (last topic#) ~@body)]))))

(defmacro ^:cloaked nth
  "Thread the nth element of topic through body.
  (->/do [1 2 3] (->/nth 1 inc -)) ;=> [1 -3 3]"
  [topic n & body]
  `(let [topic# ~topic
         n# ~n]
     (replace-content topic# (concat (take n# topic#)
                                     (cons (__do (nth topic# n#) ~@body)
                                           (drop (inc n#) topic#))))))

(defmacro ^:cloaked take
  "Thread the first n elements of topic through body.
  (->/do [1 2 3] (->/take 2 reverse)) ;=> [2 1 3]"
  [topic n & body]
  `(let [topic# ~topic
         n# ~n]
     (replace-content topic# (concat (__do (take n# topic#) ~@body)
                                     (drop n# topic#)))))

(defmacro ^:cloaked drop
  "Thread all but the first n elements of topic through body.
  (->/do [1 2 3] (->/drop 1 reverse) ;=> [1 3 2]"
  [topic n & body]
  `(let [topic# ~topic
         n# ~n]
     (replace-content topic# (concat (take n# topic#)
                                     (__do (drop n# topic#) ~@body)))))

(defmacro ^:cloaked butlast
  "Thread all but the last item in topic through body.
  (->/do [1 2 3] (->/butlast reverse)) ;=> [2 1 3]"
  [topic & body]
  `(let [topic# ~topic]
     (replace-content topic# (concat (__do (drop-last 1 topic#) ~@body)
                                     [(last topic#)]))))

(defmacro ^:cloaked rest
  "Thread the rest of items in topic through body.
  (->/do [1 2 3] (->/rest reverse)) ;=> [1 3 2]"
  [topic & body]
  `(let [topic# ~topic]
     (replace-content topic# (cons (first topic#)
                               (__do (rest topic#) ~@body)))))

(defmacro ^:cloaked update
  "Thread the value at each key of topic through the pair form where
  each form must be a function which accepts the value of the key as
  its first argument.
  (->/do {:a 1} (->/update :a inc)) ;=> {:a 2}"
  [topic & key-form-pairs]
  (let [topic2 (gensym)]
    `(let [~topic2 ~topic]
       (assoc ~topic2
         ~@(->> key-form-pairs
                (partition 2)
                (mapcat (fn [[key form]]
                          [key `(__do (get ~topic2 ~key) ~form)])))))))


(defmacro in
  "Thread the portion of topic specified by path through body.
  (->/do {:a 1, :b 2} (->/in [:a] (+ 2))) ;=> {:a 3, :b 2}"
  [topic path & body]
  `(let [topic# ~topic
         path# ~path
         f# (fn [topic#] (__do topic# ~@body))]
     (if (empty? path#)
       (f# topic#)
       (update-in topic# path# f#))))

(defmacro as
  "Bind value of topic and thread topic through body.
   EXPERIMENTALLY supports arbitrary threading form in place of binding form.
  (->/do {} (->/as old-v (assoc :foo (count old-v)))) ;=> {:foo 0}"
  [topic binding & body]
  (if (seq? binding)
    `(let [~'<> ~topic
           ~(last binding) (-> ~'<> ~(drop-last binding))]
       (__do ~'<> ~@body))
    `(let [~'<> ~topic
           ~binding ~'<>]
       (__do ~'<> ~@body))))

(defmacro aside
  "Bind value of topic, evaluate unthreaded body and return topic.
  (->/do 4 (->/aside v (println v))) ;=> 4 (and prints 4)"
  [topic binding & body]
  `(doto ~topic (as ~binding (do ~@body))))

(defmacro side
  "Evaluate unthreaded body and return unchanged topic.
  (->/do 4 (->/side (println \"hello\"))) ;=> 4 (and prints hello)"
  [topic & body]
  `(let [~'<> ~topic]
     ~@body
     ~'<>))

(defmacro each
  "EXPERIMENTAL Thread each item in topic through body.
  (->/do [1 2 3] (->/each inc)) ;=> [2 3 4]"
  [topic & body]
  `(let [topic# ~topic]
     (replace-content topic# (map #(__do % ~@body) topic#))))

(defmacro each-as
  "EXPERIMENTAL Thread each item in topic through body and apply
  binding to each item.
  (->/do [1 2 3] (->/each-as i (* i))) ;=> [1 4 9]"
  [topic binding & body]
  `(each ~topic (as ~binding ~@body)))

(defmacro ^:cloaked meta
  "Thread meta data on topic through body.
  (->/do [1 2 3] (->/meta (assoc :foo true))) ;=> ^:foo [1 2 3]"
  [topic & body]
  `(vary-meta ~topic (fn [topic#] (__do topic# ~@body))))

(defn ^:cloaked apply
  "Apply f to topic and args.
  (->/do [1 2 3] (->/apply conj [4 5 6])) ;=> [1 2 3 4 5 6]"
  [topic & f-args]
  (let [[f & args] (concat (drop-last f-args) (last f-args))]
    (apply f topic args)))

(defn reset
  "Replace topic with new-topic.
  (->/do 4 (->/reset 5)) ;=> 5"
  [topic new-topic] new-topic)

;; Section 3: special binding macros for both updating a subtopic
;; and binding a result based on the subtopic.

(defmacro ^:binding upget
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

(defmacro ^:binding getup
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
