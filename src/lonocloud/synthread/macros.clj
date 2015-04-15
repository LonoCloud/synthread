(ns lonocloud.synthread.macros
  (:require [lonocloud.synthread.fns :as impl]))

;; Section 0: special syntax support for updating and getting from a
;; sub-path.

(defmacro ^:bind-macro >upget
  "Expand into a threaded update-form followed by a threaded get-form both of
  which act on the value found in the current topic (bound to <>) under
  context. The resulting updated context value is then added back into <> and
  the result of get-form is bound to the bind-label (see below).

  This is a special binding macro that may only be expanded within a bind
  context and, when expanded in a bind context, the assoicated bind-label is
  passed into the macro by the enclosing macro."
  [context update-form get-form]
  (if-let [label (:bind-label (meta &form))]
    `[a# (get ~'<> ~context)
      a# (-> a# ~update-form)
      ~'<> (assoc ~'<> ~context a#)
      ~label (-> a# ~get-form)]
    (throw (Exception. "Unable to expand upget in non-binding block context."))))

(defmacro ^:bind-macro >getup
  "Expand into a threaded get-form followed by a threaded update-form both of
  which act on the value found in the current topic (bound to <>) under
  context. The resulting updated context value is then added back into <> and
  the result of get-form is bound to the bind-label (see below).

  This is a special binding macro that may only be expanded within a bind
  context and, when expanded in a bind context, the assoicated bind-label is
  passed into the macro by the enclosing macro."
  [context get-form update-form]
  (if-let [label (:bind-label (meta &form))]
    `[a# (get ~'<> ~context)
      ~label (-> a# ~get-form)
      a# (-> a# ~update-form)
      ~'<> (assoc ~'<> ~context a#)]
    (throw (Exception. "Unable to expand getup in non-binding block context."))))

(defn- expand-bind-macro
  [[label expr :as binding]]
  (if (and (list? expr)
           (symbol? (first expr))
           (:bind-macro (meta (resolve (first expr)))))
    (macroexpand (vary-meta expr assoc :bind-label label))
    binding))

(defn- expand-bind-macros
  "Look for special 'by' forms in binding pairs to expand them into multiple binding pairs"
  [bindings]
  (->> bindings
       (partition 2)
       (mapcat expand-bind-macro)))

;; Section 1: macros that do not update the topic.
;;            Generally control flow macros.
(defmacro >do
  "Thread x through body. Semantically identical to -> with the
  extra feature that the symbol <> is bound to the new value of x
  between each form.

  Note that the old marking constraint has been removed as it was
  unduly restrictive."
  [x & body]
  (if (empty? body)
    x
    `(let [~'<> ~x]
       (>do (-> ~'<> ~(first body))
              ~@(rest body)))))

(defmacro >if
  "If pred is true, thread x through the then form, otherwise through
  the else form.
  (-> 5 (>if should-inc? inc dec))"
  [x pred then else]
  `(let [~'<> ~x]
     (if ~pred
       (>do ~'<> ~then)
       (>do ~'<> ~else))))

(defmacro >when
  "If pred is true, thread x through body, otherwise return x unchanged.
  (-> 5 (>when should-inc? inc))"
  [x pred & body]
  `(let [~'<> ~x]
     (if ~pred
       (>do ~'<> ~@body)
       ~'<>)))

(defmacro >when-not
  "If pred is false, thread x through body, otherwise return x unchanged.
  (-> 5 (>when should-inc? inc))"
  [x pred & body]
  `(let [~'<> ~x]
     (if ~pred
       ~'<>
       (>do ~'<> ~@body))))

(defmacro >cond
  "EXPERIMENTAL Thread x through forms in each clause. Return x if no test matches.
  (>cond [1 2] true (conj 3) false pop)"
  [x & test-form-pairs]
  `(let [~'<> ~x]
     (cond ~@(mapcat (fn [[test form]] `[~test (>do ~'<> ~form)])
                     (partition 2 test-form-pairs))
           :else ~'<>)))

(defmacro >for
  "Thread x through each iteration of body. Uses standard looping
  binding syntax for iterating.
  (>for 4 [x [1 2 3]] (+ x)) ;; returns 10"
  [x seq-exprs & body]
  `(let [box# (atom ~x)
         ~'<> @box#]
     (doseq ~seq-exprs
       (reset! box# (>do @box# ~@body)))
     @box#))

(defmacro >let
  "Thread x through body (with bindings available as usual).
  (>let 4 [x 3] (+ x) (- x)) ;; returns 4"
  [x bindings & body]
  `(let [~'<> ~x
         ~@(expand-bind-macros bindings)]
     (>do ~'<> ~@body)))

(defmacro >if-let
  "Thread x through then or else depending on the value of pred. If
  pred is true, bind local to pred.
  (-> {}
    (>if-let [x :bar]
      (assoc :foo x)
      (assoc :was-bar false)))
  ;; returns {:foo :bar}"
  [x [local pred :as binding] then else]
  `(let [~'<> ~x]
     (if-let [~@(expand-bind-macros binding)]
       (>do ~'<> ~then)
       (>do ~'<> ~else))))

(defmacro >when-let
  "If bound values are true in bindings, thread x through the body,
  otherwise return x unchanged.
  (-> 5 (>when-let [amount (:amount foo)] (+ amount)))"
  [x [local pred :as binding] & forms]
  `(let [~'<> ~x
         ~@(expand-bind-macros binding)]
     (if ~local
       (>do ~'<> ~@forms)
       ~'<>)))

(defmacro >fn
  "Thread x into body of fn. (inspired by Prismatic's fn->).
  (let [add-n (>fn [n] (+ n))]
    (-> 1 (add-n 2))) ;; returns 3"
  [args & body]
  `(fn [~'<> ~@args] (>do ~'<> ~@body)))

;; Section 2: Macros that access or update the topic.

;; TODO review performance of nth and last.
;; TODO Preserve the type of topic when digging into it with each,
;; each-as, first, second, etc

;; Labeling forms (as, as-do, as-to)
;; +----- label value x
;; | +--- modify value x
;; | | +- thread value x
;; | | |
;; 0 0 0 (doto x (do ...))     ;; for side effects, no access to the topic: (prn "hello")
;; 0 0 1 (doto x (-> ...))     ;; almost but not quite (doto x ...) threading but with chained side-effects?
;; 0 1 0 (do x ...)
;; 0 1 1 (-> x ...)
;; 1 0 0 (>aside x ...)      ;; equivilent to (doto (>as x (do ...)))
;; 1 0 1 (doto x (>as ...))
;; 1 1 0 (>as x (do .... ))  ;; mention in style guide
;; 1 1 1 (>as x ...)

(defmacro >as
  "Bind value of x and thread x through body.
   EXPERIMENTALLY supports arbitrary threading form in place of binding form."
  [x binding & body]
  (if (seq? binding)
    `(let [~'<> ~x
           ~(last binding) (-> ~'<> ~(drop-last binding))]
       (>do ~'<> ~@body))
    `(let [~'<> ~x
           ~binding ~'<>]
       (>do ~'<> ~@body))))

(defmacro >aside
  "Bind value of x, evaluate unthreaded body and return x."
  [x binding & body]
  `(doto ~x (>as ~binding (do ~@body))))

(defmacro >side
  "Evaluate unthreaded body and return unchanged x."
  [x & body]
  `(let [~'<> ~x]
     ~@body
     ~'<>))

(defmacro >first
  "Thread the first element of x through body.
  (>first [1 2 3] inc -) ;; returns [-2 2 3]"
  [x & body]
  `(let [x# ~x]
     (impl/replace-content x# (cons (>do (first x#) ~@body)
                                    (rest x#)))))
(defmacro >second
  "Thread the second element of x through body.
  (>second [1 2 3] inc -) ;; returns [1 -3 3]"
  [x & body]
  `(let [x# ~x]
     (impl/replace-content x# (cons (first x#)
                                    (cons (>do (second x#) ~@body)
                                          (drop 2 x#))))))

(defmacro >nth
  "EXPERIMENTAL Thread the nth element of x through body.
  (>nth [1 2 3] 1 inc -) ;; returns [1 -3 3]"
  [x n & body]
  `(let [x# ~x
         n# ~n]
     (impl/replace-content x# (concat (take n# x#)
                                      (cons (>do (nth x# n#) ~@body)
                                            (drop (inc n#) x#))))))

(defmacro >take
  "EXPERIMENTAL Thread the first n elements of x through body.
  (>take [1 2 3] 2 reverse) ;; returns [2 1 3]"
  [x n & body]
  `(let [x# ~x
         n# ~n]
     (impl/replace-content x# (concat (>do (take n# x#) ~@body)
                                      (drop n# x#)))))

(defmacro >drop
  "EXPERIMENTAL Thread all but the first n elements of x through body.
  (>drop [1 2 3] 1 reverse) ;; returns [1 3 2]"
  [x n & body]
  `(let [x# ~x
         n# ~n]
     (impl/replace-content x# (concat (take n# x#)
                                      (>do (drop n# x#) ~@body)))))

(defmacro >last
  "EXPERIMENTAL Thread the last element of x through body.
  (>last [1 2 3] inc -) ;; returns [1 2 -4]"
  [x & body]
  `(let [x# ~x]
     (impl/replace-content x# (concat (drop-last 1 x#)
                                      [(>do (last x#) ~@body)]))))

(defmacro >butlast
  "EXPERIMENTAL Thread all but the last item in x through body."
  [x & body]
  `(let [x# ~x]
     (impl/replace-content x# (concat (>do (drop-last 1 x#) ~@body)
                                      [(last x#)]))))
(defmacro >rest
  "EXPERIMENTAL Thread the rest of items in x through body."
  [x & body]
  `(let [x# ~x]
     (impl/replace-content x# (cons (first x#)
                                    (>do (rest x#) ~@body)))))

(defmacro >update
  "Thread the value at each key through the pair form.
  The form must be a function which accepts the value of the key as its first
  argument, e.g.

    => (-> {:a 1}
         (>update :a inc))
    {:a 2}"
  [x & key-form-pairs]
  (let [xx (gensym)]
    `(let [~xx ~x]
       (assoc ~xx
         ~@(->> key-form-pairs
                (partition 2)
                (mapcat (fn [[key form]]
                          [key `(>do (get ~xx ~key) ~form)])))))))

(defmacro >in
  "Thread the portion of x specified by path through body.
  (>in {:a 1, :b 2} [:a] (+ 2)) ;; = {:a 3, :b 2}"
  [x path & body]
  `(let [x# ~x
         path# ~path
         f# (fn [topic#] (>do topic# ~@body))]
     (if (empty? path#)
       (f# x#)
       (update-in x# path# f#))))

(defmacro >each
  "EXPERIMENTAL Thread each item in x through body."
  [x & body]
  `(let [x# ~x]
     (impl/replace-content x# (map #(>do % ~@body) x#))))

(defmacro >each-as
  "EXPERIMENTAL Thread each item in x through body and apply binding to each item."
  [x binding & body]
  `(>each ~x (>as ~binding ~@body)))

(defmacro >meta
  "Thread meta data on x through body."
  [x & body]
  `(vary-meta ~x (fn [topic#] (>do topic# ~@body))))
