(ns lonocloud.suspenders
  (:refer-clojure :exclude [defn defmacro])
  (:require [lonocloud.synthread :as ->])
  (#?(:clj :require :cljs :require-macros)
     [lonocloud.synthread.cloaking :refer [defn defmacro decloak]]))

;; Syntax support for suspendable threaded forms. This provides the
;; ability for a call stack to suspend work by defining a mirrored
;; callback stack containing the remaining work.

(comment
  "This is a sample data structure for capturing suspensions."
  ( ;; suspend-stack
   { ;; current suspension map
     id4 (fn callback [topic value] ...) ;; callback keyed by id
     id5 (fn callback [topic value] ...) ;; callback keyed by id
   },

   { ;; stashed block map
     id1 (fn ...)
     id2 (fn ...)
     id3 (fn ...)
   })


  (defn combine [state]
    (-> state
        pop
        (add-blocks (first state))))

  (defn clear [state]
    '({}))

  (def current first))

(comment

  (concat [(fn [x] (-> x (+ 5))) (fn [x] (-> x (- 10)))] suspender-tail)

  (defn foobar [topic]
    (>do topic
         (let [^{:tail ...} tail nil]
           ;; define a 'tail'
           (assoc :a (>do 10 (+ 1)))

           (>do
            (let [^{:tail ...} tail nil])
            ;; expand the 'tail'
            (assoc :b 2))

           (dissoc :a)))))

(def sample-state
  {:active {} ;; suspensions that are on
   :proposed {} ;; suspensions that may occur
   :stashed ()}) ;; proposals that have been stashed for now

(defn suspender-macro?
  "Return true if the unexpanded form is a suspender-macro."
  [form]
  (and (list? form)
       (symbol? (first form))
       (:suspender (meta (resolve (first form))))))

(defn suspended?
  "Return true if topic has been suspended."
  [topic]
  (-> topic meta ::state :proposed empty? not))

(defn already-wrapped?
  "Return true if topic has already been wrapped by a nested
   suspender-macro."
  [topic fn-name]
  (= fn-name
     ;; dig into the state and look up the function name of the first
     ;; callback that you find.
     (-> topic meta ::state :proposed first second meta ::name)))

(defmacro ->suspender-state
  "Convenience macro to update state like synthread."
  [topic & body]
  `(->/do ~topic
          (->/meta
           (->/update ::state
                      (->/do ~@body)))))

(defn wrap
  "Wrap each of the proposed suspension callbacks."
  [topic wrap-fn]
  (->suspender-state topic
    (->/update :proposed
      (->/each ;suspension-item
        (->/second ;suspension-callback
          wrap-fn)))))

(defn suspend
  "Define a new proposed suspension entry. callback-fn must be able
   take the topic and result as parameters."
  [topic id callback-fn]
  (->suspender-state topic
    (->/update :proposed
      (assoc id callback-fn))))

(defn stash
  "Stash the proposed suspesion map."
  [topic]
  (->suspender-state topic
    (->/let [proposed-map (:proposed <>)]
      (->/update
        :proposed empty
        :stashed (conj proposed-map)))))

(defn unstash
  "Merge the most recently stashed suspension map into the active
   suspension map."
  [topic]
  (->suspender-state topic
    (->/let [stashed-map (first (:stashed <>))]
      (->/update
        :stashed pop
        :proposed (merge stashed-map)))))

(defn reset-suspensions
  "Remove proposed and stashed proposals."
  [topic]
  (->suspender-state topic
    (->/update
      :proposed empty
      :stashed empty)))

(defn activate-suspensions
  "Merge proposed suspensions into active suspensions and clear any
   stashed suspesions."
  [topic]
  (->suspender-state topic
    (->/let [proposed-map (:proposed <>)]
      (->/update
        :active (merge proposed-map)
        :proposed empty
        :stashed empty))))

(defn resume
  "Resume by invoking the callback keyed by id in the active
   suspension map. Both topic and result are pased to the callback."
  [topic id result]
  (let [callback (-> topic meta ::state :active (get id))]
    (->/do topic
        ;; TODO clean up the active suspensions map
        ;; 1. dissoc the entry with the matching id
        ;; 2. dissoc all pending related suspensions that 'lost.
           #_(->/side (clojure.pprint/pprint {:state (::state (meta topic))
                                            :callback callback
                                            :result result}))
           (callback result))))

(defn add-context
  "Add suspender context to new-form."
  [new-form old-form & [tail-root]]
  (if (suspender-macro? new-form)
    (vary-meta new-form
               assoc
               ::stack (::stack (meta old-form))
               ::name (::name (meta old-form)))
    new-form))

;; 1 The do macro does several things: redefines tail, checks the
;;   topic for suspensions, checks the suspensions for nested callback
;;   and add callback (using tail) if not present, and stops executing
;;   if suspended.
(defmacro ^:suspender ^:cloaked do
  ([topic] topic)
  ([topic form & forms]
     (let [stack    (or (::stack (meta &form)) '[])
           cmd      (or (::cmd (meta &form)) (first &form))
           name     (or (::name (meta &form)) (gensym "resume-"))
           form (if (suspender-macro? form)
                    (vary-meta form
                               assoc
                               ::stack (cons `(~cmd ~@forms) stack)
                               ::cmd (first &form) ;; default to `do
                               ::name name)
                    form)]
       #_(clojure.pprint/pprint {:form &form
                               :meta (meta &form)})
       `(let [~'<> ~topic
              _# (assert (not (suspended? ~'<>))) ;; not strictly necessary
              ~'<> (-> ~'<> ~form)]
          (if (suspended? ~'<>)
            (if (already-wrapped? ~'<> '~name)
              ~'<>
              (wrap ~'<>
                    ~(or (::recur (meta &form))
                       (let [callback-arg (gensym "callback-")
                             result-arg (gensym "result-")
                             wrap-form
                             `(fn ~name [~callback-arg]
                                ^{::name '~name}
                                (fn [t# ~result-arg]
                                  (-> t#
                                      ~(reduce (fn [f [cmd & body]]
                                                 `(~cmd ~f ~@body))
                                               `^{::recur ~name}
                                               (~cmd (~callback-arg ~result-arg)
                                                     ~@forms)
                                               stack))))
                             ]
                         #_(clojure.pprint/pprint {:wrap-form wrap-form})
                         wrap-form
                         ))))
            ^{::stack ~stack
              ::cmd ~cmd
              ::name ~name}
            (__do ~'<> ~@forms))))))

(defmacro ^:suspender ^:cloaked if
  "threaded, suspended if."
  [topic pred then & [else]]
  `(let [~'<> ~topic]
     (if ~pred
       ~(add-context `(__do ~'<> ~then) &form)
       ~(add-context `(__do ~'<> ~else) &form))))

#?(:clj
   (defn replace-content
     [n o]
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
     [n o]
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

(defmacro ^:suspender ^:cloaked first
  ""
  [topic & body]
  (let [{stack ::stack
         name ::name
         recur ::recur} (meta &form)
         cmd (first &form)]
    #_(when recur (println "recurred!" recur))
    `(let [topic# ~topic]
       (-> (first topic#)
           ^{::stack ~stack
             ::cmd ~cmd
             ::name ~name
             ::recur ~recur}
           (__do ~@body)
           ;; propogate subtopic's suspension to supertopic
           #_(->/side
            (println (suspended? ~'<>)
                    (already-wrapped? ~'<> '~name)
                    (::state (meta ~'<>))
                    "\n"
                    (meta (:id (:proposed (::state (meta ~'<>)))))))
           (->/let [substate# (::state (meta ~'<>))]
             (vary-meta dissoc ::state)
             (cons (rest topic#))
             (replace-content topic#)
             (vary-meta assoc ::state substate#)))
       )))

(defmethod print-method clojure.lang.Fn [o w]
  (binding [*out* w]
    (print (format "#<fn %s 0x%x" (.getName (class o)) (hash o)))))

(decloak do if first)
