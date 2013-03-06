(ns lonocloud.synthread.impl)

(defn iobj? [x]
  (instance? clojure.lang.IObj x))

(defn assert-mark [x val do-asserts file form-num line do-line]
  (when (and do-asserts (not= val (-> x meta ::mark)))
    (let [msg (str "Threaded topic was lost on form " (inc form-num)
                   (when line
                     (str " (line " line ")"))
                   " of ->/do " file ":" do-line)]
      (if-let [ex-info (resolve 'ex-info)]
        (throw (ex-info msg {:file file, :line line, :from-num form-num, :do-line do-line}))
        (throw (Exception. msg)))))
  x)

(defn mark [x val]
  (if (iobj? x)
    (with-meta x {::mark val})
    x))

(defn ^:private map->record [^String typename, data-map]
  (let [nameidx (.lastIndexOf typename ".")
        ns (subs typename 0 nameidx)
        ctor-name (str "map->" (subs typename (inc nameidx)))
        ctor (resolve (symbol ns ctor-name))]
    (if ctor
      (ctor data-map)
      data-map)))

(defn ^:private map-entry [pair]
  (if (vector? pair)
    pair
    (let [[k v] pair]
      (clojure.lang.MapEntry. k v))))

(defmacro compile-if
  "Expands to then-form for versions of clojure at or above
  version-selector, otherwise expands to else-form"
  [compile-time-test then-form else-form]
  (let [test-result (try (eval compile-time-test) (catch Exception e false))]
    ;;(prn :compile-if test-result compile-time-test)
    (if test-result
      then-form
      else-form)))



(def into-does-metadata?
  (:ok (meta (into ^:ok [] [1]))))

(compile-if clojure.lang.IRecord
  (import '(clojure.lang IRecord))
  (do
    (deftype Nothing [])
    (def IRecord Nothing)))

(defn replace-content
  [o n]
  (condp instance? o
    (type n)                           (if (iobj? o) (with-meta n (meta o)) n)
    clojure.lang.IMapEntry             (map-entry n)
    IRecord                            (with-meta (map->record (.getName (class o))
                                                               (if (map? n) n
                                                                   (into {} (map map-entry n))))
                                         (meta o))
    clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
    clojure.lang.IPersistentMap        (compile-if into-does-metadata?
                                                   (into (empty o) (map map-entry n))
                                                   (with-meta (into
                                                               (if (ifn? o)
                                                                 (empty o)  ;; maps that do empty also do ifn
                                                                 {}) ;; defrecord in clojure 1.2?
                                                               (map map-entry n)) (meta o)))
    clojure.lang.ISeq                  (with-meta (doall n) (meta o))
    clojure.lang.IPersistentCollection (compile-if into-does-metadata?
                                                   (into (empty o) n)
                                                   (with-meta (into (empty o) n) (meta o)))
    clojure.lang.IObj                  (with-meta n (meta o))
    Object                             n))

;; examples
(defn ^:private map-or-identity [f x]
  (if (coll? x)
    (map f x)
    x))

(defn prewalk [f form]
  (prn :form form)
  (replace-content form (map-or-identity (partial prewalk f) (f form))))

(defn postwalk [f form]
  (f (replace-content form (map-or-identity (partial postwalk f) form))))

(comment
  "The following protocol, while preferable to the above (closed)
  function requires that Clojure provide a way to resolve which
  interface takes precidence when multiple could apply."

  (defprotocol ReplaceContent
    (replace-content [orig-coll new-seq]))

  (extend-protocol ReplaceContent
    clojure.lang.IPersistentList       (replace-content [o n] (with-meta (apply list n) (meta o)))
    clojure.lang.ISeq                  (replace-content [o n] (with-meta (doall n)      (meta o)))
    clojure.lang.IMapEntry             (replace-content [o n] (vec n))
    clojure.lang.IPersistentCollection (replace-content [o n] (with-meta (into (empty o) n) (meta o))) ;; works around pre-1.5 into+metadata bug
    Object                             (replace-content [o n] n))

  (when-let [irec (resolve 'clojure.lang.IRecord)]
    (extend irec
      ReplaceContent
      {:replace-content (fn [o n] (with-meta
                                    (map->record (.getName (class o)) (into {} n))
                                    (meta o)))}))

  )