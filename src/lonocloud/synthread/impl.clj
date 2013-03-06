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
  (if (instance? java.util.Map$Entry pair)
    pair
    (let [[k v] pair]
      (clojure.lang.MapEntry. k v))))

(defn replace-content
  [o n]
  (condp instance? o
      clojure.lang.IMapEntry             (map-entry n)
      clojure.lang.IRecord               (with-meta (map->record (.getName (class o)) (into {} n)) (meta o))
      clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
      clojure.lang.IPersistentMap        (into (empty o) (map map-entry n))
      clojure.lang.ISeq                  (with-meta (doall n) (meta o))
      clojure.lang.IPersistentCollection (into (empty o) n)
      clojure.lang.IObj                  (with-meta n (meta o))
      Object                             n))

(defn replace-content-pre-1-5
  [o n]
  (condp instance? o
      clojure.lang.IMapEntry             (map-entry n)
      clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
      clojure.lang.IPersistentMap        (with-meta (into (empty o) (map map-entry n)) (meta o))
      clojure.lang.ISeq                  (with-meta (doall n) (meta o))
      clojure.lang.IPersistentCollection (with-meta (into (empty o) n) (meta o))
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