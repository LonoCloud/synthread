(ns lonocloud.synthread.impl)

(defn iobj? [x]
  (instance? clojure.lang.IObj x))

(defn ^:private map-entry [pair]
  (if (vector? pair)
    pair
    (let [[k v] pair]
      (clojure.lang.MapEntry. k v))))

(defn replace-content
  [o n]
  (condp instance? o
    (type n)                           (if (iobj? o) (with-meta n (meta o)) n)
    clojure.lang.IMapEntry             (map-entry n)
    clojure.lang.IRecord               (with-meta (merge o (if (map? n) n
                                                               (into {} (map map-entry n))))
                                         (meta o))
    clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
    clojure.lang.IPersistentMap        (into (empty o) (map map-entry n))

    clojure.lang.ISeq                  (with-meta (doall n) (meta o))
    clojure.lang.IPersistentCollection (into (empty o) n)

    clojure.lang.IObj                  (with-meta n (meta o))
    n))

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
