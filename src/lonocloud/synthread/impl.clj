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