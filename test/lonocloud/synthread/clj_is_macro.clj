(ns lonocloud.synthread.clj-is-macro
  (:require [clojure.test :refer [deftest is]]))

(defmacro ->is
  ([x op & vs]
     (let [xx '<topic>]
       `(let [~xx ~x]
          (is (~op ~xx ~@vs))
          ~xx))))
