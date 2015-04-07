(ns lonocloud.synthread.cljs-is-macro
  (:require [cljs.test :refer [deftest is]]))

(defmacro ->is
  ([x op & vs]
     (let [xx '<topic>]
       `(let [~xx ~x]
          (is (~op ~xx ~@vs))
          ~xx))))
