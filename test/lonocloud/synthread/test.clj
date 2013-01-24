(ns lonocloud.synthread.test
  (:require [lonocloud.synthread :as ->])
  (:use [clojure.test :only [deftest is]]))

(defmacro ->is [x binop v]
  (let [xx '<topic>]
    `(let [~xx ~x]
       (is (~binop ~xx ~v))
       ~xx)))

(deftest test-if
  (-> 0
      (->/if true inc dec)
      (->is = 1)
      (->/if false inc dec)
      (->is = 0)))

(deftest test-when
  (-> 0
      (->/when false
               (->is = :never-reached))
      (->is = 0)
      (->/when true
               (->is = 0)
               inc
               (->is = 1))
      (->is = 1)))

(deftest test-when-not
  (-> 0
      (->/when-not true
                   (->is = :never-reached))
      (->is = 0)
      (->/when-not false
                   (->is = 0)
                   inc
                   (->is = 1))
      (->is = 1)))

(deftest test-when-let
  (-> 0
      (->/when-let [x 2]
                   (+ x))
      (->is = 2)
      (->/when-let [y nil]
                   (->/reset y))
      (->is = 2)))

(deftest test-cond
  (-> 0
      (->/cond
        false (->is = :never-reached)
        true inc)
      (->is = 1)
      (->/cond
       false dec)
      (->is = 1)))

(deftest test-for
  (-> 0
      (->/for [n (range)
               :when (pos? n)
               :while (< n 5)]
              (+ n))
      (->is = 10)))

(deftest test-first
  (-> (range 4)
      (->/first (->is = 0)
                inc
                inc)
      (->is = [2 1 2 3])))

(deftest test-second
  (-> (range 4)
      (->/second (->is = 1)
                 inc
                 inc)
      (->is = [0 3 2 3])))

(deftest test-nth
  (-> (range 4)
      (->/nth 2 (->is = 2)
                 inc
                 inc)
      (->is = [0 1 4 3])))

(deftest test-last
  (-> (range 4)
      (->/last (->is = 3)
                 inc
                 inc)
      (->is = [0 1 2 5])))

(deftest test-rest
  (-> (range 4)
      (->/rest (->is = [1 2 3])
                 rest)
      (->is = [0 2 3])))

(deftest test-assoc
  (-> {:a 1 :b 2}
      (->/assoc
       :a dec
       :b (* 2))
      (->is = {:a 0 :b 4})))

(deftest test-in
  (-> {:a {:b {:c 10}}}
      (->/in [:a :b :c]
             (/ 2)
             inc
             (->is = 6))
      (->is = {:a {:b {:c 6}}})))

(deftest test-keys
  (-> {1 10, 2 20, 3 30}
      (->/keys (->> (map inc)))
      (->is = {2 10, 3 20, 4 30})))

(deftest test-vals
  (-> {1 10, 2 20, 3 30}
      (->/vals (->> (map inc)))
      (->is = {1 11, 2 21, 3 31})))

(deftest test-let
  (-> 1
      (->/let [a 3
               b 5]
              (+ a b))
      (->is = 9)))

(deftest test-as
  (-> 10
      (->/as ten
             (+ ten))
      (->is = 20)))

(deftest test-as-with-arrow
  (-> {:a {:delta 1} :b 2}
      (->/as (-> :a :delta delta)
             (->/assoc :b (+ delta)))
      (->is = {:a {:delta 1} :b 3})))

(deftest test-aside
  (-> 10
      (->/aside ten
                (is (= ten 10))
                :ignored)
      (->is = 10)))

(deftest test-each
  (-> (range 5)
      (->/each (* 2))
      (->is = [0 2 4 6 8])))

(deftest test-each-as
  (-> (range 5)
      (->/each-as x
                  (+ x))
      (->is = [0 2 4 6 8])))

(deftest test-reset
  (-> 0
      (->/reset 5)
      (->is not= 0)
      (->is = 5)))

(deftest test-apply
  (-> 10
      (->/apply + [1 2])
      (->is = 13)))
