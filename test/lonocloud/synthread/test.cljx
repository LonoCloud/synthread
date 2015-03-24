(ns lonocloud.synthread.test
  #+clj
  (:require [lonocloud.synthread :refer :all]
            [clojure.test :refer [deftest is]])
  #+cljs
  (:require-macros [lonocloud.synthread.test-macros :refer [->is]]
                   [lonocloud.synthread :refer
                    [>do >if >let >if-let >when >when-not >when-let >for >first
                     >second >nth >take >drop >last >butlast >rest >update >let
                     >fn >as >aside >side >each >each-as >cond >in]])
  (:require [lonocloud.synthread :refer [>reset >apply]]
            [cljs.test :refer-macros [deftest is]]))

#+clj
(defmacro ->is
  ([x op]
   (let [xx '<topic>]
     `(let [~xx ~x]
        (is (~op ~xx))
        ~xx)))
  ([x binop v]
   (let [xx '<topic>]
     `(let [~xx ~x]
        (is (~binop ~xx ~v))
        ~xx))))

(deftest test-do
  (is (= (-> 0 inc (+ 2) (- 1 1))
         (>do 0 inc (+ 2) (- 1 1)))))

(deftest test-<>
  (>do {:a 1 :b 2}
         (assoc :a (:b <>))
         (->is = {:a 2 :b 2})))

;; Testing flow control
(deftest test-if
  (-> 0
      (>if true inc dec)
      (->is = 1)
      (>if false inc dec)
      (->is = 0)))

(deftest test-if-let
  (>do {}
    (>let [x :orig]
      (>if-let [x :foo]
        (assoc :yes x)
        (assoc :no x))
      (->is = {:yes :foo})
      (>if-let [x false]
        (assoc :yes x)
        (assoc :no x))
      (->is = {:yes :foo, :no :orig}))))

(deftest test-when
  (-> 0
      (>when false
               (->is = :never-reached))
      (->is = 0)
      (>when true
               (->is = 0)
               inc
               (->is = 1))
      (->is = 1)))

(deftest test-when-not
  (-> 0
      (>when-not true
                   (->is = :never-reached))
      (->is = 0)
      (>when-not false
                   (->is = 0)
                   inc
                   (->is = 1))
      (->is = 1)))

(deftest test-when-let
  (-> 0
      (>when-let [x 2]
                   (+ x))
      (->is = 2)
      (>when-let [y nil]
                   (>reset y))
      (->is = 2)))

(deftest test-cond
  (-> 0
      (>cond
        false (->is = :never-reached)
        true inc)
      (->is = 1)
      (>cond
       false dec)
      (->is = 1)))

(deftest test-for
  (-> 0
      (>for [n (range)
               :when (pos? n)
               :while (< n 5)]
              (+ n))
      (->is = 10)))

(deftest test-first
  (>do (range 4)
    (>first
      (->is = 0)
      inc
      inc)
    (->is = [2 1 2 3]))
  (>do {:a 1, :b 2, :c 3}
    (>first reverse)
    (->is #{{:a 1, :b 2, 3 :c}
            {1 :a, :b 2, :c 3}
            {:a 1, 2 :b, :c 3}})))

(deftest test-second
  (>do (range 4)
    (>second
      (->is = 1)
      inc
      inc)
    (->is = [0 3 2 3]))
  (>do {1 10, 2 20, 3 30}
    (>each
      (>second inc))
    (->is = {1 11, 2 21, 3 31})))

(deftest test-nth
  (>do (range 4)
    (>nth 2
      (->is = 2)
      inc
      inc)
    (->is = [0 1 4 3])))

(deftest test-take
  (>do (range 4)
         (>take 2
                  (->is = [0 1])
                  reverse)
         (->is = [1 0 2 3])))

(deftest test-drop
  (>do (range 4)
         (>drop 2
                  (->is = [2 3])
                  reverse)
         (->is = [0 1 3 2])))

(deftest test-last
  (>do (range 4)
    (>last
      (->is = 3)
      inc
      inc)
    (->is = [0 1 2 5])))

(deftest test-butlast
  (>do (range 4)
    (>butlast
      (->is = [0 1 2])
      rest)
    (->is = [1 2 3])))

(deftest test-rest
  (>do (range 4)
         (>rest
          (->is = [1 2 3])
          rest)
         (->is = [0 2 3])))

(deftest test-update
  (>do {:a 1 :b 2}
      (>update
       :a dec
       :b (* 2))
      (->is = {:a 0 :b 4})))

(deftest test-in
  (>do {:a {:b {:c 10}}}
      (>in [:a :b :c]
             (/ 2)
             inc
             (->is = 6))
      (->is = {:a {:b {:c 6}}})))

(deftest test-let
  (-> 1
      (>let [a 3
               b 5]
              (+ a b))
      (->is = 9)))

(deftest test-fn
  (let [add-n (>fn [n] (+ n))]
    (-> 1
      (add-n 2)
      (->is = 3))))

(deftest test-as
  (-> 10
      (>as ten
             (+ ten))
      (->is = 20)))

(deftest test-as-with-arrow
  (>do {:a {:delta 1} :b 2}
      (>as (-> :a :delta delta)
             (>update :b (+ delta)))
      (->is = {:a {:delta 1} :b 3})))

(deftest test-aside
  (-> 10
      (>aside ten
                (is (= ten 10))
                :ignored)
      (->is = 10)))

(deftest test-side
  (-> 10
      (>side (inc <>))
      (->is = 10)))

(defrecord R [a b c])

(deftest test-each
  (-> (range 5)
    (>each (* 2))
    (->is = [0 2 4 6 8]))

  (>do {"a" 1 "b" 2}
    (>each
     (>first keyword)
     (>second -))
    (->is = {:a -1 :b -2}))

  (>do [0 1 2 3 4]
    (>each (* 2))
    (->is = [0 2 4 6 8]))

  (>do {1 2, 3 4, 5 6}
    (>each reverse)
    (->is = {2 1, 4 3, 6 5}))

  (>do {1 2, 3 4, 5 6}
    (>each
     (>update 0 (* 10)
               1 (+ 10)))
    (->is = {10 12, 30 14, 50 16}))

  (>do (R. 1 2 3)
    (>each
     (>second inc))
    (->is = (R. 2 3 4)))
  (-> nil
    (>each inc)
    (->is = ())))

(deftest test-each-as
  (-> (range 5)
      (>each-as x
                  (+ x))
      (->is = [0 2 4 6 8]))

  (>do {1 2, 3 4, 5 6}
    (>each-as [k v]
      (do [v k]))
    (->is = {2 1, 4 3, 6 5}))

  (>do {1 2, 3 4, 5 6}
    (>each-as [k v]
      (do [(* 10 k) (+ 10 v)]))
    (->is = {10 12, 30 14, 50 16})))

(deftest test-reset
  (-> 0
      (>reset 5)
      (->is not= 0)
      (->is = 5)))

(deftest test-apply
  (-> 10
      (>apply + [1 2])
      (->is = 13)))
