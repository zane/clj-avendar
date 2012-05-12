(ns clj-avendar.test.core
  (:refer-clojure :exclude [char string])
  (:use [the.parsatron]
        [clj-avendar.core]
        [clojure.test]))

(deftest test-whitespace
  (are [x] (not (nil? (run (whitespace) x)))
       " "
       "\t"
       "\n"
       "\r"))

(deftest test-integer
  (are [x y] (= (run (integer) y) x)
       123 "123"
       0 "0"))

(deftest test-integer
  (are [x y] (= (run (nonzero-integer) y) x)
       123 "123"
       1 "1")
  (is (thrown? RuntimeException
               (run (nonzero-integer "0")))))

(deftest test-tilde-string
  (are [x y] (= (run (tilde-string) y) x)
       "hello" "   hello~"
       "hello" "\thello~"
       "hello" "\nhello~"))

(deftest test-string
  (are [x] (run (string x) x)
       "hello"
       "foo"))

(deftest test-area-name-decl
  (are [x y] (= (:name (run (area-name-decl) x))
                y)
       "Name Var Bandor~" "Var Bandor"))

(deftest test-area
  (let [area (run (area) "#AREADATAName Var Bandor~End")]
    (is (= (:name area)
           "Var Bandor"))))