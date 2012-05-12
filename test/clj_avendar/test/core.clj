(ns clj-avendar.test.core
  (:refer-clojure :exclude [char string])
  (:use [the.parsatron]
        [clj-avendar.core]
        [clojure.pprint]
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
  (are [x y] (= (:names (run (area-name-decl) x))
                y)
       "Name Var Bandor~"
       ["Var Bandor"]))

(deftest test-area-builders-decl
  (are [x y] (= (:builders (run (area-builders-decl) x))
                y)
       "Builders Jolinn        Iandir      Neongrey~"
       ["Jolinn        Iandir      Neongrey"]))

(deftest test-vnum-decl
  (are [x y] (= (run (area-vnum-decl) x)
                y)
       "VNUMs 1 10" {:vnums [(->NumberRange 1 10)]}
       "\nVNUMs   1    10" {:vnums [(->NumberRange 1 10)]}))


(deftest test-area
    (let [area (run (area) "#AREADATA
Name Var Bandor~
Builders Jolinn        Iandir      Neongrey~
Credits [ ALL ] Staff     Var Bandor~
VNUMs 11100 11299
VNUMs 22900 22999
VNUMs 22850 22899
VNUMs 3400 3699
Danger 1
End")]
      (is (= (:name area) "Var Bandor"))
      (is (= (:builders area) "Jolinn        Iandir      Neongrey"))
      (is (= (:credits "Credits [ ALL ] Staff     Var Bandor")))
      (is (= (:vnums area) [#clj_avendar.core.NumberRange{:start 11100, :end 11299}
                            #clj_avendar.core.NumberRange{:start 22900, :end 22999}
                            #clj_avendar.core.NumberRange{:start 22850, :end 22899}
                            #clj_avendar.core.NumberRange{:start 3400, :end 3699}]))
      (is (= (:danger area) 1))))



