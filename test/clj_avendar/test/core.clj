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

(deftest test-vnum-decl
  (are [x y] (= (run (area-vnum-decl) x)
                y)
       "VNUMs 1 10" (->NumberRange 1 10)
       "VNUMs   1    10" (->NumberRange 1 10)))


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
Security 1
Areainfo 59
Herbs 0
Weather 2 2 2 1 0
End")]
    (is (= (:name area) "Var Bandor"))
    (is (= (:builders area) "Jolinn        Iandir      Neongrey"))
    (is (= (:credits "Credits [ ALL ] Staff     Var Bandor")))
    (is (= (:vnums area)
           [#clj_avendar.core.NumberRange{:start 11100, :end 11299}
            #clj_avendar.core.NumberRange{:start 22900, :end 22999}
            #clj_avendar.core.NumberRange{:start 22850, :end 22899}
            #clj_avendar.core.NumberRange{:start 3400, :end 3699}]))
    (is (= (:danger area) 1))
    (is (= (:security area) 1))
    (is (= (:info-flags area) 59))
    (is (= (:herbs area) 0))
    (is (= (:weather area)
           #clj_avendar.core.Weather{:base-precip 2,
                                     :base-temp 2,
                                     :base-wind-mag 2,
                                     :base-wind-dir 1,
                                     :geography 0}))))

(deftest test-list-merge
  (is (= (list-merge {:a 1 :b 2}
                     {:a 3})
         {:a [1 3] :b 2})))

(deftest test-named
  (is (= (run (named :int (integer))
              "1234")
         {:int 1234})))

(deftest test-weather-decl
  (is (= (run (area-weather-decl)
              "Weather 2 2 2 1 0")
         #clj_avendar.core.Weather{:base-precip 2,
                                   :base-temp 2,
                                   :base-wind-mag 2,
                                   :base-wind-dir 1,
                                   :geography 0})))

(deftest test-ordered
  (are [x y z] (= (run x y) z
                  [\a \b \c])
       (ordered (char \a) (char \b) (char \c)) "abc" [\a \b \c]))

(deftest test-flag-convert
  (are [chr num] (= (flag-convert chr)
                    num)
       \A 1
       \B 2
       \C 4
       \a 67108864
       \b 134217728
       \c 268435456))

(deftest test-maybe
  (are [expected parser input] (= expected (run parser input))
       \a (maybe \b (char \a)) "a"
       \b (maybe \b (char \a)) "x"))

(deftest test-flag
  (are [expected input] (= expected (run (flag) input))
       1 "1"
       10 "10"
       6 "1|2|3"
       62 "10|21|31"
       10 "A0"
       671088640 "a0"))

(deftest test-alignment
  (are [expected input] (= expected (run (alignment) input))
       :good    "1"
       :neutral "0"
       :evil    "-1"))

(deftest test-number
  (are [expected input] (= expected (run (number) input))
       1  "1"
       0  "0"
       -1 "-1"))

(deftest test-dice
  (is (= #clj_avendar.core.Dice{:number 1, :type 6, :bonus 0}
         (run (dice) "1d6+0")))
  (is (= #clj_avendar.core.Dice{:number 0, :type 4, :bonus 1}
         (run (dice) "0d4+1")))
  (is (= #clj_avendar.core.Dice{:number 10, :type 6, :bonus 20}
         (run (dice) "10d6+20"))))

(deftest test-armor-class
  (is (= #clj_avendar.core.ArmorClass{:pierce 10,
                                      :slash  20,
                                      :bash   30,
                                      :exotic 40}
         (run (armor-class) "1 2 3 4"))))

