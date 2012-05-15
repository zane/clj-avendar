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

(deftest test-word
  (are [expected input] (= expected (run (word) input))
       "jolinn" "jolinn"
       "Jolinn" "Jolinn"
       "JOLINN" "JOLINN"
       "JOLINN" "   JOLINN"
       "rand_prog" "rand_prog"))

(deftest test-mobprog
  (let [long-prog ">rand_prog 50~
if mobvalue(2)==0
  if iscarrying($i)==986
  or iscarrying($i)==3516
  or iscarrying($i)==3517
  or iscarrying($i)==6929
  or iscarrying($i)==6930
  or iscarrying($i)==7437
  or iscarrying($i)==8106
  or iscarrying($i)==8803
  or iscarrying($i)==15050
  or iscarrying($i)==19044
  or iscarrying($i)==19090
  or iscarrying($i)==19102
  or iscarrying($i)==21423
  or iscarrying($i)==22831
  or iscarrying($i)==23805
  or iscarrying($i)==24048
  or iscarrying($i)==25254
    eat bread
    break
  endif
  if isanyobjhere()
    if objhere(986)
    or objhere(3516)
    or objhere(3517)
    or objhere(6929)
    or objhere(6930)
    or objhere(7437)
    or objhere(8106)
    or objhere(8803)
    or objhere(15050)
    or objhere(19044)
    or objhere(19090)
    or objhere(19102)
    or objhere(21423)
    or objhere(22831)
    or objhere(23805)
    or objhere(24048)
    or objhere(25254)
      mpecho $I darts over to the bread on the ground and quickly eats it up.
      mppurge bread
      break
    endif
  endif
else
endif
mpvaluerand 1 1 100
if mobvalue(1)==1
  mpvaluerand 1 1 3
  if mobvalue(1) == 1
    if mobvalue(2) == 0
      mpfocus $r
      if iscarrying($f)==986
      or iscarrying($f)==3516
      or iscarrying($f)==3517
      or iscarrying($f)==6929
      or iscarrying($f)==6930
      or iscarrying($f)==7437
      or iscarrying($f)==8106
      or iscarrying($f)==8803
      or iscarrying($f)==15050
      or iscarrying($f)==19044
      or iscarrying($f)==19090
      or iscarrying($f)==19102
      or iscarrying($f)==21423
      or iscarrying($f)==22831
      or iscarrying($f)==23805
      or iscarrying($f)==24048
      or iscarrying($f)==25254
        mpechoat $f $I approaches you tentatively, eyeing your bread.
        mpechoaround $f $I approaches $F tentatively, begging for bread.
        mpunfocus
      endif
    endif
  endif
  if mobvalue(1) == 2
    if mobvalue(2) == 0
      mpecho $I coos softly.
    endif
  endif
  if mobvalue(1) == 3
    if mobvalue(2) == 0
      mpecho $I is startled, and flies up over the city.
      mpwizi
      mpvalueset 2 1
    else
      mpunwizi
      mpecho $I lands and begins walking about, looking for food.
      mpvalueset 2 0
    endif
  endif
endif
~"]
    (is (= (:type (run (mobprog) ">rand_prog 50~lines~")) "rand_prog"))
    (is (= (:type (run (mobprog) long-prog) "rand_prog")))
    ))
