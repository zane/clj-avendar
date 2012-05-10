(ns clj-avendar.test.core
  (:require [net.cgrand.parsley :as parsley])
  (:use [clj-avendar.core])
  (:use [midje.sweet])
  (:use [clojure.pprint]))

(fact "#AREADATA
Name Var Bandor~
Builders Jolinn        Iandir      Neongrey~
VNUMs 11100 11299
VNUMs 22900 22999
VNUMs 22850 22899
VNUMs 3400 3699
Credits [ ALL ] Staff     Var Bandor~
Danger 1
Security 1
Areainfo 59
Herbs 0
Weather 2 2 2 1 0
End" => (parses-to? :area))

(fact "Test~"         => (parses-to? :string)
      "Foo Bar\nBaz~" => (parses-to? :string)
      "a~"            => (parses-to? :string)
  
      "0"    => (parses-to? :integer)
      "1234" => (parses-to? :integer)
  
      "1" => (parses-to? :nzinteger)
      "0" => (complement (parses-to? :nzinteger))
      ""  => (complement (parses-to? :nzinteger))
  
      "  \n \t" => (parses-to? :whitespace))

(fact "#MOBILES\n#0" => (parses-to? :mobiles)
      "V1 2" => (parses-to? :v-prefix)
      "V1 2" => (parses-to? :v-prefix))