(ns clj-avendar.core
  (:require [net.cgrand.parsley :as parsley]))

(defn parse
  [tag str]
  (let [parse (parsley/parser
               {:main tag
                :space :whitespace?
                :make-node (fn [tag content] {:tag tag :content content})
                ;; :make-unexpected (fn [content] (throw (Exception. content)))
                }

               :area ["#AREADATA" :decl* "End"]
               :decl- #{:name-decl
                        :builders-decl
                        :vnums-decl
                        :credits-decl
                        :danger-decl
                        :security-decl
                        :areainfo-decl
                        :herbs-decl
                        :weather-decl}
               :name-decl     ["Name" :string]
               :builders-decl ["Builders" :string]
               :vnums-decl    ["VNUMs" :integer :integer]
               :credits-decl  ["Credits" :string]
               :danger-decl   ["Danger" :integer]
               :security-decl ["Security" :integer]
               :areainfo-decl ["Areainfo" :integer]
               :herbs-decl    ["Herbs" :integer]
               :weather-decl  ["Weather" :integer :integer :integer :integer :integer]

               :mobiles ["#MOBILES" :mobile* "#0"]
               :mobile [:mobile-prefix :string :string :string :string :string  "|"]
               :mobile-prefix #{:v-prefix :#-prefix}
               :v-prefix (parsley/unspaced "V" :nzinteger :whitespace :nzinteger)
               :#-prefix #"#[1-9][0-9]*"
               
               :string [:string-body :string-terminator ]
               :string-body #"[^\s][^~]*"
               :string-terminator "~"
               :integer #"[0-9]+"
               :nzinteger #"[1-9][0-9]*"
               
               :whitespace #"[ \t\n]+")]
    (parse str)))

(defn parses-to?
  [tag]
  (fn [x]
    (let [y (parse tag x)]
      (and (= (:tag y)
              :net.cgrand.parsley/root)
           (= (count (:content y))
              1)
           (= (:tag ((:content y) 0))
              tag)))))
