(ns clj-avendar.core
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core])
  (:use [the.parsatron])
  (:use [clojure.math.numeric-tower :only (expt)]))

(defn list-merge
  [& lst]
  (apply (partial merge-with
                  (fn [x y]
                    (if (vector? x)
                      (conj x y)
                      [x y])))
         lst))

(defparser named
    [tag parser]
  (let->> [x parser]
    (always {tag x})))

(defn string
  [string]
  (reduce nxt (map char (seq string))))

(defn ordered
  [& parsers]
  (reduce (fn [xs-parser x-parser]
            (let->> [xs xs-parser
                     x x-parser]
              (always (conj xs x))))
          (always [])
          parsers))

(defn whitespace
  "Consume a whitespace character"
  []
  (token #(Character/isWhitespace %)))

(defparser nonzero-digit []
  (token #(re-matches #"[1-9]" (str %))))

(defparser not-char [chr]
  (token #(not (= chr %))))

(defn not-chars [& chrs]
  (token #(nil? ((apply hash-set chrs) %))))

(defparser pos-integer []
  (let->> [digits (many1 (digit))]
    (always (read-string (apply str digits)))))

(defparser neg-integer []
  (let->> [_ (char \-)
           int (pos-integer)]
    (always (- int))))

(defparser integer []
  (>> (many (whitespace))
      (either (neg-integer)
              (pos-integer))))

(defparser number []
  (choice (integer)
          (let->> [negint (>> (char \-) (integer))]
            (always (- negint)))))

(defparser nonzero-integer []
  (let->> [nonzero-digit (nonzero-digit)
           digits (many (digit))]
    (always (read-string (apply str (cons nonzero-digit digits))))))

(defparser word []
  (let->> [chars (>> (many (whitespace))
                     (many1 (choice (letter)
                                    (char \_))))]
    (always (apply str chars))))

(defparser tilde-string []
  (let->> [chars (>> (many (whitespace))
                     (many (not-char \~)))
           _ (char \~)]
    (always (apply str chars))))

(defrecord NumberRange
    [start
     end])

(defparser area-vnum-decl []
  (let->> [vals (>> (string "VNUMs") (times 2 (integer)))]
    (always (apply ->NumberRange vals))))

(defrecord Weather
    [base-precip
     base-temp
     base-wind-mag
     base-wind-dir
     geography])

(defparser area-weather-decl []
  (let->> [vals (>> (string "Weather") (times 5 (integer)))]
    (always (apply ->Weather vals))))

(defrecord Area
    [name
     builders
     credits
     vnums
     danger
     security
     info-flags
     herbs
     weather])

(defparser area-block []
  (let->> [opts
           (between
            (string "#AREADATA")
            (string "End")
            (many (choice (>> (many1 (whitespace)) (always {}))

                          (named :name       (>> (string "Name")     (tilde-string)))
                          (named :builders   (>> (string "Builders") (tilde-string)))
                          (named :credits    (>> (string "Credits")  (tilde-string)))
                          (named :danger     (>> (string "Danger")   (integer)))
                          (named :security   (>> (string "Security") (integer)))
                          (named :info-flags (>> (string "Areainfo") (integer)))
                          (named :herbs      (>> (string "Herbs")    (integer)))

                          (named :weather (area-weather-decl))
                          (named :vnums (area-vnum-decl)))))]
    (always (map->Area (apply list-merge opts)))))

(defparser maybe [alternative parser]
  (choice parser
          (always alternative)))

(defn flag-convert
  [chr]
  (if (and (<= (int \A) (int chr))
           (<= (int chr) (int \Z)))
    (expt 2 (- (int chr) (int \A)))
    (expt 2 (+ 26 (- (int chr) (int \a))))))

(defparser flag []
  (>> (many (whitespace))
      (let->> [letters (many (letter))
               digits  (many (digit))
               rest    (maybe 0 (>> (char \|)
                                    (flag)))]
        (always (+ (reduce (fn [acc digit]
                             (+ (* 10 acc)
                                digit))
                           (reduce + (map flag-convert letters))
                           (map (fn [digit]
                                  (Integer/parseInt (str digit)))
                                digits))
                   rest)))))

(defparser alignment []
  (>> (many (whitespace))
      (choice (let->> [num (number)]
                (cond (> num 0) (always :good)
                      (< num 0) (always :evil)
                      :else     (always :neutral)))
              (>> (char \G) (always :good))
              (>> (char \N) (always :neutral))
              (>> (char \E) (always :evil))
              (>> (char \R) (always :random)))))

(defrecord Dice
    [number
     type
     bonus])

(defparser dice []
  (let->> [number (integer)
           _      (char \d)
           type   (integer)
           _      (char \+)
           bonus  (integer)]
    (always (->Dice number type bonus))))

(defrecord ArmorClass
    [pierce
     slash
     bash
     exotic])

(defparser armor-class []
  (let->> [vals (times 4 (integer))]
    (always (apply ->ArmorClass (map #(* 10 %)
                                     vals)))))

(defrecord MobProg
    [type
     args
     coms])

(defparser mobprog []
  (let->> [_ (many (whitespace))
           _ (char \>)
           type (word)
           args (tilde-string)
           coms (tilde-string)
           _ (many (whitespace))]
    (always (->MobProg type args coms))))

(defrecord Mobile
    [version
     vnum
     player-name
     short-desc
     long-desc
     description
     race
     class
     act
     nact
     affected-by
     alignment
     group
     level
     hitroll
     hit
     mana
     damage
     dam-type
     dam-verb
     armor-class
     off-flags
     imm-flags
     resists
     assist-vnums
     start-pos
     default-pos
     sex
     wealth
     faction
     form
     parts
     languages
     size
     material])

(defparser mobile []
  (let->> [opts (ordered (>> (many (whitespace))
                             (char \V)
                             (named :version (integer)))
                         (named :vnum (integer))
                         (named :player-name (tilde-string))
                         (named :short-desc (tilde-string))
                         (named :long-desc (tilde-string))
                         (named :description (tilde-string))
                         (named :race (tilde-string))
                         (named :class (tilde-string))
                         (named :act (flag))
                         (named :nact (flag))
                         (named :affected-by (flag))
                         (named :alignment (alignment))
                         (named :group (integer))
                         (named :level (integer))
                         (named :hitroll (integer))
                         (named :hit (dice))
                         (named :mana (dice))
                         (named :damage (dice))
                         (named :dam-type (tilde-string))
                         (named :dam-verb (tilde-string))
                         (named :armor-class (armor-class))
                         (named :off-flags (flag))
                         (named :imm-flags (flag))
                         (named :resists
                                (let->> [count (integer)]
                                  (times count (integer))))
                         (named :assist-vnums
                                (let->> [count (integer)]
                                  (times count (integer))))
                         (named :start-pos (word))
                         (named :default-pos (word))
                         (named :sex (word))
                         (named :wealth (integer))
                         (named :faction (integer))
                         (named :form (flag))
                         (named :parts (flag))
                         (named :languages (flag))
                         (named :size (word))
                         (named :material (word))
                         (maybe {} (>> (many (whitespace)) (always {})))
                         (named :mobprogs (maybe [] (let->> [progs (many (mobprog))
                                                             _ (>> (many (whitespace))
                                                                   (char \|))]
                                                      (always progs))))
                         (maybe {} (>> (many (whitespace)) (always {}))))]
    (always (map->Mobile (reduce merge opts)))))

(defparser mobiles-block []
  (let->> [_ (>> (string "#MOBILES")
                 (many (whitespace)))
           mobiles (many (mobile))
           _ (>> (many (whitespace))
                 (string "#0"))]
    (always mobiles)))

(defparser area []
  (let->> [area (>> (many (whitespace))
                    (area-block))
           mobiles (>> (many (whitespace))
                       (mobiles-block))]
    (always {:area area
             :mobiles mobiles})))