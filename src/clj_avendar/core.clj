(ns clj-avendar.core
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core])
  (:use [the.parsatron])
  (:use [clojure.math.numeric-tower :only (expt)]))

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

(defrecord NumberRange
    [start
     end])

(defrecord Weather
    [base-precip
     base-temp
     base-wind-mag
     base-wind-dir
     geography])

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

(defparser not-tilde []
  (token #(not (= \~ %))))

(defparser integer []
  (let->> [_ (many (whitespace))
           digits (many1 (digit))]
    (always (read-string (apply str digits)))))

(defparser number []
  (choice (integer)
          (let->> [negint (>> (char \-) (integer))]
            (always (- negint)))))

(defparser nonzero-integer []
  (let->> [nonzero-digit (nonzero-digit)
           digits (many (digit))]
    (always (read-string (apply str (cons nonzero-digit digits))))))

(defparser tilde-string []
  (let->> [chars (>> (many (whitespace))
                     (many1 (not-tilde)))
           _ (char \~)]
    (always (apply str chars))))

(defparser area-vnum-decl []
  (let->> [vals (>> (string "VNUMs") (times 2 (integer)))]
    (always (apply ->NumberRange vals))))

(defparser area-weather-decl []
  (let->> [vals (>> (string "Weather") (times 5 (integer)))]
    (always (apply ->Weather vals))))

(defparser area []
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
               rest))))

(defparser alignment []
  (choice (let->> [num (number)]
            (cond (> num 0) (always :good)
                  (< num 0) (always :evil)
                  :else     (always :neutral)))
          (>> (char \G) (always :good))
          (>> (char \N) (always :neutral))
          (>> (char \E) (always :evil))
          (>> (char \R) (always :random))))

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

(defrecord Mobile
    [version
     vnum
     player-name
     short-desc
     long-desc
     description
     race
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
     armor-class])

(defparser mobile []
  (let->> [opts
           (ordered (>> (char \V) (integer))
                    (integer)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (flag)
                    (flag)
                    (flag)
                    (alignment)
                    (integer)
                    (integer)
                    (integer)
                    (dice)
                    (dice)
                    (dice)
                    (tilde-string)
                    (tilde-string)
                    (armor-class))]
    (always (apply ->Mobile opts))))