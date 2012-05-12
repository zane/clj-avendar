(ns clj-avendar.core
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core])
  (:use [the.parsatron]))

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

(defrecord Mobile
    [version
     vnum
     player-name
     short-desc
     long-desc
     description
     race])

(defparser mobile []
  (let->> [opts
           (ordered (>> (char \V) (integer))
                    (integer)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string)
                    (tilde-string))]
    (always (apply ->Mobile opts))))

(defn flag-convert
  [chr]
  (if (and (<= (int \A) (int chr))
           (<= (int chr) (int \Z)))
    (loop [chr chr
           acc 1]
      (if (<= (int chr) (int \A))
        acc
        (recur (core/char (dec (int chr)))
               (* acc 2))))
    (loop [chr chr
           acc 67108864]
      (if (<= (int chr) (int \a))
        acc
        (recur (core/char (dec (int chr)))
               (* acc 2))))))