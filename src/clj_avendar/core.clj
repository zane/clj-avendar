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

(defmacro ws->>
  "Expands into nested nxt forms that ignore interposed whitespace."
  ([m] m)
  ([m n] `(nxt ~m
               (nxt (many (whitespace))
                    ~n)))
  ([m n & ms] `(nxt ~m
                    (nxt (many (whitespace))
                         (ws->> ~n ~@ms)))))

(defmacro ws-let->>
  "Expands into nested bind forms"
  [[& bindings] & body]
  (let [[bind-form p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~bind-form] ~@body))
      `(bind ~p (fn [~bind-form] (nxt (many (whitespace))
                                     (ws-let->> ~(drop 2 bindings) ~@body)))))))

(defn ws-times
  "Consume exactly n number of p ignoring interposed whitespace."
  [n p]
  (let [p (if (= n 1)
            p
            (nxt (many (whitespace))
                 p))]
    (if (= n 0)
      (always [])
      (fn [state cok cerr eok eerr]
        (letfn [(pcok [item state]
                  (let [q (times (dec n) p)]
                    (letfn [(qcok [items state]
                              (cok (cons item items) state))]
                      (q state qcok cerr qcok eerr))))
                (peok [item state]
                  (eok (repeat n item) state))]
          (p state pcok cerr peok eerr))))))

(defn ws-many
  "Consume zero or more p with interposed whitespace."
  [p]
  (letfn [(many-err [_ _]
            (throw (RuntimeException. "Combinator '*' is applied to a parser that accepts an empty string")))
          (safe-p [state cok cerr eok eerr]
            (p state cok cerr many-err eerr))]
    (either
     (ws-let->> [x safe-p
                 xs (ws-many safe-p)]
       (always (cons x xs)))
     (always []))))

(defn ws-between
  "Parse p after parsing open and before parsing close, returning the
   value of p and discarding the values of open and close. Ignores
   interposed whitespace."
  [open close p]
  (ws-let->> [_ open
              x p
              _ close]
    (always x)))

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
            (ws-let->> [xs xs-parser
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
  (either (neg-integer)
          (pos-integer)))

(defparser nonzero-integer []
  (let->> [nonzero-digit (nonzero-digit)
           digits (many (digit))]
    (always (read-string (apply str (cons nonzero-digit digits))))))

(defparser word []
  (let->> [chars (many1 (choice (letter)
                                (char \_)))]
    (always (apply str chars))))

(defparser tilde-string []
  (let->> [chars (many (not-char \~))
           _ (char \~)]
    (always (apply str chars))))

(defrecord NumberRange
    [start
     end])

(defparser area-vnum-decl []
  (let->> [vals (ws->> (string "VNUMs") (ws-times 2 (integer)))]
    (always (apply ->NumberRange vals))))

(defrecord Weather
    [base-precip
     base-temp
     base-wind-mag
     base-wind-dir
     geography])

(defparser area-weather-decl []
  (let->> [vals (ws->> (string "Weather") (ws-times 5 (integer)))]
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
  (bind (ws-between
         (string "#AREADATA")
         (string "End")
         (ws-many (choice (named :name       (ws->> (string "Name")     (tilde-string)))
                          (named :builders   (ws->> (string "Builders") (tilde-string)))
                          (named :credits    (ws->> (string "Credits")  (tilde-string)))
                          (named :danger     (ws->> (string "Danger")   (integer)))
                          (named :security   (ws->> (string "Security") (integer)))
                          (named :info-flags (ws->> (string "Areainfo") (integer)))
                          (named :herbs      (ws->> (string "Herbs")    (integer)))

                          (named :weather (area-weather-decl))
                          (named :vnums (area-vnum-decl)))))
        #(always (map->Area (apply list-merge %)))))

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
  (choice (let->> [num (integer)]
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
  (ws-let->> [pierce (integer)
              slash  (integer)
              bash   (integer)
              exotic (integer)]
    (always (apply ->ArmorClass (map #(* 10 %)
                                     (list pierce
                                           slash
                                           bash
                                           exotic))))))

(defrecord MobProg
    [type
     args
     coms])

(defparser mobprog []
  (ws-let->> [_ (char \>)
           type (word)
           args (tilde-string)
           coms (tilde-string)]
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

(defparser position []
  (let->> [pos (choice (string "stand")
                       (string "sit")
                       (string "rest"))]
    (always (keyword pos))))

(defparser mobile []
  (bind (ordered (>> (char \V)
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
                        (ws-let->> [count (integer)]
                          (ws-times count (integer))))
                 (named :assist-vnums
                        (ws-let->> [count (integer)]
                          (ws-times count (integer))))
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
                 (named :mobprogs (maybe [] (ws-let->> [progs (ws-many (mobprog))
                                                        _ (char \|)]
                                              (always progs)))))
        #(always (map->Mobile (reduce merge %)))))

(defparser mobiles-block []
  (ws-between (string "#MOBILES")
              (string "#0")
              (ws-many (mobile))))

(defparser area []
  (ws-let->> [area (area-block)
              mobiles (mobiles-block)]
    (always {:area area
             :mobiles mobiles})))