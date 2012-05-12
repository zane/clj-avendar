(ns clj-avendar.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defrecord Area [name builders credits vnums danger])
(defrecord NumberRange [start end])

(defn string
  [string]
  (reduce nxt
          (map char (seq string))))

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

(defparser area-name-decl []
  (let->> [name (>> (string "Name")
                    (tilde-string))]
    (always {:names [name]})))

(defparser area-builders-decl []
  (let->> [builders (>> (string "Builders")
                        (tilde-string))]
    (always {:builders [builders]})))

(defparser area-credits-decl []
  (let->> [credits (>> (string "Credits")
                       (tilde-string))]
    (always {:credits [credits]})))

(defparser area-vnum-decl []
  (let->> [_ (many (whitespace))
           _ (string "VNUMs")
           start (integer)
           end (integer)]
    (always {:vnums [(NumberRange. start end)]})))

(defparser area []
  (let->> [opts (between (string "#AREADATA")           
                         (string "End")
                         (many (choice (let->> [_ (many1 (whitespace))]
                                         (always {})) 
                                       (area-name-decl)
                                       (area-builders-decl)
                                       (area-credits-decl)
                                       (area-vnum-decl)
                                       (let->> [_ (string "Danger")
                                                danger (integer)]
                                         (always {:danger [danger]})))))]
    (always (let [merged (apply (partial merge-with into) opts)]
              (->Area (first (:names merged))
                      (first (:builders merged))
                      (first (:credits merged))
                      (:vnums merged)
                      (first (:danger merged)))))))

              
