(ns clj-avendar.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defrecord Area [name])

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
  (let->> [digits (many1 (digit))]
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
  (let->> [name (>> (string "Name") (tilde-string))]
    (always {:name name})))

(defparser area []
  (let->> [opts (between (string "#AREADATA")           
                         (string "End")
                         (many (choice (area-name-decl))))]
    (always (let [merged-opts (apply merge opts)]
              (Area. (:name merged-opts))))))