(ns honeysql-postgres.parser.protocol
  (:require [clojure.string :as str]))

(def ^:privat no-match :honeysql-postgres.parser/no-match)

(defprotocol Parser
  "Protocol for a parser."
  (parse [_ tokens]
    "Parses the given tokens.  Returns either no-match or a tuple of matched
    value and remaining tokens."))

(deftype Alt [parsers]
  Parser
  (parse [_ tokens]
    (reduce (fn [_ p]
              (let [result (parse p tokens)]
                (if (not= no-match result)
                  (reduced result)
                  no-match)))
            no-match
            parsers))
  Object
  (toString [_]
   (str "( "
        (str/join " | " (map str parsers))
        " )")))

(deftype Maybe [parser no-value]
  Parser
  (parse [_ tokens]
    (let [result (parse parser tokens)]
      (if (not= no-match result)
        result
        [no-value tokens])))
  Object
  (toString [_]
    (str parser " ?")))

(deftype Sequence [parsers]
  Parser
  (parse [_ tokens]
    (reduce (fn [[r t] p]
              (let [result (parse p t)]
                (if (= no-match result)
                  (reduced no-match)
                  (assoc result 0 (conj r (first result))))))
            [[] tokens]
            parsers))
  Object
  (toString [_]
    (str/join " " (map str parsers))))

(deftype Predicate [pred str-rep]
  Parser
  (parse [_ [t & more :as tokens]]
    (cond
      (empty? tokens) no-match
      (pred t) [t more]
      :otherwise no-match))
  Object
  (toString [_]
    str-rep))

(deftype Vector [parsers]
  Parser
  (parse [_ [t & more]]
    (if (vector? t)
      (let [result (parse (->Sequence parsers) t)]
        (if (not= no-match result)
          (let [[_ extra-tokens] result]
            (if (empty? extra-tokens)
              (assoc result 1 more)
              no-match))
          no-match))
      no-match))
  Object
  (toString [_]
    (if (empty? parsers)
      "[]"
      (str "[ "
           (str/join " " (map str parsers))
           " ]"))))

(deftype Transform [parser f]
  Parser
  (parse [_ tokens]
    (let [result (parse parser tokens)]
      (if (not= result no-match)
        (let [[inner-result tokens] result
              new-result (f inner-result)]
          [new-result tokens])
        no-match)))
  Object
  (toString [_]
    (str parser)))

(deftype KleeneStar [parser]
  Parser
  (parse [_ tokens]
    (loop [[acc tokens :as last] [[] tokens]]
      (if (seq tokens)
        (let [result (parse parser tokens)]
          (if (not= no-match result)
            (recur (assoc result 0 (conj acc (first result))))
            last))
        last)))
  Object
  (toString [_]
    (str parser " *")))

(deftype KleenePlus [parser]
  Parser
  (parse [_ tokens]
    (let [star-result (parse (->KleeneStar parser) tokens)]
      (cond
        (= no-match star-result) no-match
        (zero? (count (first star-result))) no-match
        :ok star-result)))
  Object
  (toString [_]
    (str parser " +")))

(deftype Epsilon []
  Parser
  (parse [_ tokens]
    (if (seq tokens)
      no-match
      [nil nil]))
  Object
  (toString [_] "ε"))

#?(:clj
   (defmacro pred*
     "Creates a parser that will match the given predicate.  Takes a text
     representation as optional arguments."
     ([p]
      `(pred* ~p ~(pr-str p)))
     ([p txt]
      `(->Predicate ~p ~txt))))
