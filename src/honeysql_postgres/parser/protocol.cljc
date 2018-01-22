(ns honeysql-postgres.parser.protocol
  (:require [clojure.string :as str]))

(def ^:privat no-match :honeysql-postgres.parser/no-match)

(defprotocol Parser
  "Protocol for a parser."
  (parse [_ state tokens]
    "Parses the given tokens with the given state.  Returns either no-match
    or a tuple of matched value, new state, and remaining tokens."))

(deftype Alt [parsers]
  Parser
  (parse [_ state tokens]
    (reduce (fn [_ p]
              (let [result (parse p state tokens)]
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
  (parse [_ state tokens]
    (let [result (parse parser state tokens)]
      (if (not= no-match result)
        result
        [no-value state tokens])))
  Object
  (toString [_]
    (str parser " ?")))

(deftype Sequence [parsers]
  Parser
  (parse [_ state tokens]
    (reduce (fn [[r s t] p]
              (let [result (parse p s t)]
                (if (= no-match result)
                  (reduced no-match)
                  (assoc result 0 (conj r (first result))))))
            [[] state tokens]
            parsers))
  Object
  (toString [_]
    (str/join " " (map str parsers))))

(deftype Predicate [pred str-rep]
  Parser
  (parse [_ state [t & more :as tokens]]
    (cond
      (empty? tokens) no-match
      (pred t) [t state more]
      :otherwise no-match))
  Object
  (toString [_]
    str-rep))

(deftype Vector [parsers]
  Parser
  (parse [_ state [t & more]]
    (if (vector? t)
      (let [result (parse (->Sequence parsers) state t)]
        (if (not= no-match result)
          (let [[_ _ extra-tokens] result]
            (if (empty? extra-tokens)
              (assoc result 2 more)
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
  (parse [_ state tokens]
    (let [result (parse parser state tokens)]
      (if (not= result no-match)
        (let [[inner-result inner-state tokens] result
              new-result (f inner-result inner-state)]
          (conj new-result tokens))
        no-match)))
  Object
  (toString [_]
    (str parser)))

(deftype KleeneStar [parser]
  Parser
  (parse [_ state tokens]
    (loop [[acc state tokens :as last] [[] state tokens]]
      (if (seq tokens)
        (let [result (parse parser state tokens)]
          (if (not= no-match result)
            (recur (assoc result 0 (conj acc (first result))))
            last))
        last)))
  Object
  (toString [_]
    (str parser " *")))

(deftype KleenePlus [parser]
  Parser
  (parse [_ state tokens]
    (let [star-result (parse (->KleeneStar parser) state tokens)]
      (cond
        (= no-match star-result) no-match
        (zero? (count (first star-result))) no-match
        :ok star-result)))
  Object
  (toString [_]
    (str parser " +")))

(deftype Epsilon []
  Parser
  (parse [_ state tokens]
    (if (seq tokens)
      no-match
      [nil state nil]))
  Object
  (toString [_] "ε"))

#?(:clj
   (defmacro pred*
     "Creates a parser that will match the given predicate.  Takes a state
     transformation function and text representation as optional arguments."
     ([p]
      `(pred* ~p ~(pr-str p)))
     ([p txt]
      `(->Predicate ~p ~txt))))
