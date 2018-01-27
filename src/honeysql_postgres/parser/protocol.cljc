(ns honeysql-postgres.parser.protocol
  (:require [clojure.string :as str]))

(def ^:privat no-match :honeysql-postgres.parser/no-match)

(defprotocol Parser
  "Protocol for a parser."
  (parse [_ tokens]
    "Parses the given tokens.  Returns either no-match or a tuple of matched
    value and remaining tokens.")
  (explain [_ tokens]
    "Explains why the parser could not parse the given tokens."))

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
  (explain [this tokens]
    (when (= no-match (parse this tokens))
      (let [reasons (reduce #(conj %1 (explain %2 tokens))
                            []
                            parsers)]
        (str "Alternates failed: "
             (str/join ", " reasons)))))
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
  (explain [_ _]
    nil)
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
  (explain [this tokens]
    (when (= no-match (parse this tokens))
      (loop [ts tokens
             ps parsers]
        (when (seq ps)
          (let [p (first ps)
                result (parse p ts)]
            (if (= no-match result)
              (let [context (loop [context []
                                   tokens tokens]
                              (if (= tokens ts)
                                context
                                (recur (conj context (pr-str (first tokens)))
                                       (next tokens))))
                    explanation (explain p ts)]
                (if (seq context)
                  (str "After parsing " (str/join " " context) ", " explanation)
                  explanation))
              (recur (second result) (next ps))))))))
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
  (explain [this [t :as tokens]]
    (cond
      (not= no-match (parse this tokens)) nil
      (seq tokens) (str (pr-str t) " did not match " str-rep)
      :otherwise (str "End of input when expecting " str-rep)))
  Object
  (toString [_]
    str-rep))


(def ε
  "Parser that matches the end of input."
  (reify
    Parser
    (parse [_ tokens]
      (if (seq tokens)
        no-match
        [nil nil]))
    (explain [_ [t :as tokens]]
      (when (seq tokens)
        (str "Expected end of input but found " (pr-str t))))
    Object
    (toString [_] "ε")))

(deftype Vector [parsers]
  Parser
  (parse [_ [t & more]]
    (if (vector? t)
      (let [result (parse (->Sequence (conj parsers ε)) t)]
        (if (not= no-match result)
          (let [[r] result]
            [(subvec r 0 (dec (count r))) more])
          no-match))
      no-match))
  (explain [this [t :as tokens]]
    (when (= no-match (parse this tokens))
      (cond
        (empty? tokens) (str "Expected a vector but got end of input")
        (not (vector? t)) (str "Expected a vector but got " (pr-str t))
        :otherwise (str "Contents of vector did not match: "
                        (explain (->Sequence (conj parsers ε)) t)))))
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
  (explain [_ tokens]
    (explain parser tokens))
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
  (explain [_ tokens]
    nil)
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
  (explain [this tokens]
    (when (= no-match (parse parser tokens))
      (explain parser tokens)))
  Object
  (toString [_]
    (str parser " +")))

#?(:clj
   (defmacro pred*
     "Creates a parser that will match the given predicate.  Takes a text
     representation as optional arguments."
     ([p]
      `(pred* ~p ~(pr-str p)))
     ([p txt]
      `(->Predicate ~p ~txt))))
