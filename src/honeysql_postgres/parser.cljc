(ns honeysql-postgres.parser
  (:refer-clojure :exclude [seq * +] :rename {map -map vector -vector})
  (:require [clojure.string :as str]
            #?(:clj [honeysql.util :refer [defalias]])
            [honeysql-postgres.parser.protocol :as proto]))

(defn |
  "Matches the first matching parser in the list of parsers."
  [& parsers]
  {:pre [(pos? (count parsers))]}
  (proto/->Alt parsers))

(defn ?
  "A parser that optionally matches the given parser."
  ([parser]
   (? parser nil))
  ([parser no-value]
   (proto/->Maybe parser no-value)))

(defn seq
  "A parser that matches a sequence of parsers."
  [& parsers]
  (proto/->Sequence parsers))

(#?(:clj defalias :cljs def) pred proto/pred*)

(defn one-of
  "Creates a parser that will match one of the given tokens, which must be
  defined in a set."
  [& ts]
  (proto/pred* (set ts)
               (str "( "
                    (str/join " | " (-map pr-str ts))
                    " )")))

(defn lit
  "Creates a parser that will match the given literal."
  [t]
  (proto/pred* #(= t %) (pr-str t)))

(defn vector
  "Matches a single vector with contents matching the sequence of parsers."
  [& parsers]
  (proto/->Vector parsers))

(defn map
  "Creates a parser that transforms the result and state returned from the
  given parser using a function.  f takes two arguments: the result and the
  state, and returns a tuple of the new result and new state."
  [f parser]
  (proto/->Transform parser f))

(defn *
  "Creates a parser that matches zero or more rules matching the given"
  [parser]
  (proto/->KleeneStar parser))

(defn +
  "Creates a parser that one zero or more rules matching the given"
  [parser]
  (proto/->KleenePlus parser))

(def ε
  "A parser that matches the end of input."
  (proto/->Epsilon))

(def eps
  "A parser that matches the end of input."
  ε)

(defn parse
  "Invokes the parser with the given tokens."
  [parser tokens]
  (let [result (proto/parse parser tokens)]
    (if (not= ::no-match result)
      (first result)
      result)))
