(ns honeysql-postgres.table-columns.data-types
  (:require [clojure.string :as str]))

(def ^:private compound-tokens
  "Mapping of simple tokens to their expanded form."
  {:time-with-time-zone [:time :with-time-zone]
   :time-without-time-zone [:time :without-time-zone]
   :timestamp-with-time-zone [:timestamp :with-time-zone]
   :timestamp-without-time-zone [:timestamp :without-time-zone]})

(defn ^:private split-compound-tokens
  "Replaces compound tokens with their expanded form."
  [[t & more :as tokens]]
  (if-let [expanded (get compound-tokens t)]
    (into expanded more)
    tokens))

(defn ^:private parse-simple-type
  "Parses a type that just uses the first token."
  [[t & more]]
  [{:type t} more])

(def ^:private takes-length-argument?
  "Data types which may take a length argument."
  #{:bit
    :bit-varying
    :char
    :character
    :character-varying
    :varbit
    :varchar})

(def ^:private takes-precision-argument?
  "Data types which may have an optional argument vector."
  #{:float})

(defn ^:private parse-type-with-optional-argument
  "Parses a token that optionally takes an argument vector."
  [arg-key [type arg & rest-tokens :as tokens]]
  (if (and (vector? arg) (= 1 (count arg)))
    [{:type type arg-key (first arg)} rest-tokens]
    (parse-simple-type tokens)))

(def ^:private arbitrary-precision-type?
  "Data types which may have precision and scale arguments."
  #{:decimal :numeric})

(defn ^:private parse-arbitrary-precision-type?
  "Parses a token that optionally takes an argument vector."
  [[type arg & rest-tokens :as tokens]]
  (if (vector? arg)
    (let [parsed (case (count arg)
                   1 {:type type :precision (first arg)}
                   2 {:type type :precision (first arg) :scale (second arg)})]
      [parsed rest-tokens])
    (parse-simple-type tokens)))

(def ^:private temporal-type?
  "Predicate for whether or not a token is a temporal type."
  #{:date :interval :time :timetz :timestamp :timestamptz})

(def ^:private interval-field?
  "Predicate for whether a token is an interval field."
  #{:year
    :month
    :day
    :hour
    :minute
    :second
    :year-to-month
    :day-to-hour
    :day-to-minute
    :day-to-second
    :hour-to-minute
    :hour-to-second
    :minute-to-second})

(defn ^:private parse-temporal-type
  "Parses a temporal data type."
  [tokens]
  (letfn [(parse-time* [tokens]
            (let [;; parse type
                  [type & [next-token & rest-tokens :as tokens]] tokens
                  ;; parse precision
                  [precision [next-token & rest-tokens :as tokens]]
                  (if (and (vector? next-token) (= 1 (count next-token)))
                    [(first next-token) rest-tokens]
                    [nil tokens])
                  ;; get time zone flag
                  [time-zone? tokens]
                  (case next-token
                    :with-time-zone [true rest-tokens]
                    :without-time-zone [false rest-tokens]
                    [nil tokens])]
              [(cond-> {:type type}
                 (some? precision) (assoc :precision precision)
                 (some? time-zone?) (assoc :time-zone? time-zone?))
               tokens]))
          (parse-interval [tokens]
            (let [;; drop :interval
                  [next-token & more-tokens :as tokens] (next tokens)
                  ;; parse field
                  [field [next-token & more-tokens :as tokens]]
                  (if (interval-field? next-token)
                    [next-token more-tokens]
                    [nil tokens])
                  ;; parse precision
                  [[precision] tokens] (if (vector? next-token)
                                       [next-token more-tokens]
                                       [nil tokens])
                  ;; build type args
                  type-map (cond-> {:type :interval}
                             (some? field) (assoc :field field)
                             (some? precision) (assoc :precision precision))]
              [type-map tokens]))]
    (let [t (first tokens)]
      (cond
        (= :timestamp t) (parse-time* tokens)
        (= :time t) (parse-time* tokens)
        (= :timestamptz t) (parse-simple-type tokens)
        (= :timetz t) (parse-simple-type tokens)
        (= :date t) (parse-simple-type tokens)
        (= :interval) (parse-interval tokens)))))

(defn parse-data-type
  "Parses a token into a data type."
  [tokens]
  (let [tokens (-> tokens split-compound-tokens)
        t (first tokens)
        [type tokens] (cond
                        (takes-length-argument? t) (parse-type-with-optional-argument :length tokens)
                        (takes-precision-argument? t) (parse-type-with-optional-argument :precision tokens)
                        (arbitrary-precision-type? t) (parse-arbitrary-precision-type? tokens)
                        (temporal-type? t) (parse-temporal-type tokens)
                        :default (parse-simple-type tokens))]
    (loop [type type
           [next-token & rest-tokens :as tokens] tokens]
      (if (vector? next-token)
        (case (count next-token)
          0 (recur {:type :array :of type} rest-tokens)
          1 (recur {:type :array :of type :length (first next-token)} rest-tokens))
        [type tokens]))))
