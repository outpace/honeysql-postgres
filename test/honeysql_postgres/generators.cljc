(ns honeysql-postgres.generators
  (:require [clojure.test.check.generators :as gen]
            [honeysql.core :as sql]
            [honeysql-postgres.ddl-grammar.data-type :as dt])
  )

(def ^:private pos-int
  "Generates a positive integer."
  (gen/large-integer* {:min 1}))

(def simple-type-args
  "Generates arguments for a simple type, e.g. [:int]."
  (gen/tuple (gen/elements @#'dt/simple-type?)))

(def float-type-args
  "Generates arguments for a float type, e.g. [:float [4]]"
  (gen/one-of [(gen/return [:float])
               (gen/fmap #(vector :float [%])
                         (gen/large-integer* {:min 1 :max 54}))]))

(def arbitrary-precision-type-args
  "Generates arguments for an arbitrary precision type, e.g. [:numeric [4 0]]"
  (let [type (gen/elements [:numeric :decimal])
        precision pos-int
        scale (gen/large-integer* {:min 0})]
    (gen/one-of [(gen/tuple type)
                 (gen/tuple type (gen/tuple precision))
                 (gen/tuple type (gen/tuple precision scale))])))

(def string-type-args
  "Generates arguments for an string type, e.g. [:varchar [40]]"
  (let [type (gen/elements [:bit :bit-varying :varbit
                            :char :character :character-varying :varchar])
        length pos-int]
    (gen/one-of [(gen/tuple type)
                 (gen/tuple type (gen/tuple length))])))

(def ^:private time-precision
  "Generates a time precision, e.g. [[4]]."
  (gen/tuple (gen/large-integer* {:min 0 :max 6})))

(def time-type-args
  "Generates arguments for a time type, e.g. [:time :without-time-zone]"
  (let [type (gen/elements [:time :timestamp])
        time-zone (gen/elements [:with-time-zone :without-time-zone])]
    (gen/one-of [(gen/tuple type)
                 (gen/tuple type time-zone)
                 (gen/tuple type time-precision)
                 (gen/tuple type time-precision time-zone)])))

(def interval-type-args
  "Generates arguments for an interval type, e.g. [:interval :minute-to-second]"
  (let [type (gen/return :interval)
        field (gen/elements [:year :month :day :hour :minute :second
                             :year-to-month :day-to-hour :day-to-minute
                             :day-to-second :hour-to-minute :hour-to-second
                             :minute-to-second])]
    (gen/one-of [(gen/tuple type)
                 (gen/tuple type field)
                 (gen/tuple type time-precision)
                 (gen/tuple type field time-precision)])))

(def to-sql-type-args
  "Generates arguments for a type that satisfies the ToSql protocol."
  (gen/one-of [(gen/tuple gen/keyword)
               (gen/tuple (gen/fmap sql/raw
                                    gen/string))]))

(def base-type-args
  "Generates arguments for a non-array type."
  (gen/one-of [simple-type-args
               float-type-args
               arbitrary-precision-type-args
               string-type-args
               time-type-args
               interval-type-args
               to-sql-type-args]))

(def standard-sql-array-type-args
  "Generates arguments for a standard SQL array of an arbitrary type, e.g. [:integer :array [4]]."
  (gen/one-of [(gen/fmap #(conj % :array)
                         base-type-args)
               (gen/fmap (fn [[bt size]]
                             (conj bt :array [size]))
                         (gen/tuple base-type-args pos-int))]))

(def pg-sql-array-type-args
  "Generates arguments for a PostgreSQL array of an arbitrary type, e.g. [:integer [] [4]]."
  (gen/fmap (fn [[bt sizes]] (into bt sizes))
            (gen/tuple base-type-args
                       (gen/vector (gen/one-of [(gen/return [])
                                                (gen/tuple pos-int)])))))

(def type-args
  "Generates arguments for an arbitrary type, including arrays."
  (gen/one-of [base-type-args
               standard-sql-array-type-args
               pg-sql-array-type-args]))
