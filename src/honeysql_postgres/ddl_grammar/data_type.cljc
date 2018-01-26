(ns honeysql-postgres.ddl-grammar.data-type
  (:refer-clojure :exclude [type])
  (:require [honeysql-postgres.ddl-grammar.common :refer [to-sql]]
            [honeysql-postgres.parser :as p]
            [honeysql-postgres.types :as pg-sqlt]))


(def ^:private simple-type?
  "Set of known types that take no arguments"
  #{;; integral types
    :integer :int :int4
    :smallint :int2
    :bigint :int8
    ;; floating point
    :double-precision :float8
    :real :float4
    ;; serial types
    :serial :serial4
    :smallserial :serial2
    :bigserial :serial8
    ;; text type
    :text
    ;; temporal types
    :timestamptz :timestamp-with-time-zone
    :timestamp-without-time-zone
    :date
    :timetz :time-with-time-zone
    :time-without-time-zone
    ;; money type
    :money
    ;; boolean types
    :boolean :bool
    ;; binary types
    :bytea :blob :binary-large-object
    ;; geometric types
    :point :line :lseg :box :path :polygon :circle
    ;; netwokrk types
    :cidr :inet :macaddr :macaddr8
    ;; text search types
    :tsvector :tsquery
    ;; uuid type
    :uuid
    ;; xml type
    :xml
    ;; json types
    :json :jsonb})

(def ^:private positive-integer?
  (p/pred (every-pred integer? pos?) "pos-int?"))

(def ^:private boxed-positive-integer?
  "Parser that gets a single positive integer from a vector."
  (p/map-result first
                (p/vector positive-integer?)))

(def ^:private natural-integer?
  (p/pred (every-pred integer? (complement neg?)) "nat-int?"))

(def ^:private simple-type
  "Parses a known simple type."
  (p/map-result #(pg-sqlt/->PgSqlSimpleType %)
                (apply p/one-of simple-type?)))

(def ^:private arbitrary-precision-type
  (p/map-result (fn [[t [p s]]] (pg-sqlt/->PgSqlArbitraryPrecisionType t p s))
                (p/seq (p/one-of :numeric :decimal)
                       (p/? (p/vector positive-integer?
                                      (p/? natural-integer?))))))

(def ^:private float-type
  "Parser for FLOAT types which may include a precision argument."
  (p/map-result #(pg-sqlt/->PgSqlFloatType (second %))
                (p/seq (p/lit :float)
                       (p/? boxed-positive-integer?))))

(def ^:private string-type
  "Parser for string types which may include a length argument."
  (p/map-result (fn [[t [l]]] (pg-sqlt/->PgSqlStringType t l))
                (p/seq (p/one-of :bit
                                 :bit-varying :varbit
                                 :character :char
                                 :character-varying :varchar)
                       (p/? (p/vector positive-integer?)))))

(def ^:private time-precision
  "Parser for time precision, i.e. an integer between 0 and 6 inlcusive."
  (p/map-result first
                (p/vector (p/pred (every-pred integer? #(<= 0 % 6)) "int-between-0-and-6?"))))

(def ^:private time-type
  "PArser for TIME and TIMESTAMP types"
  (p/map-result  #(apply pg-sqlt/->PgSqlTimeType %)
                (p/seq (p/one-of :time :timestamp)
                       (p/? time-precision)
                       (p/? (p/map-result #(= :with-time-zone %)
                                          (p/one-of :with-time-zone :without-time-zone))))))

(def ^:private interval-type
  "Parser for INTERVAL types"
  (p/map-result #(apply pg-sqlt/->PgSqlIntervalType (next %))
                (p/seq (p/lit :interval)
                       (p/? (p/one-of :year :month :day :hour :minute :second
                                      :year-to-month :day-to-hour :day-to-minute
                                      :day-to-second :hour-to-minute :hour-to-second
                                      :minute-to-second))
                       (p/? time-precision))))

(def ^:private base-type
  "Parser for non-array types"
  (p/| simple-type
       arbitrary-precision-type
       float-type
       string-type
       time-type
       interval-type
       to-sql))

(def ^:private sql-array
  "Parser for an array suffix using the standard SQL syntax."
  (p/map-result (fn [[_ size?]]
                  (pg-sqlt/->PgSqlArrayType nil
                                            (if (some? size?) [size?] [])
                                            true))
                (p/seq (p/lit :array)
                       (p/? (p/vector positive-integer?)))))

(def ^:private pg-sql-array
  "Parser for an array suffix using the PostgreSQL syntax."
  (p/map-result #(pg-sqlt/->PgSqlArrayType nil % false)
                (p/+ (p/map-result #(if (first %) % [])
                                   (p/vector (p/? positive-integer?))))))

(def type
  "Parser for a SQL type including arrays."
  (p/map-result (fn [[base array]]
                  (if array
                    (assoc array :base-type base)
                    base))
                (p/seq base-type
                       (p/? (p/| sql-array pg-sql-array)))))

(defn ^:private parse-pg-sql-type*
  "Reader function for pg-sql/type tagged literals."
  [args]
  (let [result (p/parse (p/map-result first
                                      (p/seq type p/ε))
                        args)]
    (if (not= ::p/no-match result)
      result)))

(defn parse-pg-sql-type
  "Reader function for pg-sql/type tagged literals."
  [args]
  (#?(:clj (resolve `parse-pg-sql-type*) :cljs parse-pg-sql-type*) args))
