(ns honeysql-postgres.types-test
  (:require [clojure.test :refer [deftest is testing]]
            [#?(:clj clojure.edn :cljs cljs.reader) :as edn]
            [honeysql.core :as sql]
            [honeysql.types]
            [honeysql-postgres.ddl-grammar.data-type]
            [honeysql-postgres.types :as pg-sqlt]))

;; NOTE:
;;   Having tagged literals within the source code itself works with `lein
;;   test` and `lein doo`, but seems to break with `lein test-refresh`

(deftest test-data-types
  (testing "printing"
    (is (= "#pg-sql/type [:integer]"
           (pr-str (pg-sqlt/->PgSqlSimpleType :integer)))
        "simple type")
    (testing "arbitrary precision"
      (is (= "#pg-sql/type [:numeric]"
             (pr-str (pg-sqlt/->PgSqlArbitraryPrecisionType :numeric nil nil)))
          "without precision or scale")
      (is (= "#pg-sql/type [:decimal [20]]"
             (pr-str (pg-sqlt/->PgSqlArbitraryPrecisionType :decimal 20 nil)))
          "with precision")
      (is (= "#pg-sql/type [:numeric [20 5]]"
             (pr-str (pg-sqlt/->PgSqlArbitraryPrecisionType :numeric 20 5)))
          "with precision and precision"))
    (testing "floats"
      (is (= "#pg-sql/type [:float]"
             (pr-str (pg-sqlt/->PgSqlFloatType nil)))
          "without precision")
      (is (= "#pg-sql/type [:float [2]]"
             (pr-str (pg-sqlt/->PgSqlFloatType 2)))
          "with precision"))
    (testing "strings"
      (is (= "#pg-sql/type [:char]"
             (pr-str (pg-sqlt/->PgSqlStringType :char nil)))
          "without length")
      (is (= "#pg-sql/type [:bit [2]]"
             (pr-str (pg-sqlt/->PgSqlStringType :bit 2)))
          "with length"))
    (testing "times"
      (is (= "#pg-sql/type [:time]"
             (pr-str (pg-sqlt/->PgSqlTimeType :time nil nil)))
          "without precision or time zone")
      (is (= "#pg-sql/type [:timestamp [2]]"
             (pr-str (pg-sqlt/->PgSqlTimeType :timestamp 2 nil)))
          "with precision")
      (is (= "#pg-sql/type [:timestamp :with-time-zone]"
             (pr-str (pg-sqlt/->PgSqlTimeType :timestamp nil true)))
          "with time zone")
      (is (= "#pg-sql/type [:timestamp :without-time-zone]"
             (pr-str (pg-sqlt/->PgSqlTimeType :timestamp nil false)))
          "without time zone")
      (is (= "#pg-sql/type [:timestamp [3] :with-time-zone]"
             (pr-str (pg-sqlt/->PgSqlTimeType :timestamp 3 true)))
          "with precision and time zone"))
    (testing "intervals"
      (is (= "#pg-sql/type [:interval]"
             (pr-str (pg-sqlt/->PgSqlIntervalType nil nil)))
          "without field or precision")
      (is (= "#pg-sql/type [:interval [2]]"
             (pr-str (pg-sqlt/->PgSqlIntervalType nil 2)))
          "with precision")
      (is (= "#pg-sql/type [:interval :day-to-hour]"
             (pr-str (pg-sqlt/->PgSqlIntervalType :day-to-hour nil)))
          "with field")
      (is (= "#pg-sql/type [:interval :hour-to-second [3]]"
             (pr-str (pg-sqlt/->PgSqlIntervalType :hour-to-second 3)))
          "with field and precision"))
    (testing "arrays"
      (is (= "#pg-sql/type [:integer :array]"
             (pr-str (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [] true)))
          "standard SQL array without size")
      (is (= "#pg-sql/type [:integer :array [4]]"
             (pr-str (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4]] true)))
          "standard SQL array with size")
      (is (= "#pg-sql/type [:integer []]"
             (pr-str (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[]] false)))
          "PostgreSQL array without size")
      (is (= "#pg-sql/type [:integer [4]]"
             (pr-str (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4]] false)))
          "PostgreSQL array with size")
      (is (= "#pg-sql/type [:integer [4] [] [2] []]"
             (pr-str (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4] [] [2] []] false)))
          "PostgreSQL array with and without sizes")
      ;; note: the ClojureScript printer isn't pro
      #?(:clj (is (= "#pg-sql/type [#sql/raw \"INTEGER\" [4] [] [2] []]"
                     (pr-str (pg-sqlt/->PgSqlArrayType (sql/raw "INTEGER") [[4] [] [2] []] false)))
                  "PostgreSQL array with raw type"))))
  (testing "reading"
    (is (= (pg-sqlt/->PgSqlSimpleType :integer)
           (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                            "#pg-sql/type [:integer]"))
        "simple type")
    (testing "arbitrary precision"
      (is (= (pg-sqlt/->PgSqlArbitraryPrecisionType :numeric nil nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:numeric]"))
          "without precision or scale")
      (is (= (pg-sqlt/->PgSqlArbitraryPrecisionType :decimal 20 nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:decimal [20]]"))
          "with precision")
      (is (= (pg-sqlt/->PgSqlArbitraryPrecisionType :numeric 20 4)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:numeric [20 4]]"))
          "with precision and precision"))
    (testing "floats"
      (is (= (pg-sqlt/->PgSqlFloatType nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:float]"))
          "without precision")
      (is (= (pg-sqlt/->PgSqlFloatType 2)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:float [2]]"))
          "with precision"))
    (testing "strings"
      (is (= (pg-sqlt/->PgSqlStringType :varchar nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:varchar]"))
          "without length")
      (is (= (pg-sqlt/->PgSqlStringType :varbit 2)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:varbit [2]]"))
          "with length"))
    (testing "times"
      (is (= (pg-sqlt/->PgSqlTimeType :time nil nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:time]"))
          "without precision or time zone")
      (is (= (pg-sqlt/->PgSqlTimeType :timestamp 2 nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:timestamp [2]]"))
          "with precision")
      (is (= (pg-sqlt/->PgSqlTimeType :time nil true)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:time :with-time-zone]"))
          "with time zone")
      (is (= (pg-sqlt/->PgSqlTimeType :time nil false)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:time :without-time-zone]"))
          "without time zone")
      (is (= (pg-sqlt/->PgSqlTimeType :timestamp 4 true)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:timestamp [4] :with-time-zone]"))
          "with precision and time zone"))
    (testing "intervals"
      (is (= (pg-sqlt/->PgSqlIntervalType nil nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:interval]"))
          "without field or precision")
      (is (= (pg-sqlt/->PgSqlIntervalType nil 2)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:interval [2]]"))
          "with precision")
      (is (= (pg-sqlt/->PgSqlIntervalType :year-to-month nil)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:interval :year-to-month]"))
          "with field")
      (is (= (pg-sqlt/->PgSqlIntervalType :minute-to-second 4)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:interval :minute-to-second [4]]"))
          "with field and precision"))
    (testing "arrays"
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [] true)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:integer :array]"))
          "standard SQL array without size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4]] true)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:integer :array [4]]"))
          "standard SQL array with size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[]] false)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:integer []]"))
          "PostgreSQL array without size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4]] false)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:integer [4]]"))
          "PostgreSQL array with size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4] [] [2] []] false)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type}}
                              "#pg-sql/type [:integer [4] [] [2] []]"))
          "PostgreSQL array with and without sizes")
      (is (= (pg-sqlt/->PgSqlArrayType (sql/raw "INTEGER") [[4] [] [2] []] false)
             (edn/read-string {:readers {'pg-sql/type honeysql-postgres.ddl-grammar.data-type/parse-pg-sql-type
                                         'sql/raw honeysql.types/read-sql-raw}}
                              "#pg-sql/type [#sql/raw \"INTEGER\" [4] [] [2] []]"))
          "PostgreSQL array with sql/raw type"))))
