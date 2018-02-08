(ns honeysql-postgres.types-test
  (:require [#?(:clj clojure.edn :cljs cljs.reader) :as edn]
            [clojure.test :refer [deftest is testing]]
            [honeysql.core :as sql]
            [honeysql.types :as sqlt]
            [honeysql-postgres.ddl-grammar :as ddl]
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
           (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                            "#pg-sql/type [:integer]"))
        "simple type")
    (testing "arbitrary precision"
      (is (= (pg-sqlt/->PgSqlArbitraryPrecisionType :numeric nil nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:numeric]"))
          "without precision or scale")
      (is (= (pg-sqlt/->PgSqlArbitraryPrecisionType :decimal 20 nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:decimal [20]]"))
          "with precision")
      (is (= (pg-sqlt/->PgSqlArbitraryPrecisionType :numeric 20 4)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:numeric [20 4]]"))
          "with precision and precision"))
    (testing "floats"
      (is (= (pg-sqlt/->PgSqlFloatType nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:float]"))
          "without precision")
      (is (= (pg-sqlt/->PgSqlFloatType 2)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:float [2]]"))
          "with precision"))
    (testing "strings"
      (is (= (pg-sqlt/->PgSqlStringType :varchar nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:varchar]"))
          "without length")
      (is (= (pg-sqlt/->PgSqlStringType :varbit 2)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:varbit [2]]"))
          "with length"))
    (testing "times"
      (is (= (pg-sqlt/->PgSqlTimeType :time nil nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:time]"))
          "without precision or time zone")
      (is (= (pg-sqlt/->PgSqlTimeType :timestamp 2 nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:timestamp [2]]"))
          "with precision")
      (is (= (pg-sqlt/->PgSqlTimeType :time nil true)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:time :with-time-zone]"))
          "with time zone")
      (is (= (pg-sqlt/->PgSqlTimeType :time nil false)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:time :without-time-zone]"))
          "without time zone")
      (is (= (pg-sqlt/->PgSqlTimeType :timestamp 4 true)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:timestamp [4] :with-time-zone]"))
          "with precision and time zone"))
    (testing "intervals"
      (is (= (pg-sqlt/->PgSqlIntervalType nil nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:interval]"))
          "without field or precision")
      (is (= (pg-sqlt/->PgSqlIntervalType nil 2)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:interval [2]]"))
          "with precision")
      (is (= (pg-sqlt/->PgSqlIntervalType :year-to-month nil)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:interval :year-to-month]"))
          "with field")
      (is (= (pg-sqlt/->PgSqlIntervalType :minute-to-second 4)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:interval :minute-to-second [4]]"))
          "with field and precision"))
    (testing "arrays"
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [] true)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:integer :array]"))
          "standard SQL array without size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4]] true)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:integer :array [4]]"))
          "standard SQL array with size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[]] false)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:integer []]"))
          "PostgreSQL array without size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4]] false)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:integer [4]]"))
          "PostgreSQL array with size")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlSimpleType :integer) [[4] [] [2] []] false)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type}}
                              "#pg-sql/type [:integer [4] [] [2] []]"))
          "PostgreSQL array with and without sizes")
      (is (= (pg-sqlt/->PgSqlArrayType (pg-sqlt/->PgSqlRawType (sql/raw "INTEGER")) [[4] [] [2] []] false)
             (edn/read-string {:readers {'pg-sql/type ddl/read-pg-sql-type
                                         'sql/raw sqlt/read-sql-raw}}
                              "#pg-sql/type [#sql/raw \"INTEGER\" [4] [] [2] []]"))
          "PostgreSQL array with sql/raw type"))))

(deftest test-constraints
  (testing "printing"
    (testing "CHECK constraints"
      (is (= "#pg-sql/constraint [:check [:= :a :b]]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false nil nil)))
          "just a predicate")
      (is (= "#pg-sql/constraint [:constraint :foo :check [:= :a :b]]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint :foo [:= :a :b] false nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:check [:= :a :b] :no-inherit]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] true nil nil)))
          "with no-inherit flag")
      (is (= "#pg-sql/constraint [:check [:= :a :b] :deferrable]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:check [:= :a :b] :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:check [:= :a :b] :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:check [:= :a :b] :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :check [:= :a :b] :no-inherit :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlCheckConstraint :foo [:= :a :b] true true :immediate)))
          "everything"))
    (testing "UNIQUE constraints"
      (is (= "#pg-sql/constraint [:unique]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint nil nil nil nil nil)))
          "without columns (column constraint)")
      (is (= "#pg-sql/constraint [:unique]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint nil nil nil nil nil)))
          "with columns (table constraint)")
      (is (= "#pg-sql/constraint [:constraint :foo :unique [:a :b]]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint :foo [:a :b] nil nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:unique [:a :b] :deferrable]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:unique [:a :b] :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:unique [:a :b] :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:unique [:a :b] :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :unique [:a :b] :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlUniqueConstraint :foo [:a :b] nil true :immediate)))
          "everything"))
    (testing "PRIMARY KEY constraints"
      (is (= "#pg-sql/constraint [:primary-key]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint nil nil nil nil nil)))
          "without columns (column constraint)")
      (is (= "#pg-sql/constraint [:primary-key]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint nil nil nil nil nil)))
          "with columns (table constraint)")
      (is (= "#pg-sql/constraint [:constraint :foo :primary-key [:a :b]]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint :foo [:a :b] nil nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:primary-key [:a :b] :deferrable]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:primary-key [:a :b] :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:primary-key [:a :b] :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:primary-key [:a :b] :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :primary-key [:a :b] :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlPrimaryKeyConstraint :foo [:a :b] nil true :immediate)))
          "everything"))
    (testing "FOREIGN KEY constraints"
      (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil nil nil)))
          "basic constraint")
      (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo [:x :y]]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo [:x :y] nil nil nil nil nil)))
          "with references columns")
      (testing "with MATCH clause"
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :match :full]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil :full nil nil nil nil)))
            "MATCH FULL")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :match :partial]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil :partial nil nil nil nil)))
            "MATCH PARTIAL")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :match :simple]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil :simple nil nil nil nil)))
            "MATCH SIMPLE"))
      (testing "ON DELETE clause"
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :no-action]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :no-action nil nil nil)))
            "NO ACTION")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :restrict]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :restrict nil nil nil)))
            "RESTRICT")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :cascade]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :cascade nil nil nil)))
            "CASCADE")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :set-null]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :set-null nil nil nil)))
            "SET NULL")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :set-default]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :set-default nil nil nil)))
            "SET DEFAULT"))
      (testing "ON UPDATE clause"
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :no-action]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :no-action nil nil)))
            "NO ACTION")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :restrict]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :restrict nil nil)))
            "RESTRICT")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :cascade]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :cascade nil nil)))
            "CASCADE")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :set-null]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :set-null nil nil)))
            "SET NULL")
        (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :set-default]"
               (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :set-default nil nil)))
            "SET DEFAULT"))
      (is (= "#pg-sql/constraint [:constraint :foo :foreign-key [:a :b] :references :foo]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint :foo [:a :b] :foo nil nil nil nil nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :deferrable]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :foreign-key [:a :b] :references :foo [:c :d] :match :simple :on-delete :cascade :on-update :set-null :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlForeignKeyConstraint :foo [:a :b] :foo [:c :d] :simple :cascade :set-null true :immediate)))
          "everything"))
    (testing "NULL constraints"
      (is (= "#pg-sql/constraint [:null]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil true nil nil)))
          "basic constraint")
      (is (= "#pg-sql/constraint [:constraint :foo :null]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint :foo true nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:null :deferrable]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil true true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:null :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil true false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:null :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil true nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:null :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil true nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :null :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint :foo true true :immediate)))
          "everything"))
    (testing "NOT NULL constraints"
      (is (= "#pg-sql/constraint [:not-null]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil false nil nil)))
          "basic constraint")
      (is (= "#pg-sql/constraint [:constraint :foo :not-null]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint :foo false nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:not-null :deferrable]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil false true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:not-null :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil false false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:not-null :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil false nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:not-null :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint nil false nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :not-null :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlNullConstraint :foo false true :immediate)))
          "everything"))
    (testing "DEFAULT constraints"
      (is (= "#pg-sql/constraint [:default :baz]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint nil :baz nil nil)))
          "basic constraint")
      (is (= "#pg-sql/constraint [:constraint :foo :default :baz]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint :foo :baz nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:default :baz :deferrable]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint nil :baz true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:default :baz :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint nil :baz false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:default :baz :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint nil :baz nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:default :baz :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint nil :baz nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :default :baz :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlDefaultConstraint :foo :baz true :immediate)))
          "everything"))
    (testing "REFERENCES constraints"
      (is (= "#pg-sql/constraint [:references :foo]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil nil nil)))
          "basic constraint")
      (is (= "#pg-sql/constraint [:references :foo [:x]]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo :x nil nil nil nil nil)))
          "with reference columns")
      (testing "with MATCH clause"
        (is (= "#pg-sql/constraint [:references :foo :match :full]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil :full nil nil nil nil)))
            "MATCH FULL")
        (is (= "#pg-sql/constraint [:references :foo :match :partial]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil :partial nil nil nil nil)))
            "MATCH PARTIAL")
        (is (= "#pg-sql/constraint [:references :foo :match :simple]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil :simple nil nil nil nil)))
            "MATCH SIMPLE"))
      (testing "ON DELETE clause"
        (is (= "#pg-sql/constraint [:references :foo :on-delete :no-action]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :no-action nil nil nil)))
            "NO ACTION")
        (is (= "#pg-sql/constraint [:references :foo :on-delete :restrict]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :restrict nil nil nil)))
            "RESTRICT")
        (is (= "#pg-sql/constraint [:references :foo :on-delete :cascade]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :cascade nil nil nil)))
            "CASCADE")
        (is (= "#pg-sql/constraint [:references :foo :on-delete :set-null]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :set-null nil nil nil)))
            "SET NULL")
        (is (= "#pg-sql/constraint [:references :foo :on-delete :set-default]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :set-default nil nil nil)))
            "SET DEFAULT"))
      (testing "ON UPDATE clause"
        (is (= "#pg-sql/constraint [:references :foo :on-update :no-action]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :no-action nil nil)))
            "NO ACTION")
        (is (= "#pg-sql/constraint [:references :foo :on-update :restrict]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :restrict nil nil)))
            "RESTRICT")
        (is (= "#pg-sql/constraint [:references :foo :on-update :cascade]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :cascade nil nil)))
            "CASCADE")
        (is (= "#pg-sql/constraint [:references :foo :on-update :set-null]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :set-null nil nil)))
            "SET NULL")
        (is (= "#pg-sql/constraint [:references :foo :on-update :set-default]"
               (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :set-default nil nil)))
            "SET DEFAULT"))
      (is (= "#pg-sql/constraint [:constraint :foo :references :foo]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint :foo :foo nil nil nil nil nil nil)))
          "named")
      (is (= "#pg-sql/constraint [:references :foo :deferrable]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil true nil)))
          "with deferrable")
      (is (= "#pg-sql/constraint [:references :foo :not-deferrable]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil false nil)))
          "with not deferrable")
      (is (= "#pg-sql/constraint [:references :foo :initially :deferred]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil nil :deferred)))
          "initially deferred")
      (is (= "#pg-sql/constraint [:references :foo :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil nil :immediate)))
          "initially immediate")
      (is (= "#pg-sql/constraint [:constraint :foo :references :foo [:c] :match :simple :on-delete :cascade :on-update :set-null :deferrable :initially :immediate]"
             (pr-str (pg-sqlt/->PgSqlReferencesConstraint :foo :foo :c :simple :cascade :set-null true :immediate)))
          "everything")))
  (testing "reading"
    (testing "CHECK constraints"
      (is (= (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:check [:= :a :b]]"))
          "just a predicate")
      (is (= (pg-sqlt/->PgSqlCheckConstraint :foo [:= :a :b] false nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :check [:= :a :b]]"))
          "named")
      (is (= (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] true nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:check [:= :a :b] :no-inherit]"))
          "with no-inherit flag")
      (is (= (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:check [:= :a :b] :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:check [:= :a :b] :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:check [:= :a :b] :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlCheckConstraint nil [:= :a :b] false nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:check [:= :a :b] :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlCheckConstraint :foo [:= :a :b] true true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :check [:= :a :b] :no-inherit :deferrable :initially :immediate]"))
          "everything"))
    (testing "UNIQUE constraints"
      (is (= (pg-sqlt/->PgSqlUniqueConstraint nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:unique]"))
          "without columns (column constraint)")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:unique]"))
          "with columns (table constraint)")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint :foo [:a :b] nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :unique [:a :b]]"))
          "named")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:unique [:a :b] :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:unique [:a :b] :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:unique [:a :b] :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint nil [:a :b] nil nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:unique [:a :b] :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlUniqueConstraint :foo [:a :b] nil true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :unique [:a :b] :deferrable :initially :immediate]"))
          "everything"))
    (testing "PRIMARY KEY constraints"
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:primary-key]"))
          "without columns (column constraint)")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:primary-key]"))
          "with columns (table constraint)")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint :foo [:a :b] nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :primary-key [:a :b]]"))
          "named")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:primary-key [:a :b] :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:primary-key [:a :b] :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:primary-key [:a :b] :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint nil [:a :b] nil nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:primary-key [:a :b] :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlPrimaryKeyConstraint :foo [:a :b] nil true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :primary-key [:a :b] :deferrable :initially :immediate]"))
          "everything"))
    (testing "FOREIGN KEY constraints"
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:foreign-key [:a :b] :references :foo]"))
          "basic constraint")
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo [:x :y] nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:foreign-key [:a :b] :references :foo [:x :y]]"))
          "with references columns")
      (testing "with MATCH clause"
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil :full nil nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :match :full]"))
            "MATCH FULL")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil :partial nil nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :match :partial]"))
            "MATCH PARTIAL")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil :simple nil nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :match :simple]"))
            "MATCH SIMPLE"))
      (testing "ON DELETE clause"
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :no-action nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :no-action]"))
            "NO ACTION")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :restrict nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :restrict]"))
            "RESTRICT")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :cascade nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :cascade]"))
            "CASCADE")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :set-null nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :set-null]"))
            "SET NULL")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil :set-default nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-delete :set-default]"))
            "SET DEFAULT"))
      (testing "ON UPDATE clause"
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :no-action nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :no-action]"))
            "NO ACTION")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :restrict nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :restrict]"))
            "RESTRICT")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :cascade nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :cascade]"))
            "CASCADE")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :set-null nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :set-null]"))
            "SET NULL")
        (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil :set-default nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :on-update :set-default]"))
            "SET DEFAULT"))
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint :foo [:a :b] :foo nil nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :foreign-key [:a :b] :references :foo]"))
          "named")
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint nil [:a :b] :foo nil nil nil nil nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:foreign-key [:a :b] :references :foo :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlForeignKeyConstraint :foo [:a :b] :foo [:c :d] :simple :cascade :set-null true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :foreign-key [:a :b] :references :foo [:c :d] :match :simple :on-delete :cascade :on-update :set-null :deferrable :initially :immediate]"))
          "everything"))
    (testing "NULL constraints"
      (is (= (pg-sqlt/->PgSqlNullConstraint nil true nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:null]"))
          "basic constraint")
      (is (= (pg-sqlt/->PgSqlNullConstraint :foo true nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :null]"))
          "named")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil true true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:null :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil true false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:null :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil true nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:null :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil true nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:null :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlNullConstraint :foo true true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :null :deferrable :initially :immediate]"))
          "everything"))
    (testing "NOT NULL constraints"
      (is (= (pg-sqlt/->PgSqlNullConstraint nil false nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:not-null]"))
          "basic constraint")
      (is (= (pg-sqlt/->PgSqlNullConstraint :foo false nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :not-null]"))
          "named")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil false true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:not-null :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil false false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:not-null :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil false nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:not-null :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlNullConstraint nil false nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:not-null :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlNullConstraint :foo false true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :not-null :deferrable :initially :immediate]"))
          "everything"))
    (testing "DEFAULT constraints"
      (is (= (pg-sqlt/->PgSqlDefaultConstraint nil :baz nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:default :baz]"))
          "basic constraint")
      (is (= (pg-sqlt/->PgSqlDefaultConstraint :foo :baz nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :default :baz]"))
          "named")
      (is (= (pg-sqlt/->PgSqlDefaultConstraint nil :baz true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:default :baz :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlDefaultConstraint nil :baz false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:default :baz :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlDefaultConstraint nil :baz nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:default :baz :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlDefaultConstraint nil :baz nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:default :baz :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlDefaultConstraint :foo :baz true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :default :baz :deferrable :initially :immediate]"))
          "everything"))
    (testing "REFERNCES constraints"
      (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:references :foo]"))
          "basic constraint")
      (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo :x nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:references :foo [:x]]"))
          "with references columns")
      (testing "with MATCH clause"
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil :full nil nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :match :full]"))
            "MATCH FULL")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil :partial nil nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :match :partial]"))
            "MATCH PARTIAL")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil :simple nil nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :match :simple]"))
            "MATCH SIMPLE"))
      (testing "ON DELETE clause"
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :no-action nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-delete :no-action]"))
            "NO ACTION")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :restrict nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-delete :restrict]"))
            "RESTRICT")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :cascade nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-delete :cascade]"))
            "CASCADE")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :set-null nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-delete :set-null]"))
            "SET NULL")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil :set-default nil nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-delete :set-default]"))
            "SET DEFAULT"))
      (testing "ON UPDATE clause"
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :no-action nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-update :no-action]"))
            "NO ACTION")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :restrict nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-update :restrict]"))
            "RESTRICT")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :cascade nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-update :cascade]"))
            "CASCADE")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :set-null nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-update :set-null]"))
            "SET NULL")
        (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil :set-default nil nil)
               (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                                "#pg-sql/constraint [:references :foo :on-update :set-default]"))
            "SET DEFAULT"))
      (is (= (pg-sqlt/->PgSqlReferencesConstraint :foo :foo nil nil nil nil nil nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :references :foo]"))
          "named")
      (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil true nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:references :foo :deferrable]"))
          "with deferrable")
      (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil false nil)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:references :foo :not-deferrable]"))
          "with not deferrable")
      (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil nil :deferred)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:references :foo :initially :deferred]"))
          "initially deferred")
      (is (= (pg-sqlt/->PgSqlReferencesConstraint nil :foo nil nil nil nil nil :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:references :foo :initially :immediate]"))
          "initially immediate")
      (is (= (pg-sqlt/->PgSqlReferencesConstraint :foo :foo :c :simple :cascade :set-null true :immediate)
             (edn/read-string {:readers {'pg-sql/constraint ddl/read-pg-sql-constraint}}
                              "#pg-sql/constraint [:constraint :foo :references :foo [:c] :match :simple :on-delete :cascade :on-update :set-null :deferrable :initially :immediate]"))
          "everything"))))
