(ns honeysql-postgres.ddl-grammar-test
  (:require [clojure.test :refer [deftest is testing]]
            [honeysql.core :as sql]
            [honeysql-postgres.ddl-grammar :as ddl]
            [honeysql-postgres.parser.protocol :as p]
            [honeysql-postgres.types :as pg-sqlt]))

(deftest test-table-constraint
  (testing "CHECK constraint"
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false}) nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b]]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:name :foo :predicate [:= :a :b] :no-inherit? false}) nil]
           (p/parse @#'ddl/table-constraint [:constraint :foo :check [:= :a :b]]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? true}) nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b] :no-inherit]))
        "constraint with no-inherit")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? true}) nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b] :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? false}) nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b] :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :deferred}) nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b] :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :immediate}) nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b] :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b]
                                                :no-inherit? false
                                                :deferrable? true
                                                :initially :immediate})
            nil]
           (p/parse @#'ddl/table-constraint [:check [:= :a :b] :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "UNIQUE constraint"
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:columns [:a]}) nil]
           (p/parse @#'ddl/table-constraint [:unique [:a]]))
        "a single column")
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:columns [:a :b :c]}) nil]
           (p/parse @#'ddl/table-constraint [:unique [:a :b :c]]))
        "multiple columns")
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:name :foo :columns [:a :b :c]}) nil]
           (p/parse @#'ddl/table-constraint [:constraint :foo :unique [:a :b :c]]))
        "named constraint"))
  (testing "PRIMARY KEY constraint"
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:columns [:a]}) nil]
           (p/parse @#'ddl/table-constraint [:primary-key [:a]]))
        "a single column")
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:columns [:a :b :c]}) nil]
           (p/parse @#'ddl/table-constraint [:primary-key [:a :b :c]]))
        "multiple columns")
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:name :foo :columns [:a :b :c]}) nil]
           (p/parse @#'ddl/table-constraint [:constraint :foo :primary-key [:a :b :c]]))
        "named constraint"))
  (testing "FOREIGN KEY constraint"
    (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a] :table :foo}) nil]
           (p/parse @#'ddl/table-constraint [:foreign-key [:a] :references :foo]))
        "a single column")
    (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo}) nil]
           (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo]))
        "multiple columns")
    (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :references-columns [:d :e :f]}) nil]
           (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo [:d :e :f]]))
        "with reference table columns")
    (testing "MATCH clause"
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c]
                                                       :table :foo
                                                       :references-columns [:d :e :f]
                                                       :match :full}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo [:d :e :f] :match :full]))
          "with MATCH FULL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :match :partial}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :match :partial]))
          "with MATCH PARTIAL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :match :simple}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :match :simple]))
          "with MATCH SIMPLE"))
    (testing "ON DELETE clause"
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c]
                                                       :table :foo
                                                       :match :simple
                                                       :on-delete :no-action}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :match :simple :on-delete :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :restrict}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-delete :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :cascade}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-delete :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :set-null}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-delete :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :set-default}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-delete :set-default]))
          "SET DEFAULT"))
    (testing "ON UPDATE clause"
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c]
                                                       :table :foo
                                                       :match :simple
                                                       :on-delete :no-action
                                                       :on-update :no-action}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :match :simple :on-delete :no-action :on-update :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :restrict}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-update :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :cascade}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-update :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :set-null}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-update :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :set-default}) nil]
             (p/parse @#'ddl/table-constraint [:foreign-key [:a :b :c] :references :foo :on-update :set-default]))
          "SET DEFAULT"))))

(deftest test-column-constraint
  (testing "NOT NULL"
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false}) nil]
           (p/parse @#'ddl/column-constraint [:not-null]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:name :foo :allow-null? false}) nil]
           (p/parse @#'ddl/column-constraint [:constraint :foo :not-null]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :deferrable? true}) nil]
           (p/parse @#'ddl/column-constraint [:not-null :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :deferrable? false}) nil]
           (p/parse @#'ddl/column-constraint [:not-null :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :initially :deferred}) nil]
           (p/parse @#'ddl/column-constraint [:not-null :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :initially :immediate}) nil]
           (p/parse @#'ddl/column-constraint [:not-null :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :deferrable? true :initially :immediate}) nil]
           (p/parse @#'ddl/column-constraint [:not-null :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "NULL"
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true}) nil]
           (p/parse @#'ddl/column-constraint [:null]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:name :foo :allow-null? true}) nil]
           (p/parse @#'ddl/column-constraint [:constraint :foo :null]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :deferrable? true}) nil]
           (p/parse @#'ddl/column-constraint [:null :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :deferrable? false}) nil]
           (p/parse @#'ddl/column-constraint [:null :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :initially :deferred}) nil]
           (p/parse @#'ddl/column-constraint [:null :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :initially :immediate}) nil]
           (p/parse @#'ddl/column-constraint [:null :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :deferrable? true :initially :immediate}) nil]
           (p/parse @#'ddl/column-constraint [:null :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "CHECK constraint"
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b]]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:name :foo :predicate [:= :a :b] :no-inherit? false}) nil]
           (p/parse @#'ddl/column-constraint [:constraint :foo :check [:= :a :b]]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? true}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b] :no-inherit]))
        "constraint with no-inherit")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? true}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b] :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? false}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b] :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :deferred}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b] :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :immediate}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b] :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b]
                                                :no-inherit? false
                                                :deferrable? true
                                                :initially :immediate}) nil]
           (p/parse @#'ddl/column-constraint [:check [:= :a :b] :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "DEFAULT value"
    (is (= [(pg-sqlt/map->PgSqlDefaultConstraint {:value :foo}) nil]
           (p/parse @#'ddl/column-constraint [:default :foo]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlDefaultConstraint {:name :foo :value :foo}) nil]
           (p/parse @#'ddl/column-constraint [:constraint :foo :default :foo]))
        "named constraint"))
  (testing "UNIQUE constraint"
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {}) nil]
           (p/parse @#'ddl/column-constraint [:unique]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:name :foo}) nil]
           (p/parse @#'ddl/column-constraint [:constraint :foo :unique]))
        "named constraint"))
  (testing "PRIMARY KEY constraint"
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {}) nil]
           (p/parse @#'ddl/column-constraint [:primary-key]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:name :foo}) nil]
           (p/parse @#'ddl/column-constraint [:constraint :foo :primary-key]))
        "named constraint"))
  (testing "REFERENCES constraint"
    (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo}) nil]
           (p/parse @#'ddl/column-constraint [:references :foo]))
        "with no column")
    (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:references-column :a :table :foo}) nil]
           (p/parse @#'ddl/column-constraint [:references :foo [:a]]))
        "with a column")
    (testing "MATCH clause"
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :references-column :a :match :full}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo [:a] :match :full]))
          "with MATCH FULL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :match :partial}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :match :partial]))
          "with MATCH PARTIAL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :match :simple}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :match :simple]))
          "with MATCH SIMPLE"))
    (testing "ON DELETE clause"
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo
                                                       :references-column :a
                                                       :match :simple
                                                       :on-delete :no-action}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo [:a] :match :simple :on-delete :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :restrict}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-delete :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :cascade}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-delete :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :set-null}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-delete :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :set-default}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-delete :set-default]))
          "SET DEFAULT"))
     (testing "ON UPDATE clause"
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo
                                                       :references-column :a
                                                       :match :simple
                                                       :on-update :no-action
                                                       :on-delete :no-action}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo [:a] :match :simple :on-delete :no-action :on-update :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :restrict}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-update :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :cascade}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-update :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :set-null}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-update :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :set-default}) nil]
             (p/parse @#'ddl/column-constraint [:references :foo :on-update :set-default]))
          "SET DEFAULT"))))

(deftest test-column-definition
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :integer) nil []) nil]
         (p/parse @#'ddl/column-definition [:foo :integer]))
      "basic column definition")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :integer) nil [(pg-sqlt/map->PgSqlDefaultConstraint {:value (sql/raw 2)})]) nil]
         (p/parse @#'ddl/column-definition [:foo :integer :default (sql/raw 2)]))
      "basic column definition with a default value")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :integer) nil [(pg-sqlt/map->PgSqlDefaultConstraint {:value (sql/raw 2)})]) nil]
         (p/parse @#'ddl/column-definition [:foo :integer :with-options :default (sql/raw 2)]))
      "basic column definition with WITH OPTIONS a default value")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo
                                           (pg-sqlt/->PgSqlSimpleType :integer)
                                           nil
                                           [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false})
                                            (pg-sqlt/map->PgSqlUniqueConstraint {})])
          nil]
         (p/parse @#'ddl/column-definition [:foo :integer :not-null :unique]))
      "multiple column constraints"))

(deftest test-parse-row
  (is (= [] (ddl/parse-row []))
      "empty row")
  (is (= [:foo :int :blah] (ddl/parse-row [:foo :int :blah]))
      "extra tokens")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :int) nil [])]
         (ddl/parse-row [:foo :int]))
      "valid column")
  (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:name :foo :columns [:a :b]})]
         (ddl/parse-row [:constraint :foo :unique [:a :b]]))
      "valid table constraint"))
