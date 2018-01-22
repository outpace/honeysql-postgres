(ns honeysql-postgres.ddl-grammar-test
  (:require [clojure.test :refer [deftest is testing]]
            [honeysql.core :as sql]
            [honeysql-postgres.ddl-grammar :as ddl]
            [honeysql-postgres.parser.protocol :as p]
            [honeysql-postgres.types :as pg-sqlt]))

(deftest test-table-constraint :state
  (testing "CHECK constraint"
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b]]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:name :foo :predicate [:= :a :b] :no-inherit? false}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:constraint :foo :check [:= :a :b]]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? true}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b] :no-inherit]))
        "constraint with no-inherit")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? true}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b] :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? false}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b] :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :deferred}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b] :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :immediate}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b] :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b]
                                                :no-inherit? false
                                                :deferrable? true
                                                :initially :immediate})
            :state nil]
           (p/parse @#'ddl/table-constraint :state [:check [:= :a :b] :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "UNIQUE constraint"
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:columns [:a]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:unique [:a]]))
        "a single column")
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:columns [:a :b :c]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:unique [:a :b :c]]))
        "multiple columns")
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:name :foo :columns [:a :b :c]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:constraint :foo :unique [:a :b :c]]))
        "named constraint"))
  (testing "PRIMARY KEY constraint"
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:columns [:a]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:primary-key [:a]]))
        "a single column")
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:columns [:a :b :c]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:primary-key [:a :b :c]]))
        "multiple columns")
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:name :foo :columns [:a :b :c]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:constraint :foo :primary-key [:a :b :c]]))
        "named constraint"))
  (testing "FOREIGN KEY constraint"
    (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a] :table :foo}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:foreign-key [:a] :references :foo]))
        "a single column")
    (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo]))
        "multiple columns")
    (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :references-columns [:d :e :f]}) :state nil]
           (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo [:d :e :f]]))
        "with reference table columns")
    (testing "MATCH clause"
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c]
                                                       :table :foo
                                                       :references-columns [:d :e :f]
                                                       :match :full}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo [:d :e :f] :match :full]))
          "with MATCH FULL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :match :partial}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :match :partial]))
          "with MATCH PARTIAL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :match :simple}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :match :simple]))
          "with MATCH SIMPLE"))
    (testing "ON DELETE clause"
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c]
                                                       :table :foo
                                                       :match :simple
                                                       :on-delete :no-action}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :match :simple :on-delete :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :restrict}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-delete :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :cascade}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-delete :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :set-null}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-delete :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-delete :set-default}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-delete :set-default]))
          "SET DEFAULT"))
    (testing "ON UPDATE clause"
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c]
                                                       :table :foo
                                                       :match :simple
                                                       :on-delete :no-action
                                                       :on-update :no-action}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :match :simple :on-delete :no-action :on-update :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :restrict}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-update :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :cascade}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-update :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :set-null}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-update :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlForeignKeyConstraint {:columns [:a :b :c] :table :foo :on-update :set-default}) :state nil]
             (p/parse @#'ddl/table-constraint :state [:foreign-key [:a :b :c] :references :foo :on-update :set-default]))
          "SET DEFAULT"))))

(deftest test-column-constraint :state
  (testing "NOT NULL"
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:not-null]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:name :foo :allow-null? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:constraint :foo :not-null]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :deferrable? true}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:not-null :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :deferrable? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:not-null :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :initially :deferred}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:not-null :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :initially :immediate}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:not-null :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false :deferrable? true :initially :immediate}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:not-null :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "NULL"
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:null]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:name :foo :allow-null? true}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:constraint :foo :null]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :deferrable? true}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:null :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :deferrable? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:null :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :initially :deferred}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:null :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :initially :immediate}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:null :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? true :deferrable? true :initially :immediate}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:null :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "CHECK constraint"
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b]]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:name :foo :predicate [:= :a :b] :no-inherit? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:constraint :foo :check [:= :a :b]]))
        "named constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? true}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b] :no-inherit]))
        "constraint with no-inherit")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? true}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b] :deferrable]))
        "deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :deferrable? false}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b] :not-deferrable]))
        "not deferrable constraint")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :deferred}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b] :initially :deferred]))
        "initially deferred")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b] :no-inherit? false :initially :immediate}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b] :initially :immediate]))
        "initially immediate")
    (is (= [(pg-sqlt/map->PgSqlCheckConstraint {:predicate [:= :a :b]
                                                :no-inherit? false
                                                :deferrable? true
                                                :initially :immediate}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:check [:= :a :b] :deferrable :initially :immediate]))
        "initially immediate but deferrable"))
  (testing "DEFAULT value"
    (is (= [(pg-sqlt/map->PgSqlDefaultConstraint {:value :foo}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:default :foo]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlDefaultConstraint {:name :foo :value :foo}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:constraint :foo :default :foo]))
        "named constraint"))
  (testing "UNIQUE constraint"
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:unique]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlUniqueConstraint {:name :foo}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:constraint :foo :unique]))
        "named constraint"))
  (testing "PRIMARY KEY constraint"
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:primary-key]))
        "basic constraint")
    (is (= [(pg-sqlt/map->PgSqlPrimaryKeyConstraint {:name :foo}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:constraint :foo :primary-key]))
        "named constraint"))
  (testing "REFERENCES constraint"
    (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:references :foo]))
        "with no column")
    (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:references-column :a :table :foo}) :state nil]
           (p/parse @#'ddl/column-constraint :state [:references :foo [:a]]))
        "with a column")
    (testing "MATCH clause"
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :references-column :a :match :full}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo [:a] :match :full]))
          "with MATCH FULL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :match :partial}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :match :partial]))
          "with MATCH PARTIAL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :match :simple}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :match :simple]))
          "with MATCH SIMPLE"))
    (testing "ON DELETE clause"
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo
                                                       :references-column :a
                                                       :match :simple
                                                       :on-delete :no-action}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo [:a] :match :simple :on-delete :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :restrict}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-delete :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :cascade}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-delete :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :set-null}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-delete :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-delete :set-default}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-delete :set-default]))
          "SET DEFAULT"))
     (testing "ON UPDATE clause"
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo
                                                       :references-column :a
                                                       :match :simple
                                                       :on-update :no-action
                                                       :on-delete :no-action}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo [:a] :match :simple :on-delete :no-action :on-update :no-action]))
          "NO ACTION")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :restrict}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-update :restrict]))
          "RESTRICT")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :cascade}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-update :cascade]))
          "CASCADE")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :set-null}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-update :set-null]))
          "SET NULL")
      (is (= [(pg-sqlt/map->PgSqlReferencesConstraint {:table :foo :on-update :set-default}) :state nil]
             (p/parse @#'ddl/column-constraint :state [:references :foo :on-update :set-default]))
          "SET DEFAULT"))))

(deftest test-column-definition
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :integer) nil []) :state nil]
         (p/parse @#'ddl/column-definition :state [:foo :integer]))
      "basic column definition")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :integer) nil [(pg-sqlt/map->PgSqlDefaultConstraint {:value (sql/raw 2)})]) :state nil]
         (p/parse @#'ddl/column-definition :state [:foo :integer :default (sql/raw 2)]))
      "basic column definition with a default value")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo (pg-sqlt/->PgSqlSimpleType :integer) nil [(pg-sqlt/map->PgSqlDefaultConstraint {:value (sql/raw 2)})]) :state nil]
         (p/parse @#'ddl/column-definition :state [:foo :integer :with-options :default (sql/raw 2)]))
      "basic column definition with WITH OPTIONS a default value")
  (is (= [(pg-sqlt/->PgSqlColumnDefinition :foo
                                           (pg-sqlt/->PgSqlSimpleType :integer)
                                           nil
                                           [(pg-sqlt/map->PgSqlNullConstraint {:allow-null? false})
                                            (pg-sqlt/map->PgSqlUniqueConstraint {})])
          :state
          nil]
         (p/parse @#'ddl/column-definition :state [:foo :integer :not-null :unique]))
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
