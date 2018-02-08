(ns honeysql-postgres.ddl-grammar
  (:require [honeysql.format :as sqlf]
            [honeysql-postgres.ddl-grammar.common :refer [to-sql]]
            [honeysql-postgres.ddl-grammar.data-type :as dt]
            [honeysql-postgres.parser :as p]
            [honeysql-postgres.types :as pg-sqlt]))

(def ^:private columns
  "Parser for a vector of one or more columns names."
  (p/map first
         (p/vector (p/+ to-sql))))

(def ^:private constraint-name
  "Parser for specifying a name for a constraint."
  (p/map second
         (p/seq (p/lit :constraint)
                to-sql)))

(def ^:private check-constraint
  "Parser for CHECK table/column constraints."
  (p/map (fn [[_ p ni?]] (pg-sqlt/->PgSqlCheckConstraint nil p ni? nil nil))
         (p/seq (p/lit :check)
                to-sql
                (p/map #(= :no-inherit %)
                       (p/? (p/lit :no-inherit))))))

(def ^:private index-parameters
  "Specification for index parameters in UNIQUE, PRIMARY KEY, and EXCLUDE
  constraints"
  ;; TODO: actually define
  (p/map #(if (empty? %) nil %)
         (p/seq)))

(def ^:private unique-table-constraint
  "Parser for a UNIQUE table constraint."
  (p/map (fn [[_ c ip]] (pg-sqlt/->PgSqlUniqueConstraint nil c ip nil nil))
         (p/seq (p/lit :unique)
                columns
                (p/? index-parameters))))

(def ^:private primary-key-table-constraint
  "Parser for a PRIMARY KEY table constraint."
  (p/map (fn [[_ c ip]] (pg-sqlt/->PgSqlPrimaryKeyConstraint nil c ip nil nil))
         (p/seq (p/lit :primary-key)
                columns
                (p/? index-parameters))))

(def ^:private match-clause
  "Parser for the MATCH clause in a FOREIGN KEY or REFERENCES constraint."
  (p/map second
         (p/seq (p/lit :match)
                (p/one-of :full :partial :simple))))

(defn ^:private reference-action
  "Parser for a reference action specification for the given token."
  [token]
  (p/map second
         (p/seq (p/lit token)
                (p/one-of :no-action :restrict :cascade :set-null :set-default))))

(def ^:private foreign-key-table-constraint
  "Parser for a FOREGIN KEY table constraint"
  (p/map (fn [[_ c _ t rc m od ou]]
           (pg-sqlt/->PgSqlForeignKeyConstraint nil c t rc m od ou nil nil))
         (p/seq (p/lit :foreign-key)
                columns
                (p/lit :references)
                to-sql
                (p/? columns)
                (p/? match-clause)
                (p/? (reference-action :on-delete))
                (p/? (reference-action :on-update)))))

(def ^:private deferrable
  "Parser for the suffix that determines whether a constraint is deferrable."
  (p/map {:deferrable true
          :not-deferrable false}
         (p/one-of :deferrable :not-deferrable)))

(def ^:private initially
  "Parser for the suffix that determines whether a constraint is initially deferred"
  (p/map second
         (p/seq (p/lit :initially)
                (p/one-of :immediate :deferred))))

(defn ^:private parse-constraint
  "Creates a full constraint parser that parses a name, the constraint,
  optional deferrable, and optional initially."
  [constraint-parser]
  (p/map (fn [[name constraint deferrable? initially]]
           (-> constraint
               (assoc :name name)
               (assoc :deferrable? deferrable?)
               (assoc :initially initially)))
         (p/seq (p/? constraint-name)
                constraint-parser
                (p/? deferrable)
                (p/? initially))))

(def ^:private table-constraint
  "Parser for a table constraint"
  (parse-constraint ;; TODO: EXCLUDE constraint
                    (p/| check-constraint
                         unique-table-constraint
                         primary-key-table-constraint
                         foreign-key-table-constraint)))

(def ^:private null-constraint
  "Parser for a NULL or NOT NULL column constraint."
  (p/map #(pg-sqlt/->PgSqlNullConstraint nil (= :null %) nil nil)
         (p/one-of :null :not-null)))

(def ^:private default-value
  "Parser for a default value"
  (p/map #(pg-sqlt/->PgSqlDefaultConstraint nil (second %) nil nil)
         (p/seq (p/lit :default)
                to-sql)))

(def ^:private unique-column-constraint
  "Parser for a UNIQUE column constraint."
  (p/map #(pg-sqlt/->PgSqlUniqueConstraint nil nil (second %) nil nil)
         (p/seq (p/lit :unique)
                (p/? index-parameters))))

(def ^:private primary-key-column-constraint
  "PArser for a PRIMARY KEY column constraint."
  (p/map #(pg-sqlt/->PgSqlPrimaryKeyConstraint nil nil (second %) nil nil)
         (p/seq (p/lit :primary-key)
                (p/? index-parameters))))

(def ^:private references-constraint
  "PArser for a REFERENCES column constraint."
  (p/map (fn [[_ t rc m od ou]]
           (pg-sqlt/->PgSqlReferencesConstraint nil t rc m od ou nil nil))
         (p/seq (p/lit :references)
                to-sql
                (p/? (p/map first (p/vector to-sql)))
                (p/? match-clause)
                (p/? (reference-action :on-delete))
                (p/? (reference-action :on-update)))))

(def ^:private column-constraint
  "Parser for a column constraint"
  (parse-constraint ;; TODO: GENERATED constraint
                    (p/| null-constraint
                         check-constraint
                         default-value
                         unique-column-constraint
                         primary-key-column-constraint
                         references-constraint)))

(def ^:private column-definition
  "Parser for a column definition"
  (p/map #(apply pg-sqlt/->PgSqlColumnDefinition %)
         (p/seq to-sql
                dt/type
                (p/? (p/map #{:sith-options} (p/lit :with-options)))
                (p/* column-constraint))))

(defn parse-row
  "Parses a row of a table DDL returning either the parsed row or the original row."
  [row]
  (let [result (p/parse (p/map butlast
                               (p/seq (p/| table-constraint column-definition)
                                      p/ε))
                        row)]
    (if (= ::p/no-match result)
      row
      result)))

(defn parse-type
  "Parses an argument list as a type, throwing an exception if the parsing fails."
  [args]
  (p/parse (p/map first (p/seq dt/type p/ε)) args :throw? true))

(defn read-pg-sql-type
  "Reader function for pg-sql/type tagged literals."
  [args]
  (#?(:clj (resolve `parse-type) :cljs parse-type) args))

(defn parse-constraint
  "Parses an argument list as a constraint, throwing an exception if the parsing fails."
  [args]
  (p/parse (p/map first (p/seq (p/| table-constraint column-constraint)
                               p/ε))
           args
           :throw? true))

(defn read-pg-sql-constraint
  "Reader function for pg-sql/constraint tagged literals."
  [args]
  (#?(:clj (resolve `parse-constraint) :cljs parse-constraint) args))
