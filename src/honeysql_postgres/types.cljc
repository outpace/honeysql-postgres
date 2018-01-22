(ns honeysql-postgres.types)

(defrecord PgSqlSimpleType [type])
(defrecord PgSqlArbitraryPrecisionType [type precision scale])
(defrecord PgSqlFloatType [type precision])
(defrecord PgSqlStringType [type length])
(defrecord PgSqlTimeType [type precision time-zone?])
(defrecord PgSqlIntervalType [field precision])
(defrecord SqlArrayType [base-type size])
(defrecord PgSqlArrayType [base-type dimensions])

(defprotocol PgSqlConstraint
  (with-name [_ n])
  (with-deferrable? [_ d?])
  (with-initially [_ i]))

(defrecord PgSqlCheckConstraint [name predicate no-inherit? deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlCheckConstraint. n predicate no-inherit? deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlCheckConstraint. name predicate no-inherit? d? initially))
  (with-initially [_ i]
    (PgSqlCheckConstraint. name predicate no-inherit? deferrable? i)))

(defrecord PgSqlUniqueConstraint [name columns index-parameters deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlUniqueConstraint. n columns index-parameters deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlUniqueConstraint. name columns index-parameters d? initially))
  (with-initially [_ i]
    (PgSqlUniqueConstraint. name columns index-parameters deferrable? i)))

(defrecord PgSqlPrimaryKeyConstraint [name columns index-parameters deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlPrimaryKeyConstraint. n columns index-parameters deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlPrimaryKeyConstraint. name columns index-parameters d? initially))
  (with-initially [_ i]
    (PgSqlPrimaryKeyConstraint. name columns index-parameters deferrable? i)))

(defrecord PgSqlForeignKeyConstraint [name columns table references-columns match on-delete on-update deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlForeignKeyConstraint. n columns table references-columns match on-delete on-update deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlForeignKeyConstraint. name columns table references-columns match on-delete on-update d? initially))
  (with-initially [_ i]
    (PgSqlForeignKeyConstraint. name columns table references-columns match on-delete on-update deferrable? i)))

(defrecord PgSqlNullConstraint [name allow-null? deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlNullConstraint. n allow-null? deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlNullConstraint. name allow-null? d? initially))
  (with-initially [_ i]
    (PgSqlNullConstraint. name allow-null? deferrable? i)))

(defrecord PgSqlDefaultConstraint [name value deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlDefaultConstraint. n value deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlDefaultConstraint. name value d? initially))
  (with-initially [_ i]
    (PgSqlDefaultConstraint. name value deferrable? i)))

(defrecord PgSqlReferencesConstraint [name table references-column match on-delete on-update deferrable? initially]
  PgSqlConstraint
  (with-name [_ n]
    (PgSqlReferencesConstraint. n table references-column match on-delete on-update deferrable? initially))
  (with-deferrable? [_ d?]
    (PgSqlReferencesConstraint. name table references-column match on-delete on-update d? initially))
  (with-initially [_ i]
    (PgSqlReferencesConstraint. name table references-column match on-delete on-update deferrable? i)))

(defrecord PgSqlColumnDefinition [name data-type with-options? constraints])
