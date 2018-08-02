(ns honeysql-postgres.types
  (:require [clojure.string :as str])
  #?(:clj (:import (java.io Writer))))

(defprotocol PgSqlType
  (unparse-type [_]))

(defrecord PgSqlSimpleType [type]
  PgSqlType
  (unparse-type [_]
    [type]))

(defrecord PgSqlArbitraryPrecisionType [type precision scale]
  PgSqlType
  (unparse-type [_]
    (cond-> [type]
      precision (conj [precision])
      scale (update 1 conj scale))))

(defrecord PgSqlFloatType [precision]
  PgSqlType
  (unparse-type [_]
    (cond-> [:float]
      precision (conj [precision]))))

(defrecord PgSqlStringType [type length]
  PgSqlType
  (unparse-type [_]
    (cond-> [type]
      length (conj [length]))))

(defrecord PgSqlTimeType [type precision time-zone?]
  PgSqlType
  (unparse-type [_]
    (cond-> [type]
      precision (conj [precision])
      (some? time-zone?) (conj (if time-zone?
                                 :with-time-zone
                                 :without-time-zone)))))

(defrecord PgSqlRawType [type]
  PgSqlType
  (unparse-type [_] [type]))

(defrecord PgSqlIntervalType [field precision]
  PgSqlType
  (unparse-type [_]
    (cond-> [:interval]
      field (conj field)
      precision (conj [precision]))))

(defrecord PgSqlArrayType [base-type dimensions standard-format?]
  PgSqlType
  (unparse-type [_]
    (cond-> (if (satisfies? PgSqlType base-type)
              (unparse-type base-type)
              [base-type])
        standard-format? (conj :array)
        dimensions (into dimensions))))

(defn ^:private print-type
  "Used to format a type using a tagged literal."
  [o]
  (str "#pg-sql/type " (pr-str (unparse-type o))))

(defprotocol PgSqlConstraint
  (unparse-constraint [_]))

(defrecord PgSqlCheckConstraint [name predicate no-inherit? deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    (cond-> [:check predicate]
      no-inherit? (conj :no-inherit))))

(defrecord PgSqlUniqueConstraint [name columns index-parameters deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    (cond-> [:unique]
      columns (conj columns))))

(defrecord PgSqlPrimaryKeyConstraint [name columns index-parameters deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    (cond-> [:primary-key]
      columns (conj columns))))

(defrecord PgSqlForeignKeyConstraint [name columns table references-columns match on-delete on-update deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    (cond-> [:foreign-key columns :references table]
      references-columns (conj references-columns)
      match (conj :match match)
      on-delete (conj :on-delete on-delete)
      on-update (conj :on-update on-update))))

(defrecord PgSqlNullConstraint [name allow-null? deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    (if allow-null?
      [:null]
      [:not :null])))

(defrecord PgSqlDefaultConstraint [name value deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    [:default value]))

(defrecord PgSqlReferencesConstraint [name table references-column match on-delete on-update deferrable? initially]
  PgSqlConstraint
  (unparse-constraint [_]
    (cond-> [:references table]
      references-column (conj [references-column])
      match (conj :match match)
      on-delete (conj :on-delete on-delete)
      on-update (conj :on-update on-update))))

(defrecord PgSqlColumnDefinition [name data-type with-options? constraints])

(defn ^:private print-constraint
  [{:keys [name deferrable? initially] :as c}]
  (let [name-prefix (if (some? name) [:constraint name] [])
        body (unparse-constraint c)
        deferrable-suffix (case deferrable?
                            true [:deferrable]
                            false [:not :deferrable]
                            nil [])
        initially-suffix (case initially
                           :immediate [:initially :immediate]
                           :deferred [:initially :deferred]
                           nil [])]
    (str "#pg-sql/constraint "
         (pr-str (into []
                       cat
                       [name-prefix body deferrable-suffix initially-suffix])))))


#?(:clj (do
          (defmethod print-method PgSqlSimpleType [^PgSqlSimpleType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlSimpleType [^PgSqlSimpleType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlArbitraryPrecisionType [^PgSqlArbitraryPrecisionType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlArbitraryPrecisionType [^PgSqlArbitraryPrecisionType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlFloatType [^PgSqlFloatType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlFloatType [^PgSqlFloatType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlStringType [^PgSqlStringType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlStringType [^PgSqlStringType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlTimeType [^PgSqlTimeType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlTimeType [^PgSqlTimeType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlIntervalType [^PgSqlIntervalType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlIntervalType [^PgSqlIntervalType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlRawType [^PgSqlRawType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlRawType [^PgSqlRawType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlArrayType [^PgSqlArrayType o ^Writer w]
            (.write w (print-type o)))
          (defmethod print-dup PgSqlArrayType [^PgSqlArrayType o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlCheckConstraint [^PgSqlCheckConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlCheckConstraint [^PgSqlCheckConstraint o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlUniqueConstraint [^PgSqlUniqueConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlUniqueConstraint [^PgSqlUniqueConstraint o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlPrimaryKeyConstraint [^PgSqlPrimaryKeyConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlPrimaryKeyConstraint [^PgSqlPrimaryKeyConstraint o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlForeignKeyConstraint [^PgSqlForeignKeyConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlForeignKeyConstraint [^PgSqlForeignKeyConstraint o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlNullConstraint [^PgSqlNullConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlNullConstraint [^PgSqlNullConstraint o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlDefaultConstraint [^PgSqlDefaultConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlDefaultConstraint [^PgSqlDefaultConstraint o ^Writer w]
            (print-method o w))

          (defmethod print-method PgSqlReferencesConstraint [^PgSqlReferencesConstraint o ^Writer w]
            (.write w (print-constraint o)))
          (defmethod print-dup PgSqlReferencesConstraint [^PgSqlReferencesConstraint o ^Writer w]
            (print-method o w)))
   :cljs (extend-protocol IPrintWithWriter
           PgSqlSimpleType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlArbitraryPrecisionType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlFloatType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlStringType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlTimeType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlIntervalType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlRawType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlArrayType
           (-pr-writer [o out _]
             (-write out (print-type o)))

           PgSqlCheckConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))

           PgSqlUniqueConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))

           PgSqlPrimaryKeyConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))

           PgSqlForeignKeyConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))

           PgSqlNullConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))

           PgSqlDefaultConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))

           PgSqlReferencesConstraint
           (-pr-writer [o out _]
             (-write out (print-constraint o)))))
