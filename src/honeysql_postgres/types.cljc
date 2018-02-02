(ns honeysql-postgres.types
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

(defrecord PgSqlCheckConstraint [name predicate no-inherit? deferrable? initially])

(defrecord PgSqlUniqueConstraint [name columns index-parameters deferrable? initially])

(defrecord PgSqlPrimaryKeyConstraint [name columns index-parameters deferrable? initially])

(defrecord PgSqlForeignKeyConstraint [name columns table references-columns match on-delete on-update deferrable? initially])

(defrecord PgSqlNullConstraint [name allow-null? deferrable? initially])

(defrecord PgSqlDefaultConstraint [name value deferrable? initially])

(defrecord PgSqlReferencesConstraint [name table references-column match on-delete on-update deferrable? initially])

(defrecord PgSqlColumnDefinition [name data-type with-options? constraints])


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
             (-write out (print-type o)))))
