(ns honeysql-postgres.ddl-grammar.common
  (:require [honeysql.format :as sqlf]
            [honeysql-postgres.parser :as p]))

(def to-sql
  "Parser that accepts anything that satisifes the ToSql protocol."
  (p/pred #(satisfies? sqlf/ToSql %) "(satisfies? ToSql)"))
