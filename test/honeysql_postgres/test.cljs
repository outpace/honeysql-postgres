(ns honeysql-postgres.test
  (:require [doo.runner :refer-macros [doo-tests]]
            [honeysql-postgres.ddl-grammar]
            [honeysql-postgres.ddl-grammar.data-type-test]
            [honeysql-postgres.parser-test]
            [honeysql-postgres.postgres-test]))

(doo-tests 'honeysql-postgres.ddl-grammar
           'honeysql-postgres.ddl-grammar.data-type-test
           'honeysql-postgres.parser-test
           'honeysql-postgres.postgres-test)
