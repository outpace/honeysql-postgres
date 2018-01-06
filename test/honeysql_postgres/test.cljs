(ns honeysql-postgres.test
  (:require [doo.runner :refer-macros [doo-tests]]
            [honeysql-postgres.postgres-test]
            [honeysql-postgres.table-columns.data-types-test]))

(doo-tests 'honeysql-postgres.postgres-test
           'honeysql-postgres.table-columns.data-types-test)
