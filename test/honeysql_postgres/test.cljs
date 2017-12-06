(ns honeysql-postgres.test
  (:require [doo.runner :refer-macros [doo-tests]]
            [honeysql-postgres.postgres-test]))

(doo-tests 'honeysql-postgres.postgres-test)
