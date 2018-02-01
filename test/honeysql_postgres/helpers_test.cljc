(ns honeysql-postgres.helpers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test #?(:clj :refer :cljs :refer-macros) [defspec]]
            [clojure.test.check.properties :as prop]
            [honeysql-postgres.generators :as gen]
            [honeysql-postgres.parser :as p]
            [honeysql-postgres.ddl-grammar.data-type :as dt]
            [honeysql-postgres.helpers :as pg-sqlh]))

(defspec test-type
  200
  (prop/for-all [args gen/type-args]
    (try
      (= (apply pg-sqlh/type args)
         (p/parse dt/type args))
      (catch IllegalArgumentException _
        false))))
