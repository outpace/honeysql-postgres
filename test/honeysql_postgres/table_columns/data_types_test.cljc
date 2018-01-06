(ns honeysql-postgres.table-columns.data-types-test
  (:require [clojure.test :refer [deftest is testing]]
            [honeysql-postgres.table-columns.data-types :as dt]))

(deftest test-parse-data-type--numeirc
  (testing "integer types"
    (is (= [{:type :int}  nil]
           (dt/parse-data-type [:int])))
    (is (= [{:type :integer}  nil]
           (dt/parse-data-type [:integer])))
    (is (= [{:type :smallint} nil]
           (dt/parse-data-type [:smallint])))
    (is (= [{:type :bigint} nil]
           (dt/parse-data-type [:bigint])))
    (is (= [{:type :int2} nil]
           (dt/parse-data-type [:int2])))
    (is (= [{:type :int4} nil]
           (dt/parse-data-type [:int4])))
    (is (= [{:type :int8} nil]
           (dt/parse-data-type [:int8]))))
  (testing "arbitrary precision number types"
    (testing "no precision or scale"
      (is (= [{:type :numeric} nil]
             (dt/parse-data-type [:numeric])))
      (is (= [{:type :decimal} nil]
             (dt/parse-data-type [:decimal]))))
    (testing "with precision"
      (is (= [{:type :numeric :precision 40} nil]
             (dt/parse-data-type [:numeric [40]])))
      (is (= [{:type :decimal :precision 40} nil]
             (dt/parse-data-type [:decimal [40]]))))
    (testing "with precision and scale"
      (is (= [{:type :numeric :precision 40 :scale 10} nil]
             (dt/parse-data-type [:numeric [40 10]])))
      (is (= [{:type :decimal :precision 40 :scale 10} nil]
             (dt/parse-data-type [:decimal [40 10]])))))
  (testing "floating point types"
    (testing "with no precision"
      (is (= [{:type :real} nil]
             (dt/parse-data-type [:real])))
      (is (= [{:type :double-precision} nil]
             (dt/parse-data-type [:double-precision])))
      (is (= [{:type :float8} nil]
             (dt/parse-data-type [:float8])))
      (is (= [{:type :float} nil]
             (dt/parse-data-type [:float]))))
    (testing "with precision"
      (is (= [{:type :float :precision 10} nil]
             (dt/parse-data-type [:float [10]])))))
  (testing "serial types"
    (is (= [{:type :serial} nil]
           (dt/parse-data-type [:serial])))
    (is (= [{:type :serial4} nil]
           (dt/parse-data-type [:serial4])))
    (is (= [{:type :bigserial} nil]
           (dt/parse-data-type [:bigserial])))
    (is (= [{:type :serial8} nil]
           (dt/parse-data-type [:serial8])))
    (is (= [{:type :smallserial} nil]
           (dt/parse-data-type [:smallserial])))
    (is (= [{:type :serial2} nil]
           (dt/parse-data-type [:serial2])))))

(deftest test-parse-data-type--character
  (is (= [{:type :text} nil]
         (dt/parse-data-type [:text])))
  (testing "with no lengths"
    (is (= [{:type :character-varying} nil]
           (dt/parse-data-type [:character-varying])))
    (is (= [{:type :varchar} nil]
           (dt/parse-data-type [:varchar])))
    (is (= [{:type :character} nil]
           (dt/parse-data-type [:character])))
    (is (= [{:type :char} nil]
           (dt/parse-data-type [:char]))))
  (testing "with lengths"
    (is (= [{:type :character-varying :length 20} nil]
           (dt/parse-data-type [:character-varying [20]])))
    (is (= [{:type :varchar :length 20} nil]
           (dt/parse-data-type [:varchar [20]])))
    (is (= [{:type :character :length 20} nil]
           (dt/parse-data-type [:character [20]])))
    (is (= [{:type :char :length 20} nil]
           (dt/parse-data-type [:char [20]])))))

(deftest test-parse-data-type--temporal
  (testing "timestamps"
    (testing "without time zones"
      (is (= [{:type :timestamp} nil]
             (dt/parse-data-type [:timestamp]))
          "plain TIMESTAMP")
      (is (= [{:type :timestamp :time-zone? false} nil]
             (dt/parse-data-type [:timestamp :without-time-zone]))
          "including :without-time-zone")
      (is (= [{:type :timestamp :time-zone? false} nil]
             (dt/parse-data-type [:timestamp-without-time-zone]))
          "compound keyword")
      (is (= [{:type :timestamp :precision 2} nil]
             (dt/parse-data-type [:timestamp [2]]))
          "with precision")
      (is (= [{:type :timestamp :precision 2 :time-zone? false} nil]
             (dt/parse-data-type [:timestamp [2] :without-time-zone]))
          "with precision and :without-time-zone"))
    (testing "with time zones"
      (is (= [{:type :timestamptz} nil]
             (dt/parse-data-type [:timestamptz]))
          "TIMESTAMPTZ (postgres extension)")
      (is (= [{:type :timestamp :time-zone? true} nil]
             (dt/parse-data-type [:timestamp-with-time-zone]))
          "compound keyword")
      (is (= [{:type :timestamp :precision 2 :time-zone? true} nil]
             (dt/parse-data-type [:timestamp [2] :with-time-zone]))
          "with precision")))
  (testing "date"
    (is (= [{:type :date} nil]
           (dt/parse-data-type [:date]))))
  (testing "times"
    (testing "without time zones"
      (is (= [{:type :time} nil]
             (dt/parse-data-type [:time]))
          "plain TIME")
      (is (= [{:type :time :time-zone? false} nil]
             (dt/parse-data-type [:time :without-time-zone]))
          "including :without-time-zone")
      (is (= [{:type :time :time-zone? false} nil]
             (dt/parse-data-type [:time-without-time-zone]))
          "compound keyword")
      (is (= [{:type :time :precision 2} nil]
             (dt/parse-data-type [:time [2]]))
          "with precision")
      (is (= [{:type :time :precision 2 :time-zone? false} nil]
             (dt/parse-data-type [:time [2] :without-time-zone]))
          "with precision and :without-time-zone"))
    (testing "with time zones"
      (is (= [{:type :timetz} nil]
             (dt/parse-data-type [:timetz]))
          "timetz keyword")
      (is (= [{:type :time :time-zone? true} nil]
             (dt/parse-data-type [:time-with-time-zone]))
          "compound keyword")
      (is (= [{:type :time :precision 2 :time-zone? true} nil]
             (dt/parse-data-type [:time [2] :with-time-zone]))
          "with precision")))
  (testing "intervals"
    (is (= [{:type :interval} nil]
           (dt/parse-data-type [:interval]))
        "only interval")
    (is (= [{:type :interval :precision 4} nil]
           (dt/parse-data-type [:interval [4]]))
        "with precision")
    (testing "with field"
      (testing "year"
        (is (= [{:type :interval :field :year} nil]
               (dt/parse-data-type [:interval :year]))
            "without precision")
        (is (= [{:type :interval :field :year :precision 2} nil]
               (dt/parse-data-type [:interval :year [2]]))
            "with precision"))
      (testing "month"
        (is (= [{:type :interval :field :month} nil]
               (dt/parse-data-type [:interval :month]))
            "without precision")
        (is (= [{:type :interval :field :month :precision 2} nil]
               (dt/parse-data-type [:interval :month [2]]))
            "with precision"))
      (testing "day"
        (is (= [{:type :interval :field :day} nil]
               (dt/parse-data-type [:interval :day]))
            "without precision")
        (is (= [{:type :interval :field :day :precision 2} nil]
               (dt/parse-data-type [:interval :day [2]]))
            "with precision"))
      (testing "hour"
        (is (= [{:type :interval :field :hour} nil]
               (dt/parse-data-type [:interval :hour]))
            "without precision")
        (is (= [{:type :interval :field :hour :precision 2} nil]
               (dt/parse-data-type [:interval :hour [2]]))
            "with precision"))
      (testing "minute"
        (is (= [{:type :interval :field :minute} nil]
               (dt/parse-data-type [:interval :minute]))
            "without precision")
        (is (= [{:type :interval :field :minute :precision 2} nil]
               (dt/parse-data-type [:interval :minute [2]]))
            "with precision"))
      (testing "second"
        (is (= [{:type :interval :field :second} nil]
               (dt/parse-data-type [:interval :second]))
            "without precision")
        (is (= [{:type :interval :field :second :precision 2} nil]
               (dt/parse-data-type [:interval :second [2]]))
            "with precision"))
      (testing "year-to-month"
        (is (= [{:type :interval :field :year-to-month} nil]
               (dt/parse-data-type [:interval :year-to-month]))
            "without precision")
        (is (= [{:type :interval :field :year-to-month :precision 2} nil]
               (dt/parse-data-type [:interval :year-to-month [2]]))
            "with precision"))
      (testing "day-to-hour"
        (is (= [{:type :interval :field :day-to-hour} nil]
               (dt/parse-data-type [:interval :day-to-hour]))
            "without precision")
        (is (= [{:type :interval :field :day-to-hour :precision 2} nil]
               (dt/parse-data-type [:interval :day-to-hour [2]]))
            "with precision"))
      (testing "day-to-minute"
        (is (= [{:type :interval :field :day-to-minute} nil]
               (dt/parse-data-type [:interval :day-to-minute]))
            "without precision")
        (is (= [{:type :interval :field :day-to-minute :precision 2} nil]
               (dt/parse-data-type [:interval :day-to-minute [2]]))
            "with precision"))
      (testing "day-to-second"
        (is (= [{:type :interval :field :day-to-second} nil]
               (dt/parse-data-type [:interval :day-to-second]))
            "without precision")
        (is (= [{:type :interval :field :day-to-second :precision 2} nil]
               (dt/parse-data-type [:interval :day-to-second [2]]))
            "with precision"))
      (testing "hour-to-minute"
        (is (= [{:type :interval :field :hour-to-minute} nil]
               (dt/parse-data-type [:interval :hour-to-minute]))
            "without precision")
        (is (= [{:type :interval :field :hour-to-minute :precision 2} nil]
               (dt/parse-data-type [:interval :hour-to-minute [2]]))
            "with precision"))
      (testing "hour-to-second"
        (is (= [{:type :interval :field :hour-to-second} nil]
               (dt/parse-data-type [:interval :hour-to-second]))
            "without precision")
        (is (= [{:type :interval :field :hour-to-second :precision 2} nil]
               (dt/parse-data-type [:interval :hour-to-second [2]]))
            "with precision"))
      (testing "minute-to-second"
        (is (= [{:type :interval :field :minute-to-second} nil]
               (dt/parse-data-type [:interval :minute-to-second]))
            "without precision")
        (is (= [{:type :interval :field :minute-to-second :precision 2} nil]
               (dt/parse-data-type [:interval :minute-to-second [2]]))
            "with precision")))))

(deftest test-parse-data-type--misc
  (testing "money type"
    (is (= [{:type :money} nil]
           (dt/parse-data-type [:money]))))
  (testing "binary types"
    (is (= [{:type :bytea} nil]
           (dt/parse-data-type [:bytea])))
    (is (= [{:type :blob} nil]
           (dt/parse-data-type [:blob])))
    (is (= [{:type :binary-large-object} nil]
           (dt/parse-data-type [:binary-large-object]))))
  (testing "boolean type"
    (is (= [{:type :bool} nil]
           (dt/parse-data-type [:bool])))
    (is (= [{:type :boolean} nil]
           (dt/parse-data-type [:boolean]))))
  (testing "geometric type"
    (is (= [{:type :point} nil]
           (dt/parse-data-type [:point])))
    (is (= [{:type :line} nil]
           (dt/parse-data-type [:line])))
    (is (= [{:type :lseg} nil]
           (dt/parse-data-type [:lseg])))
    (is (= [{:type :box} nil]
           (dt/parse-data-type [:box])))
    (is (= [{:type :path} nil]
           (dt/parse-data-type [:path])))
    (is (= [{:type :polygon} nil]
           (dt/parse-data-type [:polygon])))
    (is (= [{:type :circle} nil]
           (dt/parse-data-type [:circle]))))
  (testing "geometric type"
    (is (= [{:type :cidr} nil]
           (dt/parse-data-type [:cidr])))
    (is (= [{:type :inet} nil]
           (dt/parse-data-type [:inet])))
    (is (= [{:type :macaddr} nil]
           (dt/parse-data-type [:macaddr])))
    (is (= [{:type :macaddr8} nil]
           (dt/parse-data-type [:macaddr8]))))
  (testing "bit string types"
    (testing "with no length"
      (is (= [{:type :varbit} nil]
             (dt/parse-data-type [:varbit])))
      (is (= [{:type :bit-varying} nil]
             (dt/parse-data-type [:bit-varying])))
      (is (= [{:type :bit} nil]
             (dt/parse-data-type [:bit]))))
    (testing "with length defined"
      (is (= [{:type :varbit :length 20} nil]
             (dt/parse-data-type [:varbit [20]])))
      (is (= [{:type :bit-varying :length 20} nil]
             (dt/parse-data-type [:bit-varying [20]])))
      (is (= [{:type :bit :length 20} nil]
             (dt/parse-data-type [:bit [20]])))))
  (testing "text search type"
    (is (= [{:type :tsvector} nil]
           (dt/parse-data-type [:tsvector])))
    (is (= [{:type :tsquery} nil]
           (dt/parse-data-type [:tsquery]))))
  (testing "UUID type"
    (is (= [{:type :uuid} nil]
           (dt/parse-data-type [:uuid]))))
  (testing "XML type"
    (is (= [{:type :xml} nil]
           (dt/parse-data-type [:xml]))))
  (testing "JSON types"
    (is (= [{:type :json} nil]
           (dt/parse-data-type [:json])))
    (is (= [{:type :jsonb} nil]
           (dt/parse-data-type [:jsonb]))))
  (testing "postgres types"
    (is (= [{:type :pg_lsn} nil]
           (dt/parse-data-type [:pg_lsn])))
    (is (= [{:type :txid_snapshot} nil]
           (dt/parse-data-type [:txid_snapshot])))))

(deftest test-parse-data-type--arrays
  (testing "arrays of simple types"
    (testing "one dimension"
      (is (= [{:type :array :of {:type :integer}} nil]
             (dt/parse-data-type [:integer []]))
          "without length")
      (is (= [{:type :array :of {:type :integer} :length 3} nil]
             (dt/parse-data-type [:integer [3]]))
          "with length"))
    (testing "multiple dimensions"
      (is (= [{:type :array :of {:type :array :of {:type :integer}}} nil]
             (dt/parse-data-type [:integer [] []]))
          "without lengths")
      (is (= [{:type :array
               :of {:type :array
                    :of {:type :array
                         :of {:type :bytea}
                         :length 3}
                    :length 2}
               :length 1}
              nil]
             (dt/parse-data-type [:bytea [3] [2] [1]]))
          "with lengths")
      (is (= [{:type :array
               :of {:type :array
                    :of {:type :array
                         :of {:type :array
                              :of {:type :json}}
                         :length 3}}
               :length 1}
              nil]
             (dt/parse-data-type [:json [] [3] [] [1]]))
          "with and without lengths")))
  (testing "arrays of possibly ambiguous types"
    (testing "one dimension"
      (is (= [{:type :array :of {:type :varchar :length 24}} nil]
             (dt/parse-data-type [:varchar [24] []]))
          "without length")
      (is (= [{:type :array :of {:type :decimal :precision 4} :length 3} nil]
             (dt/parse-data-type [:decimal [4] [3]]))
          "with length"))
    (testing "multiple dimensions"
      (is (= [{:type :array :of {:type :array :of {:type :time :precision 3}}} nil]
             (dt/parse-data-type [:time [3] [] []]))
          "without lengths")
      (is (= [{:type :array
               :of {:type :array
                    :of {:type :array
                         :of {:type :varbit
                              :length 10}
                         :length 3}
                    :length 2}
               :length 1}
              nil]
             (dt/parse-data-type [:varbit [10] [3] [2] [1]]))
          "with lengths")
      (is (= [{:type :array
               :of {:type :array
                    :of {:type :array
                         :of {:type :array
                              :of {:type :interval
                                   :field :month
                                   :precision 5}}
                         :length 3}}
               :length 1}
              nil]
             (dt/parse-data-type [:interval :month [5] [] [3] [] [1]]))
          "with and without lengths"))))
