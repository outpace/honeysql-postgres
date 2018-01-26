(ns honeysql-postgres.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [honeysql-postgres.parser :as p]
            [honeysql-postgres.parser.protocol :as proto]))

(deftest test-one-of
  (let [parser (p/one-of :foo :bar)]
    (testing "parsing"
      (is (= [:foo nil] (proto/parse parser [:foo]))
          "basic parse")
      (is (= [:bar nil] (proto/parse parser [:bar]))
          "basic parse")
      (is (= [:foo [:foo :bar]] (proto/parse parser [:foo :foo :bar]))
          "leftover tokens")
      (is (= ::p/no-match (proto/parse parser []))
          "no input tokens")
      (is (= ::p/no-match (proto/parse parser [:baz :foo :foo :bar]))
          "failed parse"))
    (is (= "( :foo | :bar )" (str parser))
        "text representation is correct")))

(deftest test-=
  (let [parser (p/lit :foo)]
    (testing "parsing"
      (is (= [:foo nil] (proto/parse parser [:foo]))
          "basic parse")
      (is (= [:foo [:foo :bar]] (proto/parse parser [:foo :foo :bar]))
          "leftover tokens")
      (is (= ::p/no-match (proto/parse parser []))
          "no input tokens")
      (is (= ::p/no-match (proto/parse parser [:baz :foo :foo :bar]))
          "failed parse"))
    (is (= ":foo" (str parser))
        "text representation is correct")))

(deftest test-|
  (is (thrown? #?(:clj AssertionError :cljs js/Error) (p/|))
      "no parsers is not allowed")
  (testing "single parser"
    (let [parser (p/| (p/lit :a))]
      (testing "parsing"
        (is (= [:a nil] (proto/parse parser [:a]))
            "parsed")
        (is (= [:a [:a]] (proto/parse parser [:a :a]))
            "parsed with extra input")
        (is (= ::p/no-match (proto/parse parser [:foo]))
            "invalid token")
        (is (= ::p/no-match (proto/parse parser []))
            "no input tokens"))
      (is (= "( :a )" (str parser))
          "text representation is correct")))
  (testing "multiple parsers"
    (let [parser (p/| (p/lit :a)
                      (p/lit :b)
                      (p/lit :c))]
      (testing "parsing"
        (is (= [:a nil] (proto/parse parser [:a]))
            "parsed")
        (is (= [:b nil] (proto/parse parser [:b]))
            "parsed")
        (is (= [:c nil] (proto/parse parser [:c]))
            "parsed")
        (is (= [:c [:a]] (proto/parse parser [:c :a]))
            "parsed with extra input")
        (is (= ::p/no-match (proto/parse parser [:foo]))
            "invalid token")
        (is (= ::p/no-match (proto/parse parser []))
            "no input tokens"))
      (is (= "( :a | :b | :c )" (str parser))
          "text representation is correct"))))

(deftest test-?
  (testing "with default no value"
    (let [parser (p/? (p/lit :a))]
      (testing "parsing"
        (is (= [nil []]
               (proto/parse parser []))
            "empty input")
        (is (= [:a nil]
               (proto/parse parser [:a]))
            "matching input")
        (is (= [:a [:a]]
               (proto/parse parser [:a :a]))
            "matching input with extra tokens")
        (is (= [nil [:b :a]]
               (proto/parse parser [:b :a]))
            "non-matching input with extra tokens"))
      (is (= ":a ?" (str parser))
          "text representation")))
  (testing "with given no value"
    (let [parser (p/? (p/lit :a) ::nothing)]
      (testing "parsing"
        (is (= [::nothing []]
               (proto/parse parser []))
            "empty input")
        (is (= [:a nil]
               (proto/parse parser [:a]))
            "matching input")
        (is (= [:a [:a]]
               (proto/parse parser [:a :a]))
            "matching input with extra tokens")
        (is (= [::nothing [:b :a]]
               (proto/parse parser [:b :a]))
            "non-matching input with extra tokens"))
      (is (= ":a ?" (str parser))
          "text representation"))))

(deftest test-seq
  (testing "empty sequence"
    (let [parser (p/seq)]
      (testing "parsing"
        (is (= [[] []]
               (proto/parse parser []))
            "empty input")
        (is (= [[] [:a :b :c]]
               (proto/parse parser [:a :b :c]))
            "non-empty input"))
      (is (= "" (str parser))
          "text representation")))
  (testing "one item sequence"
    (let [parser (p/seq (p/lit :a))]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser []))
            "empty input")
        (is (= [[:a] nil]
               (proto/parse parser [:a]))
            "matching input")
        (is (= [[:a] [:b :c]]
               (proto/parse parser [:a :b :c]))
            "matching input with extra stuff")
        (is (= ::p/no-match (proto/parse parser [:b :a]))
            "non-matching input"))
      (is (= ":a" (str parser))
          "text representation")))
  (testing "multiple item sequence"
    (let [parser (p/seq (p/lit :a)
                        (p/lit :b)
                        (p/lit :c))]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser []))
            "empty input")
        (is (= [[:a :b :c] nil]
               (proto/parse parser [:a :b :c]))
            "matching input")
        (is (= [[:a :b :c] [:b :c]]
               (proto/parse parser [:a :b :c :b :c]))
            "matching input with extra stuff")
        (is (= ::p/no-match (proto/parse parser [:a :b]))
            "incomplete input")
        (is (= ::p/no-match (proto/parse parser [:b :a]))
            "non-matching input"))
      (is (= ":a :b :c" (str parser))
          "text representation"))))

(deftest test-pred
  (testing "without a text represtation"
    (let [parser (p/pred integer?)]
      (testing "parsing"
        (is (= [1 nil]
               (proto/parse parser [1]))
            "basic parse")
        (is (= [1 [:foo :bar]]
               (proto/parse parser [1 :foo :bar]))
            "leftover tokens")
        (is (= ::p/no-match
               (proto/parse parser []))
            "no input tokens")
        (is (= ::p/no-match
               (proto/parse parser [:baz :foo :foo :bar]))
            "failed parse"))
      (is (= "integer?" (str parser))
          "text representation is correct")))
  (testing "with a text representation"
    (let [parser (p/pred #(and (integer? %) (pos? %)) "pos-int?")]
      (testing "parsing"
        (is (= [1 nil] (proto/parse parser [1]))
            "basic parse")
        (is (= [1 [:foo :bar]] (proto/parse parser [1 :foo :bar]))
            "leftover tokens")
        (is (= ::p/no-match (proto/parse parser []))
            "no input tokens")
        (is (= ::p/no-match (proto/parse parser [:baz :foo :foo :bar]))
            "failed parse"))
      (is (= "pos-int?" (str parser))
          "text representation is correct"))))

(deftest test-vector
  (testing "empty vector"
    (let [parser (p/vector)]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser [:foo]))
            "non-matching input, not a vector")
        (is (= ::p/no-match (proto/parse parser [[:foo]]))
            "non-matching input, contents do not match")
        (is (= [[] nil] (proto/parse parser [[]]))
            "matching input")
        (is (= [[] [:foo]] (proto/parse parser [[] :foo]))
            "matching input with extra tokens"))
      (is (= "[]" (str parser))
          "text representation")))
  (testing "non-empty vector"
    (let [parser (p/vector (p/lit :a) (p/lit :b))]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser [:foo]))
            "non-matching input, not a vector")
        (is (= ::p/no-match (proto/parse parser [[:a]]))
            "non-matching input, contents do not match")
        (is (= [[:a :b] nil] (proto/parse parser [[:a :b]]))
            "matching input")
        (is (= [[:a :b] [:foo]] (proto/parse parser [[:a :b] :foo]))
            "matching input with extra tokens"))
      (is (= "[ :a :b ]" (str parser))
          "text representation"))))

(deftest test-map
  (let [parser (p/map dec (p/lit 42))]
    (is (= ::p/no-match (proto/parse parser []))
        "no input")
    (is (= (str (p/lit 42)) (str parser))
        "Text representation not altered")))

(deftest test-*
  (let [parser (p/* (p/lit :a))]
    (testing "parsing"
      (is (= [[] []]
             (proto/parse parser []))
          "empty input")
      (is (= [[:a] nil]
             (proto/parse parser [:a]))
          "matching input")
      (is (= [[:a :a] nil]
             (proto/parse parser [:a :a]))
          "matching input")
      (is (= [[:a :a :a] nil]
             (proto/parse parser [:a :a :a]))
          "matching input")
      (is (= [[] [:b :a]]
             (proto/parse parser [:b :a]))
          "non-matching input with extra tokens"))
    (is (= ":a *" (str parser))
        "text representation")))

(deftest test-+
  (let [parser (p/+ (p/lit :a))]
    (testing "parsing"
      (is (= ::p/no-match (proto/parse parser []))
          "empty input")
      (is (= [[:a] nil]
             (proto/parse parser [:a]))
          "matching input")
      (is (= [[:a :a] nil]
             (proto/parse parser [:a :a]))
          "matching input")
      (is (= [[:a :a :a] nil]
             (proto/parse parser [:a :a :a]))
          "matching input")
      (is (= ::p/no-match
             (proto/parse parser [:b :a]))
          "non-matching input with extra tokens"))
    (is (= ":a +" (str parser))
        "text representation")))

(deftest test-ε
  (testing "ε"
    (let [parser p/ε]
      (testing "parsing"
        (is (= [nil nil] (proto/parse parser []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser [:a]))
            "non-matching input"))
      (is (= "ε" (str parser)))))
  (testing "eps"
    (let [parser p/eps]
      (testing "parsing"
        (is (= [nil nil] (proto/parse parser []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser [:a]))
            "non-matching input"))
      (is (= "ε" (str parser))))))
