(ns honeysql-postgres.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [honeysql-postgres.parser :as p]
            [honeysql-postgres.parser.protocol :as proto]))

(deftest test-one-of
  (let [parser (p/one-of :foo :bar)]
    (testing "parsing"
      (is (= [:foo :state nil] (proto/parse parser :state [:foo]))
          "basic parse")
      (is (= [:bar :state nil] (proto/parse parser :state [:bar]))
          "basic parse")
      (is (= [:foo :state [:foo :bar]] (proto/parse parser :state [:foo :foo :bar]))
          "leftover tokens")
      (is (= ::p/no-match (proto/parse parser :state []))
          "no input tokens")
      (is (= ::p/no-match (proto/parse parser :state [:baz :foo :foo :bar]))
          "failed parse"))
    (is (= "( :foo | :bar )" (str parser))
        "text representation is correct")))

(deftest test-=
  (let [parser (p/lit :foo)]
    (testing "parsing"
      (is (= [:foo :state nil] (proto/parse parser :state [:foo]))
          "basic parse")
      (is (= [:foo :state [:foo :bar]] (proto/parse parser :state [:foo :foo :bar]))
          "leftover tokens")
      (is (= ::p/no-match (proto/parse parser :state []))
          "no input tokens")
      (is (= ::p/no-match (proto/parse parser :state [:baz :foo :foo :bar]))
          "failed parse"))
    (is (= ":foo" (str parser))
        "text representation is correct")))

(deftest test-|
  (is (thrown? #?(:clj AssertionError :cljs js/Error) (p/|))
      "no parsers is not allowed")
  (testing "single parser"
    (let [parser (p/| (p/lit :a))]
      (testing "parsing"
        (is (= [:a :state nil] (proto/parse parser :state [:a]))
            "parsed")
        (is (= [:a :state [:a]] (proto/parse parser :state [:a :a]))
            "parsed with extra input")
        (is (= ::p/no-match (proto/parse parser :state [:foo]))
            "invalid token")
        (is (= ::p/no-match (proto/parse parser :state []))
            "no input tokens"))
      (is (= "( :a )" (str parser))
          "text representation is correct")))
  (testing "multiple parsers"
    (let [parser (p/| (p/lit :a)
                      (p/lit :b)
                      (p/lit :c))]
      (testing "parsing"
        (is (= [:a :state nil] (proto/parse parser :state [:a]))
            "parsed")
        (is (= [:b :state nil] (proto/parse parser :state [:b]))
            "parsed")
        (is (= [:c :state nil] (proto/parse parser :state [:c]))
            "parsed")
        (is (= [:c :state [:a]] (proto/parse parser :state [:c :a]))
            "parsed with extra input")
        (is (= ::p/no-match (proto/parse parser :state [:foo]))
            "invalid token")
        (is (= ::p/no-match (proto/parse parser :state []))
            "no input tokens"))
      (is (= "( :a | :b | :c )" (str parser))
          "text representation is correct"))))

(deftest test-?
  (testing "with default no value"
    (let [parser (p/? (p/lit :a))]
      (testing "parsing"
        (is (= [nil :state []]
               (proto/parse parser :state []))
            "empty input")
        (is (= [:a :state nil]
               (proto/parse parser :state [:a]))
            "matching input")
        (is (= [:a :state [:a]]
               (proto/parse parser :state [:a :a]))
            "matching input with extra tokens")
        (is (= [nil :state [:b :a]]
               (proto/parse parser :state [:b :a]))
            "non-matching input with extra tokens"))
      (is (= ":a ?" (str parser))
          "text representation")))
  (testing "with given no value"
    (let [parser (p/? (p/lit :a) ::nothing)]
      (testing "parsing"
        (is (= [::nothing :state []]
               (proto/parse parser :state []))
            "empty input")
        (is (= [:a :state nil]
               (proto/parse parser :state [:a]))
            "matching input")
        (is (= [:a :state [:a]]
               (proto/parse parser :state [:a :a]))
            "matching input with extra tokens")
        (is (= [::nothing :state [:b :a]]
               (proto/parse parser :state [:b :a]))
            "non-matching input with extra tokens"))
      (is (= ":a ?" (str parser))
          "text representation"))))

(deftest test-cat
  (testing "empty sequence"
    (let [parser (p/seq)]
      (testing "parsing"
        (is (= [[] :state []]
               (proto/parse parser :state []))
            "empty input")
        (is (= [[] :state [:a :b :c]]
               (proto/parse parser :state [:a :b :c]))
            "non-empty input"))
      (is (= "" (str parser))
          "text representation")))
  (testing "one item sequence"
    (let [parser (p/seq (p/lit :a))]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser :state []))
            "empty input")
        (is (= [[:a] :state nil]
               (proto/parse parser :state [:a]))
            "matching input")
        (is (= [[:a] :state [:b :c]]
               (proto/parse parser :state [:a :b :c]))
            "matching input with extra stuff")
        (is (= ::p/no-match (proto/parse parser :state [:b :a]))
            "non-matching input"))
      (is (= ":a" (str parser))
          "text representation")))
  (testing "multiple item sequence"
    (let [parser (p/seq (p/lit :a)
                        (p/lit :b)
                        (p/lit :c))]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser :state []))
            "empty input")
        (is (= [[:a :b :c] :state nil]
               (proto/parse parser :state [:a :b :c]))
            "matching input")
        (is (= [[:a :b :c] :state [:b :c]]
               (proto/parse parser :state [:a :b :c :b :c]))
            "matching input with extra stuff")
        (is (= ::p/no-match (proto/parse parser :state [:a :b]))
            "incomplete input")
        (is (= ::p/no-match (proto/parse parser :state [:b :a]))
            "non-matching input"))
      (is (= ":a :b :c" (str parser))
          "text representation"))))

(deftest test-pred
  (testing "without a text represtation"
    (let [parser (p/pred integer?)]
      (testing "parsing"
        (is (= [1 :state nil]
               (proto/parse parser :state [1]))
            "basic parse")
        (is (= [1 :state [:foo :bar]]
               (proto/parse parser :state [1 :foo :bar]))
            "leftover tokens")
        (is (= ::p/no-match
               (proto/parse parser :state []))
            "no input tokens")
        (is (= ::p/no-match
               (proto/parse parser :state [:baz :foo :foo :bar]))
            "failed parse"))
      (is (= "integer?" (str parser))
          "text representation is correct")))
  (testing "with a text representation"
    (let [parser (p/pred #(and (integer? %) (pos? %)) "pos-int?")]
      (testing "parsing"
        (is (= [1 :state nil] (proto/parse parser :state [1]))
            "basic parse")
        (is (= [1 :state [:foo :bar]] (proto/parse parser :state [1 :foo :bar]))
            "leftover tokens")
        (is (= ::p/no-match (proto/parse parser :state []))
            "no input tokens")
        (is (= ::p/no-match (proto/parse parser :state [:baz :foo :foo :bar]))
            "failed parse"))
      (is (= "pos-int?" (str parser))
          "text representation is correct"))))

(deftest test-vector
  (testing "empty vector"
    (let [parser (p/vector)]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser :state []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser :state [:foo]))
            "non-matching input, not a vector")
        (is (= ::p/no-match (proto/parse parser :state [[:foo]]))
            "non-matching input, contents do not match")
        (is (= [[] :state nil] (proto/parse parser :state [[]]))
            "matching input")
        (is (= [[] :state [:foo]] (proto/parse parser :state [[] :foo]))
            "matching input with extra tokens"))
      (is (= "[]" (str parser))
          "text representation")))
  (testing "non-empty vector"
    (let [parser (p/vector (p/lit :a) (p/lit :b))]
      (testing "parsing"
        (is (= ::p/no-match (proto/parse parser :state []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser :state [:foo]))
            "non-matching input, not a vector")
        (is (= ::p/no-match (proto/parse parser :state [[:a]]))
            "non-matching input, contents do not match")
        (is (= [[:a :b] :state nil] (proto/parse parser :state [[:a :b]]))
            "matching input")
        (is (= [[:a :b] :state [:foo]] (proto/parse parser :state [[:a :b] :foo]))
            "matching input with extra tokens"))
      (is (= "[ :a :b ]" (str parser))
          "text representation"))))

(deftest test-map
  (let [parser (p/map #(vector (inc %1) (dec %2))
                      (p/lit 42))]
    (is (= ::p/no-match (proto/parse parser :state []))
        "no input")
    (is (= [43 41 nil] (proto/parse parser 42 [42]))
        "mapped state/value")
    (is (= (str (p/lit 42)) (str parser))
        "Text representation not altered")))

(deftest test-map-state
  (let [parser (p/map-state dec (p/lit 42))]
    (is (= ::p/no-match (proto/parse parser :state []))
        "no input")
    (is (= [42 41 nil] (proto/parse parser 42 [42]))
        "mapped state")
    (is (= (str (p/lit 42)) (str parser))
        "Text representation not altered")))

(deftest test-map-result
  (let [parser (p/map-result dec (p/lit 42))]
    (is (= ::p/no-match (proto/parse parser :state []))
        "no input")
    (is (= [41 42 nil] (proto/parse parser 42 [42]))
        "mapped result")
    (is (= (str (p/lit 42)) (str parser))
        "Text representation not altered")))

(deftest test-*
  (let [parser (p/* (p/lit :a))]
    (testing "parsing"
      (is (= [[] :state []]
             (proto/parse parser :state []))
          "empty input")
      (is (= [[:a] :state nil]
             (proto/parse parser :state [:a]))
          "matching input")
      (is (= [[:a :a] :state nil]
             (proto/parse parser :state [:a :a]))
          "matching input")
      (is (= [[:a :a :a] :state nil]
             (proto/parse parser :state [:a :a :a]))
          "matching input")
      (is (= [[] :state [:b :a]]
             (proto/parse parser :state [:b :a]))
          "non-matching input with extra tokens"))
    (is (= ":a *" (str parser))
        "text representation")))

(deftest test-+
  (let [parser (p/+ (p/lit :a))]
    (testing "parsing"
      (is (= ::p/no-match (proto/parse parser :state []))
          "empty input")
      (is (= [[:a] :state nil]
             (proto/parse parser :state [:a]))
          "matching input")
      (is (= [[:a :a] :state nil]
             (proto/parse parser :state [:a :a]))
          "matching input")
      (is (= [[:a :a :a] :state nil]
             (proto/parse parser :state [:a :a :a]))
          "matching input")
      (is (= ::p/no-match
             (proto/parse parser :state [:b :a]))
          "non-matching input with extra tokens"))
    (is (= ":a +" (str parser))
        "text representation")))

(deftest test-ε
  (testing "ε"
    (let [parser p/ε]
      (testing "parsing"
        (is (= [nil :state nil] (proto/parse parser :state []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser :state [:a]))
            "non-matching input"))
      (is (= "ε" (str parser)))))
  (testing "eps"
    (let [parser p/eps]
      (testing "parsing"
        (is (= [nil :state nil] (proto/parse parser :state []))
            "empty input")
        (is (= ::p/no-match (proto/parse parser :state [:a]))
            "non-matching input"))
      (is (= "ε" (str parser))))))
