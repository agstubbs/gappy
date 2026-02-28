(ns gappy.util-test
  (:require [clojure.test :refer [deftest is testing]]
            [gappy.util :as util]))

(deftest build-query-str-test
  (testing "returns nil for empty map"
    (is (nil? (util/build-query-str {}))))

  (testing "builds single key-value pair"
    (is (= "a=1" (util/build-query-str {:a "1"}))))

  (testing "URL-encodes special characters in values"
    (let [result (util/build-query-str {:q "hello world"})]
      (is (= "q=hello+world" result))))

  (testing "prepend option adds ? prefix"
    (let [result (util/build-query-str {:a "1"} :prepend true)]
      (is (= "?a=1" result))))

  (testing "prepend option with empty map returns nil"
    (is (nil? (util/build-query-str {} :prepend true)))))

(deftest split-first-test
  (testing "splits at first occurrence of boundary"
    (is (= '("hello" "world") (util/split-first "hello--world" "--"))))

  (testing "returns nil when boundary not found"
    (is (nil? (util/split-first "hello" "--"))))

  (testing "returns nil for nil input"
    (is (nil? (util/split-first nil "--")))))

(deftest split-multi-test
  (testing "splits into multiple parts"
    (is (= '("a" "b" "c") (util/split-multi "a--b--c" "--"))))

  (testing "single part with no boundary"
    (is (= '("abc") (util/split-multi "abc" "--")))))

(deftest headers-test
  (testing "parses header string into keyword map"
    (let [result (util/headers "Content-Type: application/json\r\nAccept: text/html")]
      (is (= "application/json" (:content-type result)))
      (is (= "text/html" (:accept result))))))

(deftest header-body-test
  (testing "splits headers from body"
    (let [result (util/header-body "Content-Type: application/json\r\n\r\n{\"ok\":true}")]
      (is (= "application/json" (-> result :headers :content-type)))
      (is (= "{\"ok\":true}" (:body result)))))

  (testing "body-only when no header separator"
    (let [result (util/header-body "just a body")]
      (is (= "just a body" (:body result))))))

(deftest get-status-line-test
  (testing "parses HTTP status line"
    (let [result (util/get-status-line "HTTP/1.1 200 OK")]
      (is (= 200 (:status result)))
      (is (= "OK" (:reason-phrase result)))
      (is (= "HTTP" (-> result :protocol-version :name)))
      (is (= 1 (-> result :protocol-version :major)))
      (is (= 1 (-> result :protocol-version :minor))))))
