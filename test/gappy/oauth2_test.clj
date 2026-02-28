(ns gappy.oauth2-test
  (:require [clojure.test :refer [deftest is testing]]
            [gappy.oauth2 :as oauth2]
            [clojure.string :as s]))

(deftest base64url-encode-test
  (testing "encodes bytes to base64url without padding"
    (let [result (oauth2/base64url-encode (.getBytes "test"))]
      (is (string? result))
      (is (not (s/includes? result "=")))
      (is (not (s/includes? result "+"))))))

(deftest generate-state-test
  (testing "generates a non-empty string"
    (let [state (oauth2/generate-state)]
      (is (string? state))
      (is (> (count state) 0))))

  (testing "generates unique values"
    (let [s1 (oauth2/generate-state)
          s2 (oauth2/generate-state)]
      (is (not= s1 s2)))))

(deftest generate-code-verifier-test
  (testing "generates a non-empty string"
    (let [verifier (oauth2/generate-code-verifier)]
      (is (string? verifier))
      (is (> (count verifier) 0))))

  (testing "respects custom size"
    (let [small (oauth2/generate-code-verifier 16)
          large (oauth2/generate-code-verifier 64)]
      (is (< (count small) (count large))))))

(deftest pkce-challenge-plain-test
  (testing "returns input unchanged"
    (is (= "test-verifier" (oauth2/pkce-challenge-plain "test-verifier")))))

(deftest pkce-challenge-s256-test
  (testing "produces a non-empty string different from input"
    (let [verifier "test-verifier"
          challenge (oauth2/pkce-challenge-s256 verifier)]
      (is (string? challenge))
      (is (> (count challenge) 0))
      (is (not= verifier challenge))))

  (testing "is deterministic"
    (let [verifier "test-verifier"
          c1 (oauth2/pkce-challenge-s256 verifier)
          c2 (oauth2/pkce-challenge-s256 verifier)]
      (is (= c1 c2)))))

(deftest build-authn-url-test
  (testing "builds valid OAuth2 URL with required params"
    (let [url (oauth2/build-authn-url
               {:auth_uri "https://accounts.google.com/o/oauth2/v2/auth"
                :client_id "test-client-id"
                :scope ["https://www.googleapis.com/auth/admin.directory.user"]
                :redirect_uri "http://localhost:3000/"})]
      (is (s/starts-with? url "https://accounts.google.com/o/oauth2/v2/auth?"))
      (is (s/includes? url "client_id=test-client-id"))
      (is (s/includes? url "response_type=code"))
      (is (s/includes? url "redirect_uri="))))

  (testing "includes PKCE parameters when provided"
    (let [url (oauth2/build-authn-url
               {:auth_uri "https://accounts.google.com/o/oauth2/v2/auth"
                :client_id "test-client-id"
                :scope ["email"]
                :code_challenge "test-challenge"
                :code_challenge_method "S256"})]
      (is (s/includes? url "code_challenge=test-challenge"))
      (is (s/includes? url "code_challenge_method=S256")))))
