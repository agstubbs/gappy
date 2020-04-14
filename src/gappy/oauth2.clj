(ns gappy.oauth2
  (:require [clojure.string :as s])
  (:import [java.security SecureRandom MessageDigest]
           java.util.Base64
           java.net.URLEncoder))

;; cross-platform note: ring.util.codec/url-encode could replace java.net.URLEncoder

(defn make-random-buffer [size]
  (let [result (byte-array size)]
    (.nextBytes (SecureRandom.) result)
    result))

;; base64url-encoding
;; https://tools.ietf.org/html/rfc7636#appendix-A
;; (based on https://tools.ietf.org/html/rfc4648#page-7 )
(defn base64url-encode [buffer]
  (apply str
         (map
          char
          (.encode (.withoutPadding (Base64/getUrlEncoder)) buffer))))

(defn build-authn-url [{:keys
                      [auth_uri scope response_type redirect_uri]
                      :as authn-data
                      :or {response_type "code"
                           redirect_uri "urn:ietf:wg:oauth:2.0:oob"}}]
  (let [scope-str (s/join " " scope)
        params (conj (select-keys authn-data [:client_id
                                             :response_type
                                             :code_challenge
                                             :code_challenge_method
                                             :state
                                             :login_hint])
                     {:scope scope-str
                      :response_type response_type
                      :redirect_uri redirect_uri})]
    (str auth_uri "?"
         (s/join "&"
                 (reduce-kv
                  (fn [c k v]
                    (conj c
                          (str (URLEncoder/encode (name k))
                               "="
                               (URLEncoder/encode v)))) [] params)))
    ))

(defn build-authz-url [{:keys
                        [token_uri
                         client_id client_secret
                         code code_verifier
                         grant_type
                         redirect_uri]
                        :as authz-data
                        :or {redirect_uri "urn:ietf:wg:oauth:2.0:oob"
                             grant_type "authorization_code"}}]
  (let [params (conj (select-keys authz-data [:client_id
                                              :client_secret
                                              :code
                                              :code_verifier])
                     {:redirect_uri redirect_uri
                      :grant_type grant_type})]
    (str token_uri "?"
         (s/join "&"
                 (reduce-kv
                  (fn [c k v]
                    (conj c
                          (str (URLEncoder/encode (name k))
                               "="
                               (URLEncoder/encode v)))) [] params)))
    ))
;; client_id 	The client ID obtained from the API Console Credentials page.
;; client_secret 	The client secret obtained from the API Console Credentials page.
;; code 	The authorization code returned from the initial request.
;; code_verifier 	The code verifier you created in Step 1.
;; grant_type 	As defined in the OAuth 2.0 specification, this field must contain a value of authorization_code.
;; redirect_uri 	One of the redirect URIs listed for your project in the API Console Credentials page for the given client_id.

;; https://developers.google.com/identity/protocols/oauth2/openid-connect#createxsrftoken
(defn generate-state []
  (base64url-encode (make-random-buffer 32)))

;; generate verification code
;; https://tools.ietf.org/html/rfc7636
;; https://tools.ietf.org/html/rfc7636#page-8
(defn generate-cv
  ([] (generate-cv 32))
  ([size] (make-random-buffer size)))


;; https://tools.ietf.org/html/rfc7636#page-8
(defn pkce-challenge-plain [input]
  input)

(defn pkce-challenge-s256 [input]
  (base64url-encode
   (let [md (MessageDigest/getInstance "SHA-256")]
     (.update md (.getBytes input))
     (.digest md))))

;; https://developers.google.com/identity/protocols/oauth2/native-app
