(ns gappy.oauth2
  (:require [clojure.string :as s]
            [clj-http.client :as client])
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

;; https://developers.google.com/identity/protocols/oauth2/openid-connect#createxsrftoken
(defn generate-state []
  (base64url-encode (make-random-buffer 32)))

;; generate verification code
;; https://tools.ietf.org/html/rfc7636
;; https://tools.ietf.org/html/rfc7636#page-8
(defn generate-code-verifier
  ([] (generate-code-verifier 32))
  ([size] (base64url-encode (make-random-buffer size))))


;; https://tools.ietf.org/html/rfc7636#page-8
(defn pkce-challenge-plain [input]
  input)

(defn pkce-challenge-s256 [input]
  (base64url-encode
   (let [md (MessageDigest/getInstance "SHA-256")]
     (.update md (.getBytes input))
     (.digest md))))

;; https://developers.google.com/identity/protocols/oauth2/native-app


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

(defn exchange-code-for-token
  ([client-credentials code]
   (exchange-code-for-token client-credentials code nil))
  ([{:keys [token_uri client_id client_secret redirect_uri]
     :or {redirect_uri "urn:ietf:wg:oauth:2.0:oob"}
     :as client-credentials}
    code
    code_verifier]
   (let [code_map (conj {:grant_type "authorization_code"
                         :redirect_uri redirect_uri
                         :code code}
                        (if code_verifier {:code_verifier code_verifier}))
         response (client/post token_uri
                               {:query-params (conj (select-keys client-credentials
                                                                 [:client_id
                                                                  :client_secret
                                                                  :code
                                                                  :code_verifier])
                                                    code_map)
                                :throw-exceptions false
                                :as :json})]
     {:status (:status response)
      :reason-phrase (:reason-phrase response)
      :token (:body response)}
     )))

    
(defn refresh-token [{:keys [token_uri client_id client_secret] :as client-credentials}
                     {:keys [refresh_token] :as token}]
  (let [response (client/post token_uri
                              {:content-type "application/x-www-form-urlencoded"
                               :query-params {:client_id client_id
                                              :client_secret client_secret
                                              :refresh_token refresh_token
                                              :grant_type "refresh_token"}
                               :as :json})]
    {:status (:status response)
     :reason-phrase (:reason-phrase response)
     :token (:body response)}
    ))

(defn revoke-token [token]
  (let [response (client/post "https://oauth2.googleapis.com/revoke"
                              {:content-type "application/x-www-form-urlencoded"
                               :query-params {:token token}
                               :as :json})]
    {:status (:status response)
     :reason-phrase (:reason-phrase response)
     :token (:body response)}
    ))
