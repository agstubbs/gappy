(ns gappy.util
  (:require [ring.adapter.jetty :as jetty]
            [gappy.oauth2 :as oauth2]
            [gappy.config :refer [env]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.util.http-response :as response]
            [clojure.java.browse :as browse]
            [clojure.core.async :as a])
  )

(defn get-handler [state c]
  (fn [request-map]
    (if (= (:state (:params request-map)) state)
      (if (a/>!! c (:code (:params request-map)))
        (response/ok)
        (response/internal-server-error))
      (response/bad-request))
    ))

(defn run-browser-flow [creds scope]
  (let [state (oauth2/generate-state)
        code-verifier (oauth2/generate-code-verifier)
        code-challenge (oauth2/pkce-challenge-s256 code-verifier)
        redirect-uri "http://localhost:3000/"
        result-chan (a/chan 1)
        server (jetty/run-jetty
                (-> (get-handler state result-chan) wrap-keyword-params wrap-params)
                {:port 3000 :join? false})]
    (browse/browse-url (oauth2/build-authn-url (conj creds
                                                     {:scope scope
                                                      :code_challenge code-challenge
                                                      :code_challenge_method "S256"
                                                      :redirect_uri redirect-uri}
                                                     (if state {:state state}))))
    (let [[code timeout] (a/alts!! [result-chan (a/timeout 300000)])]
      (.stop server)
      (oauth2/obtain-token (assoc creds :redirect_uri redirect-uri)
                           code code-verifier))))
