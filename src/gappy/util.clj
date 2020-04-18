(ns gappy.util
  (:require [ring.adapter.jetty :as jetty]
            [gappy.oauth2 :as oauth2]
            [gappy.config :refer [env]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.util.http-response :as response]
            [clojure.java.browse :as browse]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.async :as a])
  (:import java.net.URLEncoder))

(defn build-query-str [m]
    (s/join "&"
            (reduce-kv
             (fn [c k v]
               (conj c
                     (str (URLEncoder/encode (name k))
                          "="
                          (URLEncoder/encode v)))) [] m))
  )

(defn get-handler [state c]
  (fn [request-map]
    (if (= (:state (:params request-map)) state)
      (if (a/>!! c (:code (:params request-map)))
        (response/ok (slurp (io/resource "html/200.html")))
        (response/internal-server-error (slurp (io/resource "html/500.html"))))
      (response/bad-request (slurp (io/resource "html/400.html"))))
    ))

(defn run-browser-flow [creds scope & {:keys [port timeout] :as opts :or {port 3000 timeout 300000}}]
  (let [state (oauth2/generate-state)
        code-verifier (oauth2/generate-code-verifier)
        code-challenge (oauth2/pkce-challenge-s256 code-verifier)
        redirect-uri (str "http://localhost:" port "/")
        result-chan (a/chan 1)
        server (jetty/run-jetty
                (-> (get-handler state result-chan) wrap-keyword-params wrap-params)
                {:port port :join? false})]
    (browse/browse-url (oauth2/build-authn-url (conj creds
                                                     {:scope scope
                                                      :code_challenge code-challenge
                                                      :code_challenge_method "S256"
                                                      :redirect_uri redirect-uri}
                                                     (if state {:state state}))))
    (let [[code _] (a/alts!! [result-chan (a/timeout timeout)])]
      ;; FIXME: race condition where (.stop server) is run before page is actually served!
      (.stop server)
      (if code (:token (oauth2/obtain-token (assoc creds :redirect_uri redirect-uri)
                                            code code-verifier))))))
