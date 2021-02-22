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
  (:import java.net.URLEncoder
           java.lang.NumberFormatException))

(def crlf (str \return \newline))
(def crlfcrlf (str crlf crlf))

;; clojure.string/split and java.lang.String.split use regex; boundary text can include regex special chars; so we just roll our own here
;; https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
(defn split-first [part boundary]
  (if-let [occur (and part (s/index-of part boundary))]
    (let [pre (subs part 0 occur)
          post (subs part (+ (count boundary) occur))]
      (list pre post)
      )
    nil
  ))
  
(defn split-first-nonempty [part boundary]
  (if-let [sf (split-first part boundary)]
    (loop [[pre post] sf]
      (if-not (and pre (s/blank? pre))
        (if pre
          (list pre post))
        (recur (split-first post boundary))
  ))))

;; clojure.string/split and java.lang.String.split use regex; boundary text can include regex special chars; so we just roll our own here
;; https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
(defn split-multi [parts boundary]
  (if-let [occur (s/index-of parts boundary)]
    (let [pre (subs parts 0 occur)
          post (subs parts (+ (count boundary) occur))]
      (if (= 0 (count boundary))
        (list parts)
        (concat (if (> occur 0) [pre]) (if (> (count post) 0) (split-multi post boundary))))
      )
    (list parts)))

(defn headers [^String s]
  (let [headers (split-multi s crlf)]
    (into {}(map #(let [splits (split-first % ":")
                h (keyword (s/lower-case (first splits)))
                v (s/trim (second splits))]
            (first {h v})) headers))))

(defn header-body [^String s]
  (if-let [splits (split-first s crlfcrlf)]
    {:headers (headers (first splits))
     :body (second splits)}
    {:body s}
  ))

(defn build-query-str [m & {:keys [prepend] :as opts}]
  (if (empty? m)
    nil
    (str (if prepend "?")
         (s/join "&"
                 (reduce-kv
                  (fn [c k v]
                    (conj c
                          (str (URLEncoder/encode (name k))
                               "="
                               (URLEncoder/encode v)))) [] m))
         )
    ))

(defn get-status-line [s]
  (let [[protover status-str reason-phrase] (s/split s #" ")
        [proto ver] (s/split protover #"/")
        [major-str minor-str] (s/split ver #"\.")
        status (try (Integer. status-str) (catch NumberFormatException e nil))
        major (try (Integer. major-str) (catch NumberFormatException e nil))
        minor (try (Integer. minor-str) (catch NumberFormatException e nil))]
    {:protocol-version {:name proto :major major :minor minor}
     :status status
     :reason-phrase reason-phrase}
    ))
    

(defn get-http-response [s]
  (let [[status resp] (split-first-nonempty s crlf)
        response (header-body resp)]
    (conj (get-status-line status) {
     :headers (:headers response)
     :body (:body response)})))
    

(defmulti process (comp :content-type :headers))
(defmethod process "application/http" [x]
  (println x)
  ) 
(defmethod process nil [x]
  (println "nil")
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
