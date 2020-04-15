(require '[clojure.string :as s]
         '[clojure.zip :as zip]
         '[cprop.core :refer [load-config]]
         '[mount.core :as mount]
         '[gappy.config :refer [env]]
         '[cheshire.core :as cheshire]
         '[uritemplate-clj.core :as templ]
         '[clj-http.client :as client]
         '[clojure.java.browse :as browse]
;;         '[clj-oauth2.client :as oauth2])
         '[clojure.core.async :as async :refer [<!! >!! timeout chan alt!!]]
         '[ring.adapter.jetty :as jetty]
         '[gappy.oauth2 :as oauth2])

(mount/start)

;; (def ^:dynamic *dd* (slurp (templ/uritemplate
;;               "https://www.googleapis.com/discovery/v1/apis/{api}/{version}/rest"
;;               {:api "discovery"
;;                :version "v1"
;;                }
;;               )
;;              ))

;; (def ^:dynamic *d*
;;   (client/get "https://www.googleapis.com/discovery/v1/apis/discovery/v1/rest"))

;; (def dd (cheshire/parse-string *dd* true))

(defn disco-bootstrap
  [params]
  (cheshire/parse-string
   (:body (client/get (templ/uritemplate
                       (str (:discovery-base-url env)
                            "apis/{api}/{version}/rest")
                       params) {:accept :json})) true))


;; (defn disco-bootstrap-2
;;   [params]
;;   (cheshire/parse-string
;;    (:body (client/get (templ/uritemplate
;;                        (str (:discovery-base-url env)
;;                             "apis/{api}/{version}/rest")
;;                        params) {:accept :json})) true))

(def admin_sdk_v1 (disco-bootstrap {:api "admin" :version "directory_v1"}))

;; # Step 3.a: Build client surface
;; def build(discovery, collection):
;;   for name, resource in discovery.get('resources', {}).iteritems():
;;     setattr(collection, name, build(resource, Collection()))
;;   for name, method in discovery.get('methods', {}).iteritems():
;;     setattr(collection, name, createNewMethod(name, method))
;;   return collection


;; from https://nukelight.wordpress.com/2012/02/29/clojure-one-liners/
;; (defn de-camelcase [str]
;;     (s/join "-" (map s/lower-case (s/split str #"(?=[A-Z])"))))


;; (defn build
;;   ([node] (build node []))
;;   ([node path]
;;    (if (:methods node)
;;      (println (s/join "." path))
;;      (println (-> node :methods keys)))
;;    (if (:resources node)
;;      (reduce-kv (fn [m k v]
;;                   (assoc m (de-camelcase (name k)) (build v (conj path k))))
;;                 {}
;;                 (:resources node)))
;;    ))

(defn build!
  ([node] (build node []))
  ([node path]
   (when-let [methods (get node "methods")]
     (let [api-namespace (de-camelcase (s/join "." (concat (:ns-base-path env) path)))
           pushed-namespace *ns*]
       (in-ns (symbol api-namespace))
;;       (refer-clojure) ;; this isn't actually required
       (reduce-kv (fn [m k v]
                    (let [method-name (symbol (de-camelcase k))
                          method-meta {:doc (get v "description")}]
;;                      (println (str api-namespace ": " method-name ": " method-meta))
                      (intern (symbol api-namespace) (with-meta method-name method-meta) (fn[] v))
                      ))
                  nil (get node "methods"))
       (in-ns (ns-name pushed-namespace))))
   (when-let [resources (get node "resources")]
     (reduce-kv (fn [m k v]
                  (assoc m (de-camelcase (name k)) (build v (conj path k))))
                {}
                resources))
   ))

(defn get-resources
  ([node] (build node []))
  ([node path]
   (when-let [methods (get node "methods")]
     (let [api-namespace (de-camelcase (s/join "." (concat (:ns-base-path env) path)))
           pushed-namespace *ns*]
       (in-ns (symbol api-namespace))
;;       (refer-clojure) ;; this isn't actually required
       (reduce-kv (fn [m k v]
                    (let [method-name (symbol (de-camelcase k))
                          method-meta {:doc (get v "description")}]
;;                      (println (str api-namespace ": " method-name ": " method-meta))
                      (intern (symbol api-namespace) (with-meta method-name method-meta) (fn[] v))
                      ))
                  nil (get node "methods"))
       (in-ns (ns-name pushed-namespace))))
   (when-let [resources (get node "resources")]
     (reduce-kv (fn [m k v]
                  (assoc m (de-camelcase (name k)) (build v (conj path k))))
                {}
                resources))
   ))



(browse/browse-url "https://www.google.com/")


;; (defn disco-bootstrap
;;   [params]
;;   (cheshire/parse-string
;;    (slurp (templ/uritemplate
;;            (str (:discovery-base-url env) "apis/{api}/{version}/rest")
;;            params))))

;; https://developers.google.com/discovery/v1/building-a-client-library
;; def createNewMethod(name, method):
;;   # Step 2.b Compose request
;;   def newMethod(**kwargs):
;;     body = kwargs.pop('body', None)
;;     url = urlparse.urljoin(BASE_URL, uritemplate.expand(method['path'], kwargs))
;;     for pname, pconfig in method.get('parameters', {}).iteritems():
;;       if pconfig['location'] == 'path' and pname in kwargs:
;;         del kwargs[pname]
;;     if kwargs:
;;       url = url + '?' + urllib.urlencode(kwargs)
;;     return h.request(url, method=method['httpMethod'], body=body,
;;                      headers={'content-type': 'application/json'})

;;   return newMethod


;; http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/


;; (def creds (cheshire/parse-string (slurp "/Users/ags/.credentials/gappy.json")))

(def creds (-> env :credentials-file slurp (cheshire/parse-string true)))
(def icreds (:installed creds))
{:authorization-uri (:auth_uri icreds)
 :client-id (:client_id icreds)
 :client-secret (:client_secret icreds)
 :redirect-uri "http://localhost:8080"
 :scope ["https://www.googleapis.com/auth/admin.directory.user.readonly"]}


(oauth2/build-authn-url (conj icreds
                              {:scope ["https://www.googleapis.com/auth/admin.directory.user.readonly"]}))

(oauth2/make-auth-request {:authorization-uri (:auth_uri icreds)
                           :client-id (:client_id icreds)
                           :client-secret (:client_secret icreds)
                           :redirect-uri "http://localhost:8080"
                           :scope ["https://www.googleapis.com/auth/admin.directory.user.readonly"]})

(def auth-req
  (oauth2/make-auth-request google-com-oauth2))

(defn handler [request-map]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "<html><body> your IP is: "
              (:remote-addr request-map) "</body></html>")})

(def server (jetty/run-jetty handler {:port 3000 :join? false}))
(def oob-redirect-uri "urn:ietf:wg:oauth:2.0:oob")

;; https://developers.google.com/identity/protocols/oauth2/native-app
;; https://developers.google.com/identity/protocols/oauth2/native-app#obtainingaccesstokens

(apply str (doall (map char (.encode (.withoutPadding (Base64/getUrlEncoder)) (let [result (byte-array 32)] (.nextBytes (SecureRandom.) result) result)))))

(import java.net.URLEncode)

;;(browse/browse-url (make-auth-url (conj icreds {:scope ["https://www.googleapis.com/auth/admin.directory.user.readonly"]
;;                                   :redirect_uri "urn:ietf:wg:oauth:2.0:oob"})))

;; generate verification code
;; https://tools.ietf.org/html/rfc7636
;; https://tools.ietf.org/html/rfc7636#page-8
;; base64url-encoding
;; https://tools.ietf.org/html/rfc7636#appendix-A
;; (based on https://tools.ietf.org/html/rfc4648#page-7 )

;; https://github.com/clojure/data.codec superseded by Base64 encoding in JDK
;; https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html

;; state token https://developers.google.com/identity/protocols/oauth2/openid-connect#createxsrftoken


;; https://github.com/googleapis/google-auth-library-python-oauthlib/blob/master/google_auth_oauthlib/flow.py
;; from_client_config
;; https://github.com/googleapis/google-auth-library-python-oauthlib/blob/master/google_auth_oauthlib/helpers.py
;; session_from_client_config
;; https://github.com/requests/requests-oauthlib/blob/master/requests_oauthlib/oauth2_session.py
;; OAuth2Session
;; https://docs.python.org/2/library/wsgiref.html

;; requests.Session.state()

(def pkce-challenge (gappy.oauth2/pkce-challenge-s256 (gappy.oauth2/base64url-encode pkce)))



         


(let [ce (oauth2/collate-code-exchange (conj icreds
                               {:code ""
                                :code_verifier code-verifier}))]
        (client-get (:url ce) {:query-params (:query-params ce)}))


(def icreds (:installed creds))
(def state (oauth2/generate-state))
(def code-verifier (oauth2/generate-code-verifier))
(def code-challenge (oauth2/pkce-challenge-s256 code-verifier))
(browse/browse-url (oauth2/build-authn-url (conj icreds
                                                 {:scope ["https://www.googleapis.com/auth/admin.directory.user.readonly"]
                                                  :code_challenge code-challenge
                                                  :code_challenge_method "S256"}
                                                 (if state {:state state}))))
(def token-data (oauth2/exchange-code-for-token icreds "" code-verifier))
(oauth2/refresh-token icreds (:token token-data))
(oauth2/revoke-token (:refresh_token (:token token-data)))
