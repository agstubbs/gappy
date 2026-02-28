(ns gappy.api
  (:require [gappy.config :refer [env]]
            [clj-http.client :as http]
            [uritemplate-clj.core :as templ]
            [gappy.util :as util]
            [gappy.discovery :as disco]
            [cheshire.core :as cheshire]
            [clojure.string :as s]))

(defn client [{:keys [api version default-client-params] :as params}]
  (assoc params
         :document (disco/get-discovery-document (select-keys params [:api :version]))))

(defn -method-data [client method-path]
  (let [method-data (get-in client method-path)
        method-scopes (map keyword (:scopes method-data))
        null-scope-def (reduce #(assoc %1 (keyword %2) :not-found)
                               {}
                               method-scopes)
        scope-def (-> client
                      :document
                      :auth
                      :oauth2
                      :scopes
                      (select-keys method-scopes))
        location? (fn [location] (fn [v] (= location (:location (last v)))))
        all-param-schema (merge (-> client :document :parameters) (:parameters method-data))
        path-params (into {} (filter (location? "path") all-param-schema))
        query-params (into {} (filter (location? "query") all-param-schema))
        root-url (-> client :document :rootUrl)
        service-path (-> client :document :servicePath)]
    {:method method-data
     :client-path method-path
     :http-method (keyword (s/lower-case (:httpMethod method-data)))
     :full-path (str root-url service-path (:path method-data))
     :root-url root-url
     :service-path service-path
     :common-parameters (-> client :document :parameters)
     :method-parameters (:parameters method-data)
     :query-parameters query-params
     :path-parameters path-params
     :description (:description method-data)
     :scopes (merge null-scope-def scope-def)}))

(defn resource [client k & ks]
  (let [resource-path (into [] (conj ks k))
        rpath (into [] (->> resource-path (into [:document]) (interpose :resources)))
        resource (get-in client rpath)
        method-names (keys (:methods resource))
        methods (zipmap method-names
                        (map #(-method-data client (conj rpath :methods %1)) method-names))]
    (merge client
           {:resource-path resource-path
            :methods methods
            :method-names method-names
            :rpath rpath
            :resource resource})))

(defn doc [resource method]
  (-> resource :methods method :description))

(defn ops [resource]
  (-> resource :methods keys))

(defn resources [o]
  (let [base (or (:resource o) (:document o))
        path (or (:resource-path o) [])]
    (->> base :resources keys (map #(conj path %)))))

;; --- URI building (shared by all invoke methods) ---

(defn -build-uri
  "Build the full URI for a method invocation, interpolating path params
   and appending query params."
  [method-data request]
  (let [path-params (select-keys request (keys (:path-parameters method-data)))
        query-params (select-keys request (keys (:query-parameters method-data)))
        full-path (templ/uritemplate (:full-path method-data) path-params)
        query-str (util/build-query-str query-params)]
    (str full-path (if query-str (str "?" query-str)))))

;; --- Error handling ---

(defn -parse-error
  "Parse a Google API error response from an ExceptionInfo thrown by clj-http."
  [ex]
  (let [data (ex-data ex)
        status (:status data)
        body (:body data)
        error-body (if (map? body)
                     body
                     (try
                       (cheshire/parse-string body true)
                       (catch Exception _ nil)))
        google-error (:error error-body)]
    (ex-info (or (:message google-error) (.getMessage ex))
             {:gappy/error true
              :status status
              :code (:code google-error)
              :message (:message google-error)
              :errors (:errors google-error)
              :body error-body}
             ex)))

(defn -execute-request
  "Execute an HTTP request with error handling.
   http-fn is one of http/get, http/post, http/put, http/patch, http/delete.
   Returns the response body with HTTP metadata attached as metadata."
  [http-fn uri opts]
  (try
    (let [response (http-fn uri opts)]
      (with-meta (:body response) (dissoc response :body)))
    (catch clojure.lang.ExceptionInfo ex
      (throw (-parse-error ex)))))

;; --- Invoke multimethod ---

(defmulti invoke
  (fn [resource &[{:keys [op] :as params}]]
    (-> resource :methods op :http-method)))

(defmethod invoke :get [resource & [{:keys [op request client-params]}]]
  (let [m (-> resource :methods op)
        uri (-build-uri m request)]
    (-execute-request http/get uri
                      (merge {:accept :json :as :json}
                             (:default-client-params resource)
                             client-params))))

(defmethod invoke :post [resource & [{:keys [op request body client-params]}]]
  (let [m (-> resource :methods op)
        uri (-build-uri m request)]
    (-execute-request http/post uri
                      (merge {:accept :json :as :json
                              :content-type :json
                              :json-opts {}}
                             (:default-client-params resource)
                             (if body {:body body})
                             client-params))))

(defmethod invoke :put [resource & [{:keys [op request body client-params]}]]
  (let [m (-> resource :methods op)
        uri (-build-uri m request)]
    (-execute-request http/put uri
                      (merge {:accept :json :as :json
                              :content-type :json
                              :json-opts {}}
                             (:default-client-params resource)
                             (if body {:body body})
                             client-params))))

(defmethod invoke :patch [resource & [{:keys [op request body client-params]}]]
  (let [m (-> resource :methods op)
        uri (-build-uri m request)]
    (-execute-request http/patch uri
                      (merge {:accept :json :as :json
                              :content-type :json
                              :json-opts {}}
                             (:default-client-params resource)
                             (if body {:body body})
                             client-params))))

(defmethod invoke :delete [resource & [{:keys [op request client-params]}]]
  (let [m (-> resource :methods op)
        uri (-build-uri m request)]
    (-execute-request http/delete uri
                      (merge {:accept :json :as :json}
                             (:default-client-params resource)
                             client-params))))

;; --- Pagination ---

(defn invoke-seq
  "Returns a lazy sequence of all items across paginated responses.
   items-key is the key in the response containing the result list (e.g. :users).
   Automatically follows :nextPageToken until exhausted."
  [resource {:keys [op request client-params items-key] :as params}]
  (letfn [(fetch-page [page-token]
            (let [req (if page-token
                        (assoc request :pageToken page-token)
                        request)
                  response (invoke resource {:op op
                                             :request req
                                             :client-params client-params})
                  items (get response items-key)
                  next-token (:nextPageToken response)]
              (lazy-cat items
                        (when next-token
                          (fetch-page next-token)))))]
    (fetch-page nil)))
