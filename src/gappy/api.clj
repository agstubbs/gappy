(ns gappy.api
  (:require [gappy.config :refer [env]]
            [clj-http.client :as http]
            [uritemplate-clj.core :as templ]
            [gappy.util :as util]
            [gappy.discovery :as disco]
            [clojure.string :as s]
  ))

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
        service-path (-> client :document :servicePath)
        common-parameters (-> client :document :parameters)
        ]
    {:method method-data
     :client-path method-path
     :http-method (keyword (s/lower-case (:httpMethod method-data)))
     :full-path (str root-url service-path (:path method-data))
     :root-url root-url
     :service-path service-path
     :common-parameters common-parameters
     :method-parameters (:parameters method-data)
     :query-parameters query-params
     :path-parameters path-params
     :description (:description method-data)
     :scopes (merge null-scope-def scope-def)
     }))

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

(defmulti invoke
  (fn [resource &[{:keys [op] :as params}]]
    (-> resource :methods op :http-method)))

(defmethod invoke :get [resource & [{:keys [op request client-params] :as params}]]
  (let [m (-> resource :methods op)
        path-params (select-keys request (keys (:path-parameters m)))
        query-params (select-keys request (keys (:query-parameters m)))
        full-path (templ/uritemplate (:full-path m) path-params)
        query-str (util/build-query-str query-params)
        uri (str full-path (if query-str (str "?" query-str)))
        response (http/get uri
                           (merge {:accept :json :as :json}
                                  (:default-client-params resource)
                                  client-params))]
    (with-meta (:body response) (dissoc response :body))
    ))

(defmethod invoke :post [resource & [{:keys [op request body client-params] :as params}]]
  (let [m (-> resource :methods op)
        path-params (select-keys request (keys (:path-parameters m)))
        query-params (select-keys request (keys (:query-parameters m)))
        full-path (templ/uritemplate (:full-path m) path-params)
        query-str (util/build-query-str query-params)
        uri (str full-path (if query-str (str "?" query-str)))
        response (http/post uri
                            (merge {:accept :json :as :json
                                    :content-type :json
                                    :json-opts {}}
                                   (:default-client-params resource)
                                   (if body {:body body})
                                   client-params))]
    (with-meta (:body response) (dissoc response :body))
    ))
