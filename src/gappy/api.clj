(ns gappy.api
  (:require [gappy.config :refer [env]]
            [clj-http.client :as http]
            [uritemplate-clj.core :as templ]
            [gappy.util :as util]
            [gappy.discovery :as disco]
  ))

(defn client [{:keys [api version] :as params}]
  {:api api
   :version version
   :document (disco/get-discovery-document params)})

(defn resource [client k & ks]
  (assoc client :resource (into [] (conj ks k))))

(defn -get-resource-path [resource]
  (into []
        (->> resource
             :resource
             (apply conj [:document])
             (interpose :resources))))

(defn -get-method-def [resource method]
  (let [path (conj (-get-resource-path resource)
                   :methods method)]
    (get-in resource path)))

(defn doc-edn [resource method]
  (let [method-def (-get-method-def resource method)
        method-scopes (map keyword (:scopes method-def))
        null-scope-def (reduce #(assoc %1 (keyword %2) :not-found)
                               {}
                               method-scopes)
        scope-def (-> resource
                      :document
                      :auth
                      :oauth2
                      :scopes
                      (select-keys method-scopes))
        ]
    {:common-parameters (-> resource :document :parameters)
     :scopes (merge null-scope-def scope-def)
     :description (:description method-def)
     :parameters (:parameters method-def)
     }))

(defn doc-str [resource method]
  (let [doc-edn (doc-edn resource method)]
    (:description doc-edn)
    ))

(defn ops [resource]
  (let [path (conj (-get-resource-path resource) :methods)]
    (keys (get-in resource path))))


(defmulti invoke
  (fn [resource &[{:keys [op] :as params}]]
    (-> resource (-get-method-def op) :httpMethod)))

(defmethod invoke "GET" [resource & [params]]
  params
  )
