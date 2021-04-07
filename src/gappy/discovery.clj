(ns gappy.discovery
  (:require [clj-http.client :as client]
            [gappy.config :refer [env]]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [cheshire.core :as cheshire]
            )
  )

(defn get-discovery-directory []
  (if-let [cached (some-> env
                          :discovery-cache-location
                          (str (:discovery-directory-resource env))
                          io/resource)]
    (cheshire/parse-string (slurp cached) true)
    (let [response (client/get (:discovery-directory-url env))]
      (with-meta (cheshire/parse-string (:body response) true) {:status (:status response) :headers (:headers response)}))))

(defn get-discovery-map []
  (let [doc (get-discovery-directory)
        items (:items doc)
        mdoc (meta doc)]
    (with-meta (reduce #(assoc-in %1 [(keyword (:name %2)) (keyword (:version %2))] %2) {} items) mdoc)))

(defn get-discovery-document
  "Returns a discovery document"
  [{:keys [api version]}]
  (if-let [cached (cheshire/parse-string
                   (some-> env
                           :discovery-cache-location
                           (str (name api) "-" (name version) ".json")
                           io/resource slurp)
                   true)]
    cached
    (let [disco-map (get-discovery-map)
          retrieved (client/get (get-in disco-map [api version :discoveryRestUrl])
                                {:accept :json
                                 :as :json})
          status (:status retrieved)
          headers (:headers retrieved)
          body (:body retrieved)]
      (with-meta body {:headers headers :status status}))))

