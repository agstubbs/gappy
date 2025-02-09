(ns gappy.build
  (:require [gappy.config :refer [env]]
            [uritemplate-clj.core :as templ]
            [clj-http.client :as client]
            [gappy.util :as util]
            [clojure.string :as s])
  )

(defn build-2 [api]
  (let [methods (fn [start]
                  (if start
                    (into {}
                          (reduce-kv (fn [m k v]
                                       (conj m
                                             {(keyword (:id v)) v}))
                                     nil
                                     start)
                          )))
        resources (fn r [start]
                    (if start
                      (into {}
                            (reduce-kv (fn [m k v]
                                         (conj m
                                               (r (:resources v))
                                               (methods (:methods v))))
                                       nil
                                       start)
                            )))
        method-map (into {} (conj (methods (:methods api)) (resources (:resources api))))]
    (fn [& {:keys [method params]}]
      (let [method-data (method-map method)
            all-param-schema (conj (:parameters api) (:parameters method-data))
            location? (fn [location] (fn [v] (= location (:location (last v)))))
            pathp (into {} (filter (location? "path") all-param-schema))
            queryp (into {} (filter (location? "query") all-param-schema))
            path (templ/uritemplate (:path method-data) (select-keys params (keys pathp)))
            query (util/build-query-str (select-keys params (keys queryp)))
            uri (str (:rootUrl api) (:servicePath api) path (if (and query (> (count query) 0)) (str "?" query) ""))]
        uri)
      ))
)

(defn build-3 [api]
  (let [methods (fn [start]
                  (if start
                    (into {}
                          (reduce-kv (fn [m k v]
                                       (conj m
                                             {(keyword (:id v)) v}))
                                     nil
                                     start)
                          )))
        resources (fn r [start]
                    (if start
                      (into {}
                            (reduce-kv (fn [m k v]
                                         (conj m
                                               (r (:resources v))
                                               (methods (:methods v))))
                                       nil
                                       start)
                            )))
        method-map (into {} (conj (methods (:methods api)) (resources (:resources api))))]
    (fn [& {:keys [method params headers]}]
      (let [method-data (method-map method)
            all-param-schema (conj (:parameters api) (:parameters method-data))
            location? (fn [location] (fn [v] (= location (:location (last v)))))
            pathp (into {} (filter (location? "path") all-param-schema))
            queryp (into {} (filter (location? "query") all-param-schema))
            path-params (select-keys params (keys pathp))
            path (templ/uritemplate (:path method-data) (select-keys params (keys pathp)))
            query-params (select-keys params (keys queryp))]
        {:httpMethod (keyword (s/lower-case (:httpMethod method-data)))
         :full-path (str (:rootUrl api) (:servicePath api) path)
         :rootUrl (:rootUrl api)
         :servicePath (:servicePath api)
         :path path
         :query-params query-params
         :path-params path-params
         :headers headers})
      ))
)
