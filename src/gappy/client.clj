(ns gappy.client
  (:require [gappy.config :refer [env]]
            [clj-http.client :as http]
            [clj-http.headers :as h]
            [gappy.util :as util]
            [clojure.string :as s]
  ))


(def ^:private crlf (str \return \newline))

(defmulti exec (fn [x & r] (:httpMethod x)))

(defmethod exec :get [method-data & {:keys [auth-token] :as options}]
  (let [query-str (util/build-query-str (:query-params method-data))
        uri (str (:full-path method-data) (if query-str (str "?" query-str)))]
    (http/get uri {:accept :json :as :json :oauth-token (:access_token auth-token)}))
  )

(defmulti batch-1 :httpMethod)
(defmethod batch-1 :get [method-data]
  (let [path (str (if (s/starts-with? (:servicePath method-data) "/") "" "/")
                  (:servicePath method-data)
                  (:path method-data))
        query-str (util/build-query-str (:query-params method-data) :prepend true)
        headers (:headers method-data)]
    (s/join crlf
            (concat [ (str "GET " path query-str) ]
                    (reduce-kv (fn [m k v] (conj m (str (h/canonicalize k) ": " v))) nil headers)
                    [ "" ]
                    )
            )
    )
)
