(ns gappy.client
  (:require [gappy.config :refer [env]]
            [clj-http.client :as http]
            [gappy.util :as util])
  )


(defmulti exec (fn [x & r] (:httpMethod x)))

(defmethod exec "GET" [method-data & {:keys [auth-token] :as options}]
  (let [query (util/build-query-str (:query-params method-data))
        uri (str (:endpoint method-data) (if (and query (> (count query) 0)) (str "?" query) ""))]
    (http/get uri {:accept :json :as :json :oauth-token (:access_token auth-token)}))
  )

