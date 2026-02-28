(ns gappy.test-helpers
  (:require [cheshire.core :as cheshire]
            [clojure.java.io :as io]))

(defn load-fixture
  "Load a JSON fixture file from test/fixtures/ and parse it."
  [filename]
  (cheshire/parse-string
   (slurp (io/file "test/fixtures" filename))
   true))

(defn client-from-fixture
  "Build a client map from a cached discovery document fixture.
   Bypasses network entirely."
  [api version]
  (let [filename (str (name api) "-" (name version) ".json")
        document (load-fixture filename)]
    {:api api
     :version version
     :document document}))

(defn mock-response
  "Build a clj-http-shaped response map."
  [status body & {:keys [headers] :or {headers {"content-type" "application/json"}}}]
  {:status status
   :headers headers
   :body body})

(defn mock-error-response
  "Build a Google API error response body."
  [code message & {:keys [reason domain]}]
  {:error {:code code
           :message message
           :errors [(cond-> {:message message}
                      reason (assoc :reason reason)
                      domain (assoc :domain domain))]}})
