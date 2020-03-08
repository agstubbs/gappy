(ns gappy.core
  (:require [cprop.core :refer [load-config]]
            [mount.core :as mount]
            [gappy.config :refer [env]]
            [cheshire.core :as cheshire]
            [uritemplate-clj.core :as templ]
            [clj-http.core :as http]))
