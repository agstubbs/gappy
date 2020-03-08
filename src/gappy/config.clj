(ns gappy.config
  (:require [cprop.core :refer [load-config]]
            [mount.core :refer [defstate]]
            )
  )

(defstate env
  :start
  (load-config :resource "config.edn"))
