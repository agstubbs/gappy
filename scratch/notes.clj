(require '[clojure.string :as s]
         '[clojure.zip :as zip]
         '[cprop.core :refer [load-config]]
         '[mount.core :as mount]
         '[gappy.config :refer [env]]
         '[cheshire.core :as cheshire]
         '[uritemplate-clj.core :as templ]
         '[clj-http.client :as client]
         '[clojure.java.browse :as browse])

(mount/start)

(def ^:dynamic *dd* (slurp (templ/uritemplate
              "https://www.googleapis.com/discovery/v1/apis/{api}/{version}/rest"
              {:api "discovery"
               :version "v1"
               }
              )
             ))

(def ^:dynamic *d*
  (client/get "https://www.googleapis.com/discovery/v1/apis/discovery/v1/rest"))

(def dd (cheshire/parse-string *dd* true))

;; # Step 3.a: Build client surface
;; def build(discovery, collection):
;;   for name, resource in discovery.get('resources', {}).iteritems():
;;     setattr(collection, name, build(resource, Collection()))
;;   for name, method in discovery.get('methods', {}).iteritems():
;;     setattr(collection, name, createNewMethod(name, method))
;;   return collection


;; from https://nukelight.wordpress.com/2012/02/29/clojure-one-liners/
(defn de-camelcase [str]
    (s/join "-" (map s/lower-case (s/split str #"(?=[A-Z])"))))


;; (defn build
;;   ([node] (build node []))
;;   ([node path]
;;    (if (:methods node)
;;      (println (s/join "." path))
;;      (println (-> node :methods keys)))
;;    (if (:resources node)
;;      (reduce-kv (fn [m k v]
;;                   (assoc m (de-camelcase (name k)) (build v (conj path k))))
;;                 {}
;;                 (:resources node)))
;;    ))

(defn build
  ([node] (build node []))
  ([node path]
   (when-let [methods (get node "methods")]
     (let [api-namespace (de-camelcase (s/join "." (concat (:ns-base-path env) path)))
           pushed-namespace *ns*]
       (in-ns (symbol api-namespace))
       (refer-clojure)
       (reduce-kv (fn [m k v]
                    (doall
                     (println (str api-namespace ": " (de-camelcase k)))
                     (println (get v "description"))))
                  nil (get node "methods"))
       (println (str api-namespace ": " (keys methods)))
       (in-ns (ns-name pushed-namespace))))
   (when-let [resources (get node "resources")]
     (reduce-kv (fn [m k v]
                  (assoc m (de-camelcase (name k)) (build v (conj path k))))
                {}
                resources))
   ))

(browse/browse-url "https://www.google.com/")


;; (defn disco-bootstrap
;;   [params]
;;   (cheshire/parse-string
;;    (slurp (templ/uritemplate
;;            (str (:discovery-base-url env) "apis/{api}/{version}/rest")
;;            params))))

(defn disco-bootstrap
  [params]
  (cheshire/parse-string
   (:body (client/get (templ/uritemplate
                       (str (:discovery-base-url env)
                            "apis/{api}/{version}/rest")
                       params) {:accept :json}))))



;; https://developers.google.com/discovery/v1/building-a-client-library
;; def createNewMethod(name, method):
;;   # Step 2.b Compose request
;;   def newMethod(**kwargs):
;;     body = kwargs.pop('body', None)
;;     url = urlparse.urljoin(BASE_URL, uritemplate.expand(method['path'], kwargs))
;;     for pname, pconfig in method.get('parameters', {}).iteritems():
;;       if pconfig['location'] == 'path' and pname in kwargs:
;;         del kwargs[pname]
;;     if kwargs:
;;       url = url + '?' + urllib.urlencode(kwargs)
;;     return h.request(url, method=method['httpMethod'], body=body,
;;                      headers={'content-type': 'application/json'})

;;   return newMethod


;; http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/
