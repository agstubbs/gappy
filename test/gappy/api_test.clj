(ns gappy.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [gappy.api :as api]
            [gappy.test-helpers :as h]
            [clj-http.client :as http]
            [clojure.string :as s]))

;; --- Layer 1: Pure function tests (no HTTP) ---

(deftest resource-navigation-test
  (testing "navigates to a top-level resource"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (is (some? (:methods users)))
      (is (some? (:resource users)))
      (is (= [:users] (:resource-path users)))))

  (testing "navigates to nested sub-resource"
    (let [client (h/client-from-fixture :admin :directory_v1)
          aliases (api/resource client :users :aliases)]
      (is (some? (:methods aliases)))
      (is (= [:users :aliases] (:resource-path aliases))))))

(deftest ops-test
  (testing "lists operations on users resource"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          operations (api/ops users)]
      (is (some #{:list} operations))
      (is (some #{:get} operations))
      (is (some #{:insert} operations))
      (is (some #{:update} operations))
      (is (some #{:patch} operations))
      (is (some #{:delete} operations)))))

(deftest doc-test
  (testing "returns description string for a method"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          desc (api/doc users :list)]
      (is (string? desc))
      (is (s/includes? desc "list")))))

(deftest resources-test
  (testing "lists sub-resources from client"
    (let [client (h/client-from-fixture :admin :directory_v1)
          res (api/resources client)]
      (is (some #{[:users]} res))
      (is (some #{[:groups]} res))))

  (testing "lists sub-resources from resource"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          sub-res (api/resources users)]
      (is (some #{[:users :aliases]} sub-res)))))

(deftest method-data-test
  (testing "extracts correct HTTP method"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (is (= :get (-> users :methods :list :http-method)))
      (is (= :post (-> users :methods :insert :http-method)))
      (is (= :put (-> users :methods :update :http-method)))
      (is (= :patch (-> users :methods :patch :http-method)))
      (is (= :delete (-> users :methods :delete :http-method)))))

  (testing "extracts path and query parameters"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          get-method (-> users :methods :get)]
      (is (some? (:path-parameters get-method)))
      (is (contains? (:path-parameters get-method) :userKey))))

  (testing "extracts full path with rootUrl and servicePath"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          list-method (-> users :methods :list)]
      (is (s/includes? (:full-path list-method) "https://admin.googleapis.com/"))
      (is (s/includes? (:full-path list-method) "admin/directory/v1/")))))

(deftest build-uri-test
  (testing "builds URI with query params"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          m (-> users :methods :list)
          uri (api/-build-uri m {:customer "my_customer" :maxResults "10"})]
      (is (s/includes? uri "admin/directory/v1/users"))
      (is (s/includes? uri "customer=my_customer"))
      (is (s/includes? uri "maxResults=10"))))

  (testing "builds URI with path params"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          m (-> users :methods :get)
          uri (api/-build-uri m {:userKey "alice@example.com"})]
      (is (s/includes? uri "users/alice@example.com")))))

;; --- Layer 2: HTTP mocking tests ---

(deftest invoke-get-test
  (testing "constructs correct GET request and returns body"
    (let [captured (atom nil)
          client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/get (fn [url opts]
                               (reset! captured {:url url :opts opts})
                               {:status 200
                                :headers {"content-type" "application/json"}
                                :body {:users [{:id "1" :primaryEmail "alice@example.com"}]}})]
        (let [result (api/invoke users {:op :list
                                        :request {:customer "my_customer"
                                                  :maxResults "10"}})]
          (is (= [{:id "1" :primaryEmail "alice@example.com"}] (:users result)))
          (is (s/includes? (:url @captured) "admin/directory/v1/users"))
          (is (s/includes? (:url @captured) "customer=my_customer"))
          (is (s/includes? (:url @captured) "maxResults=10")))))))

(deftest invoke-get-with-path-params-test
  (testing "interpolates path parameters in URL"
    (let [captured (atom nil)
          client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/get (fn [url opts]
                               (reset! captured {:url url :opts opts})
                               {:status 200
                                :headers {}
                                :body {:id "user123" :primaryEmail "alice@example.com"}})]
        (let [result (api/invoke users {:op :get
                                        :request {:userKey "user123"}})]
          (is (s/includes? (:url @captured) "users/user123"))
          (is (= "user123" (:id result))))))))

(deftest invoke-post-test
  (testing "sends POST request with body"
    (let [captured (atom nil)
          client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/post (fn [url opts]
                                (reset! captured {:url url :opts opts})
                                {:status 200
                                 :headers {}
                                 :body {:id "new-user" :primaryEmail "bob@example.com"}})]
        (let [result (api/invoke users {:op :insert
                                        :body "{\"name\":{\"givenName\":\"Bob\"}}"})]
          (is (s/includes? (:url @captured) "admin/directory/v1/users"))
          (is (= "new-user" (:id result)))
          (is (= :json (:content-type (:opts @captured)))))))))

(deftest invoke-put-test
  (testing "sends PUT request with body and path params"
    (let [captured (atom nil)
          client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/put (fn [url opts]
                               (reset! captured {:url url :opts opts})
                               {:status 200
                                :headers {}
                                :body {:id "user123" :primaryEmail "updated@example.com"}})]
        (let [result (api/invoke users {:op :update
                                        :request {:userKey "user123"}
                                        :body "{\"name\":{\"givenName\":\"Updated\"}}"})]
          (is (s/includes? (:url @captured) "users/user123"))
          (is (= "updated@example.com" (:primaryEmail result))))))))

(deftest invoke-patch-test
  (testing "sends PATCH request with body and path params"
    (let [captured (atom nil)
          client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/patch (fn [url opts]
                                 (reset! captured {:url url :opts opts})
                                 {:status 200
                                  :headers {}
                                  :body {:id "user123" :name {:givenName "Patched"}}})]
        (let [result (api/invoke users {:op :patch
                                        :request {:userKey "user123"}
                                        :body "{\"name\":{\"givenName\":\"Patched\"}}"})]
          (is (s/includes? (:url @captured) "users/user123"))
          (is (= "Patched" (-> result :name :givenName))))))))

(deftest invoke-delete-test
  (testing "sends DELETE request with path params"
    (let [captured (atom nil)
          client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/delete (fn [url opts]
                                  (reset! captured {:url url :opts opts})
                                  {:status 204
                                   :headers {}
                                   :body nil})]
        (let [result (api/invoke users {:op :delete
                                        :request {:userKey "user123"}})]
          (is (s/includes? (:url @captured) "users/user123"))
          ;; DELETE typically returns nil body
          (is (nil? result)))))))

;; --- Error handling tests ---

(deftest invoke-error-handling-test
  (testing "wraps clj-http exceptions with gappy error info"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/get (fn [url opts]
                               (throw (ex-info "clj-http: status 404"
                                               {:status 404
                                                :body {:error {:code 404
                                                               :message "Not Found"
                                                               :errors [{:message "Not Found"
                                                                         :reason "notFound"}]}}})))]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"Not Found"
             (api/invoke users {:op :get
                                :request {:userKey "nonexistent"}})))
        (try
          (api/invoke users {:op :get :request {:userKey "nonexistent"}})
          (catch clojure.lang.ExceptionInfo ex
            (let [data (ex-data ex)]
              (is (true? (:gappy/error data)))
              (is (= 404 (:status data)))
              (is (= 404 (:code data)))
              (is (= "Not Found" (:message data))))))))))

;; --- Pagination tests ---

(deftest invoke-seq-test
  (testing "returns all items across multiple pages"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)
          call-count (atom 0)]
      (with-redefs [http/get (fn [url opts]
                               (swap! call-count inc)
                               (cond
                                 (not (s/includes? url "pageToken"))
                                 {:status 200
                                  :headers {}
                                  :body {:users [{:id "1"} {:id "2"}]
                                         :nextPageToken "token-2"}}

                                 (s/includes? url "pageToken=token-2")
                                 {:status 200
                                  :headers {}
                                  :body {:users [{:id "3"} {:id "4"}]
                                         :nextPageToken "token-3"}}

                                 :else
                                 {:status 200
                                  :headers {}
                                  :body {:users [{:id "5"}]}}))]
        (let [all-users (api/invoke-seq users {:op :list
                                               :request {:customer "my_customer"}
                                               :items-key :users})]
          (is (= [{:id "1"} {:id "2"} {:id "3"} {:id "4"} {:id "5"}]
                 (vec all-users)))
          (is (= 3 @call-count))))))

  (testing "returns empty seq when no items"
    (let [client (h/client-from-fixture :admin :directory_v1)
          users (api/resource client :users)]
      (with-redefs [http/get (fn [url opts]
                               {:status 200
                                :headers {}
                                :body {:users []}})]
        (let [result (api/invoke-seq users {:op :list
                                            :request {:customer "my_customer"}
                                            :items-key :users})]
          (is (empty? result)))))))
