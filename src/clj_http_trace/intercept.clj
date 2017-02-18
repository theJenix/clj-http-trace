(ns clj-http-trace.intercept
  (:require [clojure.string :as c]
            [clojure.tools.logging :as log]
            [clj-http-trace.utils :refer :all]))

(defn utf8-bytes
    "Returns the UTF-8 bytes corresponding to the given string."
    [^String s]
    (.getBytes s "UTF-8"))

(defn- byte-array?
  "Is `obj` a java byte array?"
  [obj]
  (let [byte-array-type (Class/forName "[B")]
    (instance? byte-array-type obj)))

(defn body-bytes
  "If `obj` is a byte-array, return it, otherwise use `utf8-bytes`."
  [obj]
  (if (byte-array? obj)
    obj
    (utf8-bytes obj)))

(defn intercept [traces {:keys [server-name query-string uri] :as request}]
  (log/info "In intercept" (:request-method request) (str uri "?" query-string))
  (log/debug request)
  (let [method (-> (:request-method request)
                   name
                   c/upper-case)
        matches (->> traces
                     (filter (comp #{method} :Method))
                     (filter (comp #{server-name} :Host))
                     (filter (comp #{(str uri "?" query-string)} :File)))]
    (if-let [match (first matches)]
      (let [[httpver statuscode statustxt] (c/split (:First-Line (:Response-Header match)) #" ")]
        (log/trace statuscode)
        {:status (Integer/parseInt statuscode)
         :headers (->> (:Response-Header match) 
                       (map-keys (comp c/lower-case name)))
         :body (body-bytes (-> (:Response-Body match)
                               byte-array))})

      {:status 404 :headers {} :body nil}))
  )
