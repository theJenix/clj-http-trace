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

(defn bytes-to-string [buf]
  (String. (byte-array buf)))

(defn body-to-bytes [body]
  (if body
    (with-open [o (java.io.ByteArrayOutputStream.)]
      (.writeTo body o)
      (.toByteArray o))
    []))

(defn replace-plus-with-pct20 [s]
  (c/replace s #"\+" "%20"))

;; A "safe string" is one that we can compare, e.g. all spaces are %20, and it's trimmed
(def bytes-to-safe-string (comp 
                            replace-plus-with-pct20
                            c/trim
                            bytes-to-string))
;; TODO: we should parse the request body into param parts and compare the parts.  This is because they can show up out of order in the request
(defn intercept [traces {:keys [server-name query-string uri body] :as request}]
  (let [method (-> (:request-method request)
                   name
                   c/upper-case)
        body-str (-> body
                     body-to-bytes
                     bytes-to-safe-string)
        uri-to-match (if query-string (str uri "?" query-string) uri)
        _ (log/info "In intercept" (:request-method request) (if query-string (str uri "?" query-string) uri) body-str)
        _ (log/info request)
        matches (->> traces
                     (filter (comp #{method} :Method))
                     (filter (comp #{server-name} :Host))
                     (filter (comp #{uri-to-match} :File))
                     (filter (comp #{body-str} bytes-to-safe-string :Request-Body)))]
    (if-let [match (first matches)]
      (let [[httpver statuscode statustxt] (c/split (:First-Line (:Response-Header match)) #" ")]
        (log/trace statuscode)
        (log/trace match)

        {:status (Integer/parseInt statuscode)
         :headers (->> (:Response-Header match) 
                       (map-keys (comp c/lower-case name)))
         :body (body-bytes (-> (:Response-Body match)
                               byte-array))})

      {:status 404 :headers {} :body nil}))
  )
