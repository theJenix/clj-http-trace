(ns clj-http-trace.core
  (:require [clj-http-trace.parser :refer [parse-stream]]
            [clj-http-trace.intercept :refer [intercept]]
            [clojure.java.io :as io]
            [clojure.string :as c]
            [clj-http.client :as http]
            [robert.hooke :as hooke]
            [clj-http-trace.utils :refer :all]))

(defn parse-trace-file
  [filename]
  (let [f (.getFile (io/resource filename))]
    (with-open [in (io/input-stream f)]
      (parse-stream in))))

(defn make-intercept-fn [traces]
  (println traces)
  (fn [_ request] 
    (intercept traces request)))

