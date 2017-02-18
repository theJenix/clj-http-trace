(ns clj-http-trace.core-test
  (:require [clojure.test :refer :all]
            [clj-http-trace.core :refer :all]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [robert.hooke :as hooke]))

(defn- install-trace-intercept! [filename]
    (->> (parse-trace-file filename) 
         make-intercept-fn
         (hooke/add-hook #'clj-http.core/request)))

(deftest trace-test
  (testing "Test parsing and simple get."
    (install-trace-intercept! "test.trace")
    (let [actual (-> (http/get "http://www.mcmaster.com/mv1460752213/WebParts/SrchRsltWebPart/WebSrchEng.aspx?inpArgTxt=5986K66")
                     :body
                     (json/parse-string true))
          actual-first (first actual)]
      (is (= 2 (count actual)))
      (is (= "McMPartNbr" (get actual-first :EntityTyp)))
      (is (= "5986K66"    (get-in actual-first [:WebSrchEngMetaDat :PartNbr]))))))
