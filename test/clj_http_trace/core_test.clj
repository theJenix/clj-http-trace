(ns clj-http-trace.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as c]
            [clj-http-trace.core :refer :all]
            [clj-http.client :as http]
            [cheshire.core :as json]))

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

(deftest post-test
  (testing "Test parsing, get, and post."
    (install-trace-intercept! "test-post.trace")
    (let [expected "<a href=\"http://pelosi.house.gov/\">Nancy Pelosi </a>"
          actual (-> (http/post "http://ziplook.house.gov/htbin/findrep?ADDRLK89373111089373111" {:form-params {:street "5290 diamond heights blvd" :city "san francisco" :state "CACalifornia" :Submit "FIND YOUR REP BY ADDRESS" :ZIP 94131}})
                     :body)]
      (is (c/includes? actual expected)))))
