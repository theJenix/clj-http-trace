(ns clj-http-trace.core
  (:require [clojure.string :as c]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [robert.hooke :refer :all]
            [clj-http-trace.utils :refer :all]))


; Byte buffer, current EOF, current trace, traces
(def parser-init [[] nil {} []])

(defn try-consume-line [[buf c t ts] b]
  (if (= b (byte \newline))
    (let [line (String. (byte-array buf))]
      [line [[] c t ts]])
   [nil [(conv buf b) c t ts]]))

(defn parse-key-value [[buf curr-fn trace traces] line & ks]
  (let [[kstr v] (c/split line #":" 2)
        k (keyword kstr)
        newtrace (-> trace
                     (assoc-in  (conv ks k) (c/trim v))
                     (update-in (conv ks :FieldOrder) conv k))]
;    (println line ks k v)
    [buf curr-fn newtrace traces]))

(defn parse-method
  "Parses the Method: element, which starts every trace"
  [[buf _ trace traces] line]
  (parse-key-value [buf nil {} (if trace (conj traces trace) traces)] line))

(defn make-header-parse-fn [end k]
  (fn [s b]
    (let [[l nst] (try-consume-line s b)]
      (if (c/blank? l)
        nst
        (let [[buf c t ts] nst]
          (if (= l end)
            [buf nil t ts]
            (if-not (.contains l ":")
              (assoc-in s [2 k :First-Line] l)
              (parse-key-value nst l k))))))))

(defn make-body-parse-fn [end k]
  (let [endbytes (-> (.getBytes end) ;; TODO: assumes end is a string...probably want to make this more generic
                     vec)
        endm     (atom (init-matcher endbytes))] ; TODO: this Atom makes this non-pure...would prefer to push this into the state, but its ok for now
    (fn [s b]
      (let [[buf c t ts] s]
        (if (matches? b @endm) 
          (do
            (swap! endm shift-one)
            (if (all-matched? @endm)
              [buf nil nil (conv ts t)]
              s))
          (let [toappend (conv (get-matched @endm) b)]
            (swap! endm reset-matcher)
            (update-in s [2 k] concatv toappend)))))))

(defn parse-request-header [state line]
  (println "In parse-request-header")
  (let [[k v] (c/split line #":<<" 2)
        new-fn (make-header-parse-fn v :Request-Header)]
    (assoc state 1 new-fn)))
  
(defn parse-response-header [state line]
  (println "In parse-response-header")
  (let [[k v] (c/split line #":<<" 2)
        new-fn (make-header-parse-fn v :Response-Header)]
    (assoc state 1 new-fn)))

(defn parse-response-body [state line]
  (println "In parse-response-body")
  (let [[k v] (c/split line #":<<" 2)
        new-fn (make-body-parse-fn v :Response-Body)]
    (assoc state 1 new-fn)))

(defn def-parse-fn [state b]
  (let [[line newstate] (try-consume-line state b)]
    (if (c/blank? line)
      newstate
      (condp (<-> c/starts-with?) line
        "Method:"          (parse-method newstate line)
        "Request-Header:"  (parse-request-header newstate line)
        "Response-Header:" (parse-response-header newstate line)
        "Response-Body:"   (parse-response-body newstate line)
        (parse-key-value newstate line)))))

(defn consume-new-byte
  [state b]
  (if-let [curr-fn (second state)]
    (curr-fn state b)
    (def-parse-fn state b)))

(defn read-byte-or-default
  [term in]
  (let [b (.read in)
        finished (neg? b)]
    [b finished]))

(defn parse-trace-file
  [filename]
  (let [f (.getFile (io/resource filename))]
    (with-open [in (io/input-stream f)]
      (loop [state parser-init]
        (let [[b finished] (read-byte-or-default (byte \newline) in)
              newstate (consume-new-byte state b)]
          (if finished
            (last newstate)
            (recur newstate)))))))

(defn utf8-bytes
    "Returns the UTF-8 bytes corresponding to the given string."
    [^String s]
    (.getBytes s "UTF-8"))


(let [byte-array-type (Class/forName "[B")]
  (defn- byte-array?
    "Is `obj` a java byte array?"
    [obj]
    (instance? byte-array-type obj)))

(defn body-bytes
  "If `obj` is a byte-array, return it, otherwise use `utf8-bytes`."
  [obj]
  (if (byte-array? obj)
    obj
    (utf8-bytes obj)))

(defn make-intercept-fn [traces]
  (println traces)
  (fn [origfn {:keys [server-name query-string uri] :as request}]
    (println request)
    (println (str uri "?" query-string))
    (let [matches (->> traces
                       (filter (comp #{"GET"} :Method))
                       (filter (comp #{server-name} :Host))
                       (filter (comp #{(str uri "?" query-string)} :File)))]
      (if-let [match (first matches)]
        (let [[httpver statuscode statustxt] (c/split (:First-Line (:Response-Header match)) #" ")]
          (println statuscode)
          {:status (Integer/parseInt statuscode)
           :headers (->> (:Response-Header match) 
                         (map-keys (comp c/lower-case name)))
           :body (body-bytes (-> (:Response-Body match)
                                 byte-array))})

        {:status 404 :headers {} :body nil}))
    ))

(defn -main
  ""
  [& args]
  (let [traces (parse-trace-file "test.trace")]
    (add-hook #'clj-http.core/request (make-intercept-fn traces))
    (-> (http/get "http://www.mcmaster.com/mv1460752213/WebParts/SrchRsltWebPart/WebSrchEng.aspx?inpArgTxt=5986K66")
        :body
        println)
    ))
