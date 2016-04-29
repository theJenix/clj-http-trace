(ns clj-http-trace.core
  (:require [clojure.string :as c]
            [clj-http.client :as http]
            [robert.hooke :refer :all]))

(defn <->
  "Calls a function with arguments in reverse order.  If no arguments are supplied, returns a function that will call the original function with the arguments in reversed order.  This is useful for when you want to thread a value through a function using -> or ->> and the function isn't written to take the threaded argument as the first or last argument (respectively)."
  ([f]
    (fn[& args]
      (apply <-> f args)))
  ([f & args]
    (->> (reverse args)
         (apply f))))


; Current EOF, current trace, traces
(def parser-init [nil {} []])

(defn conv [v i]
  (conj (if (vector? v) v (if v (vec v) [])) i))

(defn parse-key-value [[curr-fn trace traces] line & ks]
  (let [[kstr v] (c/split line #":" 2)
        k (keyword kstr)
        newtrace (-> trace
                     (assoc-in  (conv ks k) (c/trim v))
                     (update-in (conv ks "FieldOrder") conv k))]
;    (println line ks k v)
    [curr-fn newtrace traces]))

(defn parse-method
  "Parses the Method: element, which starts every trace"
  [[_ trace traces] line]
  (parse-key-value [nil {} (if trace (conj traces trace) traces)] line))

(defn make-header-parse-fn [end k]
  (fn [s l]
    (let [[c t ts] s]
      (if (= l end)
        [nil t ts]
        (if-not (.contains l ":")
          [c (assoc-in t [k :First-Line] l) ts]
          (parse-key-value s l k))))))

(defn make-body-parse-fn [end k]
  (fn [s l]
    (let [[c t ts] s]
      (if (= l end)
        (if (= k :Response-Body)
          [nil nil (conv ts t)]
          [nil t ts])
        (update-in s [1 k] conv l)))))

(defn parse-request-header [[_ trace traces] line]
  (println "In parse-request-header")
  (let [[k v] (c/split line #":<<" 2)
        new-fn (make-header-parse-fn v :Request-Header)]
    [new-fn trace traces]))
  
(defn parse-response-header [[_ trace traces] line]
  (println "In parse-response-header")
  (let [[k v] (c/split line #":<<" 2)
        new-fn (make-header-parse-fn v :Response-Header)]
    [new-fn trace traces]))

(defn parse-response-body [[_ trace traces] line]
  (println "In parse-response-body")
  (let [[k v] (c/split line #":<<" 2)
        new-fn (make-body-parse-fn v :Response-Body)]
    [new-fn trace traces]))

(defn def-parse-fn [state line]
  (condp (<-> c/starts-with?) line
    "Method:"          (parse-method state line)
    "Request-Header:"  (parse-request-header state line)
    "Response-Header:" (parse-response-header state line)
    "Response-Body:"   (parse-response-body state line)
    (parse-key-value state line)))

(defn parse-line [state line]
  (if (c/blank? line)
    state
    (let [[curr-fn trace traces] state]
      (if curr-fn
        (curr-fn state line)
        (def-parse-fn state line)))))

(defn parse-trace-file
  "Parses a trace file and returns a vector of request/response maps"
  [filename]
  (let [f (.getFile (clojure.java.io/resource filename))
        txt (-> (slurp f)
                (c/split #"\r?\n"))
        _ (println (last txt))
        traces (reduce parse-line parser-init txt)]
    (last traces)))


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
           :headers (assoc (:Response-Header match) "content-encoding" "gzip")
           :body (body-bytes (c/join "" (apply concat (:Response-Body match))))})

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
