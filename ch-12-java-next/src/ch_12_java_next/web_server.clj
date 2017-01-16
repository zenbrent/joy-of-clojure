(ns ch-12-java-next.web-server
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.sun.net.httpserver HttpHandler HttpExchange HttpServer]
           [java.net InetSocketAddress URLDecoder URI]
           [java.io File FilterOutputStream]))

(def OK java.net.HttpURLConnection/HTTP_OK)

(defn respond
  ([exchange body]
   (respond identity exchange body))
  ([around exchange body]
   (.sendResponseHeaders exchange OK 0)
   (with-open [resp (around (.getResponseBody exchange))]
     (.write resp (.getBytes body)))))

(defn new-server [port path handler]
  (doto
    (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler [txt]
  (proxy [HttpHandler] ;; extend
    []
    (handle [exchange]
      (respond exchange txt))))

(defonce p (default-handler "There's no problem that can't be sovled with another level of indirection."))

(defonce server (new-server 8123 "/" p))

(update-proxy p
  {"handle" (fn [this exchange]
              (respond exchange (str "this is: " this)))})

(def echo-handler
  (fn [_ exchange]
    (let [headers (.getRequestHeaders exchange)]
      (respond exchange (prn-str headers)))))

(update-proxy p {"handle" echo-handler})

(def proxy-mappings-handler
  (fn [this exchange]
    (respond exchange (prn-str (proxy-mappings this)))))

(update-proxy p {"handle" proxy-mappings-handler})

(defn html-around [o]
  (proxy [FilterOutputStream]
    [o] ;; pass wrapped val to ctor
    (write [raw-bytes]
      (proxy-super write
                   (.getBytes (str "<html><body>"
                                   (String. raw-bytes)
                                   "</body></html>")))))) 
(defn listing [file]
  (-> file .list sort))

;; tests
(listing (io/file "."))
(listing (io/file "./README.md"))

(defn html-links [root filenames]
  (string/join
    (for [file filenames]
      (str "<a href=\""
           (str root
                (if (= "/" root) "" File/separator)
                file)
           "\">"
           file
           "</a><br />\n")))) 

;; tests
(html-links "." (listing (io/file ".")))

(defn details [file]
  (str \` (.getName file) "` is "
       (.length file) " bytes."))

(defn uri->file [root uri]
  (->> uri
       str
       URLDecoder/decode
       (str root)
       io/file))

;; tests
(details (io/file "./README.md"))
(uri->file "." (URI. "/project.clj"))
(details (uri->file "." (URI. "/project.clj")))

(def fs-handler
  (fn [_ exchange]
    (let [uri (.getRequestURI exchange)
          file (uri->file "." uri)]
      (if (.isDirectory file)
        (do (.add (.getResponseHeaders exchange)
                  "Content-Type" "html")
            (respond html-around
                     exchange
                     (html-links (str uri) (listing file))))
        (respond exchange (details file))))))

(update-proxy p {"handle" fs-handler})

