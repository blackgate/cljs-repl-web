(set-env!
 :dependencies '[;; Boot deps
                 [adzerk/boot-cljs            "1.7.228-1" :scope "test"]
                 [pandeiro/boot-http          "0.7.2"     :scope "test"]
                 [adzerk/boot-reload          "0.4.4"     :scope "test"]
                 [degree9/boot-semver         "1.2.4"     :scope "test"]

                 ;; Repl
                 [adzerk/boot-cljs-repl       "0.3.0"  :scope "test"]
                 [com.cemerick/piggieback     "0.2.1"  :scope "test"]
                 [weasel                      "0.7.0"  :scope "test"]
                 [org.clojure/tools.nrepl     "0.2.12" :scope "test"]

                 ;; Tests
                 [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]

                 ;; App deps
                 [org.clojure/clojure         "1.7.0"]
                 [org.clojure/clojurescript   "1.7.228"]
                 [org.clojure/core.async      "0.2.374"]
                 [reagent                     "0.5.1"]
                 [re-frame                    "0.5.0"]
                 [replumb/replumb             "0.1.5-3"]
                 [cljsjs/jqconsole            "2.13.1-0"]
                 [cljsjs/highlight            "8.4-0"]
                 [re-com                      "0.7.0-alpha2"]
                 [cljs-ajax                   "0.5.1"]
                 [hickory                     "0.5.4"]
                 [cljsjs/showdown             "0.4.0-1"]
                 [org.clojure/tools.reader    "1.0.0-alpha3"]
                 [cljsjs/enquire              "2.1.2-0"]
                 [com.cemerick/piggieback     "0.2.1"]
                 [org.clojars.stumitchell/clairvoyant "0.2.0"]
                 [binaryage/devtools          "0.5.2"]
                 [day8/re-frame-tracer        "0.1.0-SNAPSHOT"]
                 [cljsjs/codemirror           "5.10.0-0"]
                 [re-complete                 "0.1.2-1-SNAPSHOT"]])

(def generator-deps '[[org.clojure/clojure         "1.7.0"]
                      [org.clojure/tools.reader    "1.0.0-alpha3"]
                      [endophile                   "0.1.2"]
                      [markdown-clj                "0.9.78"]])

(require '[adzerk.boot-cljs            :refer [cljs]]
         '[adzerk.boot-reload          :refer [reload]]
         '[pandeiro.boot-http          :refer [serve]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs exit!]]
         '[adzerk.boot-cljs-repl       :refer [cljs-repl start-repl]]
         '[boot-semver.core            :refer :all]
         '[boot.pod                    :as pod]
         '[clojure.pprint              :refer [pprint]])

(def +version+ (get-version))

(task-options! pom {:project "cljs-repl-web"
                    :version +version+}
               test-cljs {:js-env :phantom
                          :out-file "phantom-tests.js"})

;;;;;;;;;;;;;;;;;;;;;;
;;;    Options     ;;;
;;;;;;;;;;;;;;;;;;;;;;

(def dev-compiler-options
  {:source-map-timestamp true})

(def prod-compiler-options
  {:closure-defines {"goog.DEBUG" false}
   :optimize-constants true
   :static-fns true
   :elide-asserts true
   :pretty-print false
   :source-map-timestamp true})

(defmulti options
  "Return the correct option map for the build, dispatching on identity"
  identity)

(defmethod options :generator
  [selection]
  {:type :generator
   :env {:source-paths #{"src/clj"}
         :dependencies generator-deps
         :resource-paths #{"dev-resources"}}})

(defmethod options :dev
  [selection]
  {:type :dev
   :env {:source-paths #{"src/clj" "src/cljs" "env/dev/cljs"}
         :resource-paths #{"resources/public/"}}
   :cljs {:source-map true
          :optimizations :none
          :compiler-options dev-compiler-options}
   :test-cljs {:optimizations :none
               :cljs-opts dev-compiler-options
               :suite-ns 'cljs-repl-web.suite}})

(defmethod options :prod
  [selection]
  {:type :prod
   :env {:source-paths #{"src/clj" "src/cljs" "env/prod/cljs"}
         :resource-paths #{"resources/public/"}}
   :cljs {:source-map true
          :optimizations :simple
          :compiler-options prod-compiler-options}
   :test-cljs {:optimizations :simple
               :cljs-opts prod-compiler-options
               :suite-ns 'cljs-repl-web.suite}})

(deftask version-file
  "A task that includes the version.properties file in the fileset."
  []
  (with-pre-wrap [fileset]
    (boot.util/info "Add version.properties...\n")
    (-> fileset
        (add-resource (java.io.File. ".") :include #{#"^version\.properties$"})
        commit!)))

(deftask build
  "Build the final artifact, if no type is passed in, it builds production."
  [t type VAL kw "The build type, either prod or dev"]
  (let [options (options (or type :prod))]
    (boot.util/info "Building %s profile...\n" (:type options))
    (apply set-env! (reduce #(into %2 %1) [] (:env options)))
    (comp (version-file)
          (apply cljs (reduce #(into %2 %1) [] (:cljs options)))
          (target))))

(deftask dev
  "Start the dev interactive environment."
  []
  (boot.util/info "Starting interactive dev...\n")
  (let [options (options :dev)]
    (apply set-env! (reduce #(into %2 %1) [] (:env options)))
    (comp (version-file)
          (serve)
          (watch)
          (cljs-repl)
          (reload :on-jsload 'cljs-repl-web.core/main)
          (apply cljs (reduce #(into %2 %1) [] (:cljs options))))))

;; This prevents a name collision WARNING between the test task and
;; clojure.core/test, a function that nobody really uses or cares
;; about.
(ns-unmap 'boot.user 'test)

(defn test-cljs-opts
  [options namespaces exit?]
  (cond-> options
    namespaces (-> (update-in [:test-cljs :suite-ns] (fn [_] nil))
                   (assoc-in [:test-cljs :namespaces] namespaces))
    exit? (assoc-in [:test-cljs :exit?] exit?)))

(defn set-test-env!
  [options]
  (apply set-env! (reduce #(into %2 %1) [] (update-in (:env options) [:source-paths] conj "test/cljs"))))

(deftask test
  "Run tests once.

   If no type is passed in, it tests against the production build. It
   optionally accepts (a set of) regular expressions that are used for testing
   only some namespaces."
  [t type       VAL        kw       "The build type, either prod or dev"
   n namespace  NAMESPACE  #{regex} "Namespace regex to test against"]
  (let [options (-> (options (or type :prod))
                    (test-cljs-opts namespace true))]
    (boot.util/info "Testing options %s\n" (with-out-str (pprint options)))
    (set-test-env! options)
    (apply test-cljs (reduce #(into %2 %1) [] (:test-cljs options)))))

(deftask auto-test
  "Run tests watching for file changes.

  If no type is passed in, it tests against the production build. It optionally
  accepts (a set of) regular expressions that are used for testing only some
  namespaces."
  [t type      VAL       kw       "The build type, either prod or dev"
   n namespace NAMESPACE #{regex} "Namespace regex to test against"]
  (let [options (-> (options (or type :prod))
                    (test-cljs-opts namespace false))]
    (set-test-env! options)
    (comp (watch)
          (apply test-cljs (reduce #(into %2 %1) [] (:test-cljs options))))))

(deftask cljs-api
  "The task generates the Clojurescript API and the cljs-repl-web.cljs-api
  namespace. It does NOT add it to the fileset, but calls
  cljs-api.generator/-main and dump in src/cljs."
  []
  (with-pass-thru fs
    (boot.util/info "Generating...\n")
    (let [custom-env (:env (options :generator))
          source-paths (:source-paths custom-env)
          resource-paths (:resource-paths custom-env)
          pod-env (assoc-in (get-env) [:dependencies] (:dependencies custom-env))]
      (let [pod (future (pod/make-pod pod-env))]
        (pod/with-eval-in @pod
          (boot.util/dbug "Directories %s\n" (with-out-str (clojure.pprint/pprint boot.pod/env)))
          (doseq [src ~source-paths]
            (boot.pod/add-classpath src))
          (doseq [resource ~resource-paths]
            (boot.pod/add-classpath resource))
          (require 'cljs-api.generator)
          (cljs-api.generator/-main))
        (pod/destroy-pod @pod)))))
