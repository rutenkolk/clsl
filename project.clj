(require 'leiningen.core.eval)

;; per-os jvm-opts code cribbed from Overtone
(def JVM-OPTS
  {:common  ["-Djdk.attach.allowAttachSelf" "-XX:+UnlockDiagnosticVMOptions" "-XX:+DebugNonSafepoints"]
  :macosx   ["-XstartOnFirstThread" "-Djava.awt.headless=true"]
  :linux    []
  :windows  []})

(defn jvm-opts "Return a complete vector of jvm-opts for the current os."
  [] (let [os (leiningen.core.eval/get-os)] 
       (vec (set (concat (get JVM-OPTS :common) 
                         (get JVM-OPTS os))))))

(def LWJGL_NS "org.lwjgl")

(def LWJGL_VERSION "3.1.6")
(def jomlVersion "1.9.9")

;; Edit this to add/remove packages.
(def LWJGL_MODULES ["lwjgl"
                    "lwjgl-assimp"
                    "lwjgl-bgfx"
                    "lwjgl-egl"
                    "lwjgl-glfw"
                    "lwjgl-jawt"
                    "lwjgl-jemalloc"
                    "lwjgl-lmdb"
                    "lwjgl-lz4"
                    "lwjgl-nanovg"
                    "lwjgl-nfd"
                    "lwjgl-nuklear"
                    "lwjgl-odbc"
                    "lwjgl-openal"
                    "lwjgl-opencl"
                    "lwjgl-opengl"
                    "lwjgl-opengles"
                    "lwjgl-openvr"
                    "lwjgl-par"
                    "lwjgl-remotery"
                    "lwjgl-rpmalloc"
                    "lwjgl-sse"
                    "lwjgl-stb"
                    "lwjgl-tinyexr"
                    "lwjgl-tinyfd"
                    "lwjgl-tootle"
                    "lwjgl-vulkan"
                    "lwjgl-xxhash"
                    "lwjgl-yoga"
                    "lwjgl-zstd"])

;; It's safe to just include all native dependencies, but you might
;; save some space if you know you don't need some platform.
(def LWJGL_PLATFORMS  ["linux" "macos" "windows"])

;; These packages don't have any associated native ones.
(def no-natives? #{"lwjgl-egl" "lwjgl-jawt" "lwjgl-odbc" "lwjgl-opencl" "lwjgl-vulkan"})

(defn lwjgl-deps-with-natives  []
  (apply concat
         (for [m LWJGL_MODULES]
           (let [prefix [(symbol LWJGL_NS m) LWJGL_VERSION]]
             (into [prefix]
               (if (no-natives? m)
                 []
                 (for [p LWJGL_PLATFORMS]
                   (into prefix  [:classifier  (str "natives-" p)
                                  :native-prefix ""]))))))))

(def all-dependencies
  (into
   '[[org.clojure/clojure "1.8.0"]
     [org.reflections/reflections "0.9.11"]
     [org.joml/joml "1.9.9"]
     [com.github.kotlin-graphics/glm "0.01"]
     [com.clojure-goes-fast/clj-async-profiler "0.3.0"]
     [org.clojure/core.async "0.4.490"]]
    (lwjgl-deps-with-natives)))

(defproject clsl "0.2.1"
  :description "LWJGL3 based clojure graphics library"
  :url "https://github.com/rutenkolkc/clsl"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :repositories  [["jitpack" "https://jitpack.io"]]

  :dependencies ~all-dependencies
  :min-lein-version "2.1.0"
  :jvm-opts ^:replace ~(jvm-opts)
  :main clsl.demos
  ;:aot [clsl.core clsl.demos]
  )
