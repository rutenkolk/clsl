(ns clsl.demos
 (:require [clsl.basic-demos :as basic])
 (:require [clsl.textured-demos :as texture])
 (:require [clsl.obj-demo :as obj])
 (:require [clsl.obj-demo-fast :as obj-fast])
 (:require [clj-async-profiler.core :as prof])
 (:gen-class))

(def demos
  [basic/demo1 basic/demo2 basic/demo3
   texture/demo (partial texture/demo2 3)
   obj/demo obj-fast/demo])

(defn all-demos! []
  (doall (map #(%) demos)))

(comment
  (double (* 1000 (/ 200)))
  "Please call any of the demo-fns."
  (do
    (prof/start {:threads true})
    (obj/demo)
    (prof/stop))
  ((nth demos 0))
  ((nth demos 1))
  ((nth demos 2))
  ...
  (clsl.core/stop&reset!)
  (texture/demo)
  (all-demos!)) 

(defn -main [& args]
  (all-demos!))
