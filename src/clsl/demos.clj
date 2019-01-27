(ns clsl.demos
 (:require [clsl.basic-demos :as basic])
 (:require [clsl.textured-demos :as texture])
 (:require [clsl.obj-demo :as obj])
 (:gen-class))

(def demos
  [basic/demo1 basic/demo2 basic/demo3
   texture/demo (partial texture/demo2 3)
   obj/demo])

(defn all-demos! []
  (doall (map #(%) demos)))

(comment
  "Please call any of the demo-fns."
  ((nth demos 0))
  ((nth demos 1))
  ((nth demos 2))
  ...
  (all-demos!)) 

(defn -main [& args]
  (all-demos!))
