(ns clsl.demos
 (:require [clsl.core :as c])
 (:gen-class))

(def positions
  ;X    Y    Z    W
  [0.0  0.5  0.0  1.0   ; 1. Vertex
  -0.5 -0.5  0.0  1.0   ; 2. Vertex
   0.5 -0.5  0.0  1.0]) ; 3. Vertex 

(def colors
  ;R   G   B   A
  [1.0 0.0 0.0 1.0
   0.0 1.0 0.0 1.0
   0.0 0.0 1.0 1.0])

(def interleaved-data
  (vec 
    (flatten 
      (interleave (partition 4 4 positions)
                  (partition 4 4 colors)))))

(def demo-vert-shader
  (c/vertex-shader [pos trans_mat color]
    [(c/mul trans_mat pos)
     (c/typed :vec4 color)]))

(def demo-frag-shader
  (c/fragment-shader [color factor]
    (c/mul color factor)))

(def demo-render-pipeline
  (let [vert-out (c/shader-output demo-vert-shader)] 
    (c/simple-pipeline [pos color mvp light_factor]
      [(c/prime-shader demo-vert-shader pos mvp color) 
       (c/prime-shader demo-frag-shader (first vert-out) light_factor)])))

(def demo-triangle-drawer
  (c/drawer [tr-buf [:objs :tr-buf]
             tr-buf-count [:objs :tr-buf-count]
             t [:time]]
    demo-render-pipeline
    [(c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4) 0)
     (c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4) (c/size-of-type :vec4))
     (let [adjusted_t (* t 0.001)] 
       (glm.mat4x4.Mat4.
        (*  2.0 (Math/cos adjusted_t))  (* 1.0 (Math/sin adjusted_t)) 0.0 0.0
        (* -1.0 (Math/sin adjusted_t))  (* 2.0 (Math/cos adjusted_t)) 0.0 0.0
        0.0                             0.0                           1.0 0.0
        0.0                             0.0                           0.0 1.0))
     1]
    (c/drawarrays :triangles 0 tr-buf-count)))

(defn my-state-init-fn [state]
  (assoc state
         :objs {:tr-buf (c/buf (c/load-value-to-array interleaved-data))
                :tr-buf-count 3}
         :start-time (System/currentTimeMillis)
         :time 0)) 

(defn demo1 []
  (c/add-drawer! demo-triangle-drawer)
  (c/add-update-fn! (fn [state] (assoc state :time (- (System/currentTimeMillis) (:start-time state)))))
  (c/start! my-state-init-fn)
  (c/reset-global-state!)
  "Demo 1 completed. Global State has been reset!")

(defn demo2-fill-in [tr-buf tr-buf-count t]
  [(c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4) 0)
   (c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4) (c/size-of-type :vec4))
   (let [adjusted_t (* t -0.001)] 
     (glm.mat4x4.Mat4.
      (*  2.0 (Math/cos adjusted_t))  (* 1.0 (Math/sin adjusted_t)) 0.0 0.0
      (* -1.0 (Math/sin adjusted_t))  (* 2.0 (Math/cos adjusted_t)) 0.0 0.0
      0.0                             0.0                           1.0 0.0
      0.0                             0.0                           0.0 1.0))
   (Math/abs (Math/sin (* t 0.0001)))])

(def demo2-drawer 
  (c/drawer [tr-buf [:objs :tr-buf]
             tr-buf-count [:objs :tr-buf-count]
             t [:time]]
    demo-render-pipeline
    (demo2-fill-in tr-buf tr-buf-count t)
    (c/drawarrays :triangles 0 tr-buf-count)))

(defn demo2 []
  (c/add-drawer! demo2-drawer)
  (c/add-update-fn! (fn [state] (assoc state :time (- (System/currentTimeMillis) (:start-time state)))))
  (c/start! my-state-init-fn)
  (c/reset-global-state!)
  "Demo 2 completed. Global State has been reset!")

(defn demo3 []
  (c/add-drawer! demo-triangle-drawer)
  (c/add-drawer! demo2-drawer)
  (c/add-update-fn! (fn [state] (assoc state :time (- (System/currentTimeMillis) (:start-time state)))))
  (c/start! my-state-init-fn)
  (c/reset-global-state!)
  "Demo 3 completed. Global State has been reset!")

(comment
  "Please call any of the demo-fns. Simply eval the exprs below :)"
  (demo1)
  (demo2)
  (demo3))

(defn -main [& args]
  (demo3))
