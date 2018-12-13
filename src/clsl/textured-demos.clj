(ns clsl.textured-demos
 (:require [clsl.core :as c])
 (:gen-class))

;This is what texture support will probably look like.
;It has bugs and doesn't work right now. Expect NullPointerExceptions.

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

(def uv-coordinates
  [0 0
   0 1
   1 1])

(def interleaved-data-2
  (vec (flatten (interleave 
                  (partition 4 4 positions)
                  (partition 4 4 colors)
                  (partition 2 2 uv-coordinates)))))

(def texture-vert-shader
  (c/vertex-shader [pos texture_coords trans_mat color]
    [(c/mul trans_mat pos)
     (c/typed :vec4 color)
     (c/typed :vec2 texture_coords)]))

(def texture-frag-shader
  (c/fragment-shader [color sampler uv factor]
    (c/add (c/mul factor (c/sample sampler uv))
           (c/mul (c/sub 1 factor) color))))

(def texture-render-pipeline
  (let [vert-out (c/shader-output demo-vert-shader)] 
    (c/simple-pipeline [pos cool_texture uv_coords color mvp blend]
      [(c/prime-shader 
         texture-vert-shader pos uv_coords mvp color) 
       (c/prime-shader 
         texture-frag-shader (first vert-out) cool_texture (second vert-out) blend)])))

(def texture-triangle-drawer
  (c/drawer [tr-buf [:objs :tr-buf]
             my-texture [:objs :tex-id]
             tr-buf-count [:objs :tr-buf-count]
             t [:time]]
    texture-render-pipeline 
    [(c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4 :vec2) 0)
     (c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4 :vec2) (c/size-of-type :vec4))
     (c/texture-2d-take my-texture)
     (c/buf-take tr-buf :vec2 (c/size-of-type :vec4 :vec4 :vec2) (c/size-of-type :vec4 :vec4))
     (let [adjusted_t (* t 0.001)] 
       (glm.mat4x4.Mat4.
        (*  2.0 (Math/cos adjusted_t))  (* 1.0 (Math/sin adjusted_t)) 0.0 0.0
        (* -1.0 (Math/sin adjusted_t))  (* 2.0 (Math/cos adjusted_t)) 0.0 0.0
        0.0                             0.0                           1.0 0.0
        0.0                             0.0                           0.0 1.0))
     (Math/abs (Math/sin t))]
    (c/drawarrays :triangles 0 tr-buf-count)))

(defn my-state-init-fn [state]
  (assoc state
         :objs {:tr-buf (c/buf (c/load-value-to-array interleaved-data-2))
                :tex-id (c/texture-2d "/home/wiredaemon/Pictures/test.png")
                :tr-buf-count 3}
         :start-time (System/currentTimeMillis)
         :time 0)) 

(defn demo []
  (c/add-drawer! texture-triangle-drawer)
  (c/add-update-fn! (fn [state] (assoc state :time (- (System/currentTimeMillis) (:start-time state)))))
  (c/start! my-state-init-fn)
  (c/reset-global-state!)
  "Demo completed. Global State has been reset!")

(comment 
  "evaluating the expression below may show you textured triangles" 
  (demo)
  )
