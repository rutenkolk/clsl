(ns clsl.textured-demos
 (:require [clsl.core :as c])
 (:gen-class))

;This is what texture support will probably look like.
;It has bugs and doesn't work right now. Expect NullPointerExceptions.

(def positions
  ;X    Y    Z    W
  [0.5  0.5  0.0  1.0   ; 1. Vertex
  -0.5 -0.5  0.0  1.0   ; 2. Vertex
   0.5 -0.5  0.0  1.0   ; 3. Vertex 
   
   0.5  0.5  0.0  1.0   ; 1. Vertex
  -0.5 -0.5  0.0  1.0   ; 2. Vertex
  -0.5  0.5  0.0  1.0   ; 3. Vertex 
   ]) 
  
(def colors
  ;R   G   B   A
  [1.0 0.0 0.0 1.0
   0.0 1.0 0.0 1.0
   0.0 0.0 1.0 1.0
   
   1.0 0.0 0.0 1.0
   0.0 1.0 0.0 1.0
   0.0 0.0 1.0 1.0])

(def uv-coordinates
  [1 1
   0 0
   1 0
   
   1 1
   0 0
   0 1
   ])

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
  (c/fragment-shader [color texture1 uv factor]
    (c/add (c/mul factor (c/sample texture1 uv))
           (c/mul (c/sub 1.0 factor) color))))

(def texture-render-pipeline
  (let [vert-out (c/shader-output texture-vert-shader)]
    (c/simple-pipeline [pos cool_texture uv_coords color mvp blend]
      [(c/prime-shader 
         texture-vert-shader pos uv_coords mvp color) 
       (c/prime-shader 
         texture-frag-shader (first vert-out) cool_texture (second vert-out) blend)])))

(defn create-texture-quad-drawer [texture-id-lookup [offx offy]]
  (c/drawer [tr-buf [:objs :tr-buf]
             my-texture [:objs texture-id-lookup]
             tr-buf-count [:objs :tr-buf-count]
             t [:time]]
    texture-render-pipeline 
    [(c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4 :vec2) 0)
     (c/texture-2d-take my-texture)
     (c/buf-take tr-buf :vec2 (c/size-of-type :vec4 :vec4 :vec2) (c/size-of-type :vec4 :vec4))
     (c/buf-take tr-buf :vec4 (c/size-of-type :vec4 :vec4 :vec2) (c/size-of-type :vec4))
     (let [adjusted_t (* t 0.001)] 
       (.times
         (.rotate glm.glm/INSTANCE (glm.mat4x4.Mat4. 1) adjusted_t (glm.vec3.Vec3. 0 0 -1))
         (.translate glm.glm/INSTANCE (glm.mat4x4.Mat4. 1) (glm.vec3.Vec3. offx offy 0))))
     (+ 0.5 (/ (Math/sin (* t 0.001)) 2))]
    (c/drawarrays :triangles 0 tr-buf-count)))

(defn my-state-init-fn [state]
  (assoc state
         :objs {:tr-buf (c/buf (c/load-value-to-array interleaved-data-2))
                :tex-id-1 (c/texture-2d "res/birb1.jpg")
                :tex-id-2 (c/texture-2d "res/birb2.jpg")
                :tr-buf-count 6}
         :start-time (System/currentTimeMillis)
         :time 0)) 

(defn demo []
  (c/add-drawer! (create-texture-quad-drawer :tex-id-1 [-0.5 0.0]))
  (c/add-drawer! (create-texture-quad-drawer :tex-id-2 [0.5 0.0]))
  (c/add-update-fn! (fn [state] (assoc state :time (- (System/currentTimeMillis) (:start-time state)))))
  (c/start! my-state-init-fn)
  (c/reset-global-state!)
  "Demo completed. Global State has been reset!")

(comment 
  "evaluating the expression below may show you textured triangles" 
  (demo))
