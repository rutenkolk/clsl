(ns clsl.obj-demo
 (:require [clsl.core :as c])
 (:gen-class))

;recreation of the lwjgl3 assimp obj-renderer demo:
;https://github.com/LWJGL/lwjgl3-demos/blob/master/src/org/lwjgl/demo/opengl/assimp/WavefrontObjDemo.java

(defn create-meshes [ai-meshes]
  (doall 
  (for [mesh ai-meshes] 
    (let [vertices-buf (c/buf 
                         (* org.lwjgl.assimp.AIVector3D/SIZEOF (.remaining (.mVertices mesh))) 
                         (.address (.mVertices mesh)))
          normals-buf (c/buf 
                        (* org.lwjgl.assimp.AIVector3D/SIZEOF (.remaining (.mNormals mesh))) 
                        (.address (.mNormals mesh)))
          elem-count (* 3 (.mNumFaces mesh))
          faces (.mFaces mesh)
          elems (doall (map #(.mIndices (.get faces %)) (range (.mNumFaces mesh))))
          elems-host-buf (org.lwjgl.BufferUtils/createIntBuffer elem-count)
          _ (doall (map #(.put elems-host-buf %) elems))
          _ (.flip elems-host-buf)
          elem-buf (c/buf elems-host-buf)]
      {:vertices-buf vertices-buf
       :normals-buf normals-buf
       :element-buf elem-buf
       :material-index (.mMaterialIndex mesh)
       :elem-count elem-count}))))

(defn create-materials [ai-materials]
  (doall
    (for [material ai-materials] 
    (let 
      [ambient-color (org.lwjgl.assimp.AIColor4D/create)
       _ (org.lwjgl.assimp.Assimp/aiGetMaterialColor 
           material 
           org.lwjgl.assimp.Assimp/AI_MATKEY_COLOR_AMBIENT 
           org.lwjgl.assimp.Assimp/aiTextureType_NONE 
           0 
           ambient-color)
       diffuse-color (org.lwjgl.assimp.AIColor4D/create)
       _ (org.lwjgl.assimp.Assimp/aiGetMaterialColor 
           material
           org.lwjgl.assimp.Assimp/AI_MATKEY_COLOR_DIFFUSE
           org.lwjgl.assimp.Assimp/aiTextureType_NONE
           0
           diffuse-color)
       specular-color (org.lwjgl.assimp.AIColor4D/create)
       _ (org.lwjgl.assimp.Assimp/aiGetMaterialColor 
           material
           org.lwjgl.assimp.Assimp/AI_MATKEY_COLOR_SPECULAR
           org.lwjgl.assimp.Assimp/aiTextureType_NONE
           0
           specular-color)]
      ;ignore alpha for the moment (the lwjgl demo does that too) 
      {:ambient-color  [(.r ambient-color) (.g ambient-color) (.b ambient-color)] 
       :diffuse-color  [(.r diffuse-color) (.g diffuse-color) (.b diffuse-color)]
       :specular-color [(.r specular-color) (.g specular-color) (.b specular-color)]}))))

(defn load-obj-model [path]
  (let [path "res/magnet.obj"
        scene (org.lwjgl.assimp.Assimp/aiImportFile 
                path 
                (bit-or org.lwjgl.assimp.Assimp/aiProcess_JoinIdenticalVertices 
                        org.lwjgl.assimp.Assimp/aiProcess_Triangulate))
        _ (println "Assimp error String:" (org.lwjgl.assimp.Assimp/aiGetErrorString))
        meshcount (.mNumMeshes scene)
        _ (println "meshcount:" meshcount)
        ai-meshes (doall (map 
                           #(org.lwjgl.assimp.AIMesh/create (.get (.mMeshes scene) %)) 
                           (range meshcount)))
        _ (println "ai-meshes read in!")
        meshes (create-meshes ai-meshes)
        _ (println "meshes created!")
        _ (println "materialcount" (.mNumMaterials scene))
        raw-materials (map #(.get (.mMaterials scene) %) (range (.mNumMaterials scene)))
        _ (clojure.pprint/pprint raw-materials)
        ai-materials (doall (map 
                              #(org.lwjgl.assimp.AIMaterial/createSafe 
                                 (.get (.mMaterials scene) %)) 
                              (range (.mNumMaterials scene))))
        _ (println "ai-materials read in!")
        materials (create-materials ai-materials)
        
        _ (println "loading model finished.")
        _ (println "Assimp error String:" (org.lwjgl.assimp.Assimp/aiGetErrorString))
        _ (org.lwjgl.assimp.Assimp/aiReleaseImport scene)
        ]
    {:meshes meshes
     :materials materials}))

; --- SHADERS ---

(def obj-vert-shader
  (c/vertex-shader [aVertex aNormal uModelMatrix uViewProjectionMatrix uNormalMatrix]
    [(c/mul uViewProjectionMatrix uModelMatrix (c/vec4 aVertex 1))
     (c/typed :vec3 (c/swizzle (c/mul uModelMatrix (c/vec4 aVertex 1)) :xyz))
     (c/typed :vec3 (c/mul uNormalMatrix aNormal))]))

(def obj-frag-shader
  (c/fragment-shader [uLightPosition uViewPosition uAmbientColor
                      uDiffuseColor uSpecularColor vPosition vNormal]
    (let [ambientStrength  0.5
          diffuseStrength  0.5
          specularStrength 0.5
          shininess        4.0
          ambientColor (c/mul ambientStrength  uAmbientColor)
          normal (c/normalize vNormal)
          lightDirection (c/normalize (c/sub uLightPosition vPosition))
          diffuseColor (c/mul diffuseStrength 
                              (c/max 0.0 (c/dot normal lightDirection)) 
                              uDiffuseColor)
          viewDirection (c/normalize (c/sub uViewPosition vPosition))
          reflectDirection (c/reflect (c/sub 0 lightDirection) normal)
          reflect-intensity (c/pow (c/max 
                                     (c/dot viewDirection reflectDirection) 
                                     0.0) 
                                   shininess)
          specularColor (c/mul specularStrength reflect-intensity uSpecularColor)]
      (c/vec4 (c/add ambientColor diffuseColor specularColor) 1.0))))

; --- PIPELINE ---

(def obj-render-pipeline
  (let [vert-out (c/shader-output obj-vert-shader)]
    (c/simple-pipeline [aVertex aNormal uModelMatrix uViewProjectionMatrix 
                        uNormalMatrix uLightPosition uViewPosition 
                        uAmbientColor uDiffuseColor uSpecularColor]
      [(c/prime-shader obj-vert-shader 
         aVertex aNormal uModelMatrix uViewProjectionMatrix uNormalMatrix) 
       (c/prime-shader obj-frag-shader 
         uLightPosition uViewPosition uAmbientColor uDiffuseColor uSpecularColor 
         (first vert-out) (second vert-out))])))

; --- DRAWER ---

(defn create-obj-drawer [mesh] 
  (c/drawer [materials [:objs :materials] 
             view-position [:objs :view-position]
             view-projection-mat [:objs :view-projection-mat]
             view-mat [:objs :view-mat]
             model-mat [:objs :model-mat]
             normal-mat [:objs :normal-mat]]
    obj-render-pipeline
    [(c/buf-take (:vertices-buf mesh) :vec3 (c/size-of-type :vec3) 0)
     (c/buf-take (:normals-buf mesh) :vec3 (c/size-of-type :vec3) 0)
     model-mat
     view-projection-mat
     normal-mat
     [-5 5 5]
     view-position
     (:ambient-color (nth materials (:material-index mesh)))
     (:diffuse-color (nth materials (:material-index mesh)))
     (:specular-color (nth materials (:material-index mesh)))]
    (c/draw-elements :triangles 0 (:elem-count mesh) (:element-buf mesh))))

(defn init-fn [state]
  (let [model (load-obj-model "res/magnet.obj")
        model-mat (glm.mat4x4.Mat4. 1)]
    (reduce 
      (fn [acc-state mesh-i] 
        (c/add-drawer (create-obj-drawer mesh-i) acc-state)) 
      (assoc state
        :objs {:meshes (:meshes model)
               :materials (:materials model)
               :view-projection-mat (glm.mat4x4.Mat4. 1)
               :view-mat (glm.mat4x4.Mat4. 1)
               :model-mat model-mat
               :normal-mat (.transpose (.inverse (glm.mat3x3.Mat3. model-mat)))
               :view-position [0 0 0]
               :fov 60
               :start-time (System/currentTimeMillis)}
        :time 0)
      (:meshes model))))

(defn update-fn [state]
  (let [new-t (- (System/currentTimeMillis) (-> state :objs :start-time)) 
        t (* 0.001 new-t)
        distance 10
        r (+ 1 (/ distance 2) (* (/ distance 2) (Math/sin (* 0.5 t))))
        height (+ (/ distance 4) (* (/ distance 4) (Math/sin (* 0.3 t)))) 
        projection (.perspective glm.glm/INSTANCE 
                                 (Math/toRadians (-> state :objs :fov))
                                 (float (/ (:width state) (:height state)))
                                 0.01   ;near plane
                                 100.0) ;far plane
        view-position [(* r (Math/cos t)) height (* r (Math/sin t))]
        view-mat (.lookAt glm.glm/INSTANCE 
                          (glm.vec3.Vec3. (nth view-position 0)
                                          (nth view-position 1)
                                          (nth view-position 2)) ;eye
                          (glm.vec3.Vec3. 0 0 0)  ;center 
                          (glm.vec3.Vec3. 0 1 0)) ;up-vector
        view-projection (.times projection view-mat)]
    (assoc 
      (update-in state [:objs] merge
               {:view-projection-mat view-projection 
                :view-mat view-mat 
                :view-position view-position}) 
      :time new-t)))

(defn demo []
  (c/add-update-fn! update-fn)
  (c/start! init-fn)
  (println "fps stats:")
  (clojure.pprint/pprint (-> @c/global-state :internals :fps-stats))
  (c/reset-global-state!)
  "Demo completed. Global State has been reset!")

(comment
  (demo))
