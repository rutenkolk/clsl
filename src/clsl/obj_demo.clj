(ns clsl.obj-demo
 (:require [clsl.core :as c])
 (:gen-class))

;recreation of the lwjgl3 assimp obj-renderer demo:
;https://github.com/LWJGL/lwjgl3-demos/blob/master/src/org/lwjgl/demo/opengl/assimp/WavefrontObjDemo.java

(defn create-meshes [ai-meshes]
  (for [mesh ai-meshes] 
    (let [vertices-buf (c/buf 
                         (* org.lwjgl.assimp.AIVector3D/SIZEOF (.remaining (.vertices mesh))) 
                         (.address (.vertices mesh)))
          normals-buf (c/buf 
                        (* org.lwjgl.assimp.AIVector3D/SIZEOF (.remaining (.normals mesh))) 
                        (.address (.normals mesh)))
          elem-count (* 3 (.mNumFaces mesh))
          elems (map #(.mIndices %) (.mFaces mesh))
          elem-buf (c/buf elems)]
      {:vertices-buf vertices-buf
       :normals-buf normals-buf
       :element-buf elem-buf
       :material-index (.mMaterialIndex mesh)})))
(defn create-materials [ai-materials]
  (for [material ai-materials] 
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
     :specular-color [(.r specular-color) (.g specular-color) (.b specular-color)]}))

(defn load-obj-model [path]
  (let [path "res/magnet.obj"
        scene (org.lwjgl.assimp.Assimp/aiImportFile 
                path 
                (bit-or org.lwjgl.assimp.Assimp/aiProcess_JoinIdenticalVertices 
                        org.lwjgl.assimp.Assimp/aiProcess_Triangulate))
        _ (println "is scene nil?" (nil? scene))
        _ (clojure.pprint/pprint scene)
        _ (println "Assimp error String:" (org.lwjgl.assimp.Assimp/aiGetErrorString))
        meshcount (.mNumMeshes scene)
        ai-meshes (map #(org.lwjgl.assimp.AIMesh/create %) (.mMeshes scene))
        meshes (create-meshes ai-meshes)
        ai-materials (map #(org.lwjgl.assimpAIMaterial/create %) (.mMaterials scene))
        materials (create-materials ai-materials)
        
        _ (println "loading model finished.")
        _ (println "Assimp error String:" (org.lwjgl.assimp.Assimp/aiGetErrorString))
        ;_ (org.lwjgl.assimp.Assimp/aiReleaseImport scene)
        ]
    {:meshes meshes
     :materials materials}))



(def obj-vert-shader
  (c/vertex-shader [aVertex aNormal uModelMatrix uViewProjectionMatrix uNormalMatrix]
    [(c/mul uViewProjectionMatrix uModelMatrix aVertex)
     (c/typed :vec3 (c/swizzle (c/mul uModelMatrix aVertex) :xyz))
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
          specularColor (c/mul specularStrength 
                               (c/pow 
                                 (c/max 
                                   (c/dot viewDirection reflectDirection) 
                                   0.0) 
                                 shininess) 
                               uSpecularColor)]
      (c/vec4 (c/add ambientColor diffuseColor specularColor) 1.0))))

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

(defn create-obj-drawer [mesh] 
  (c/drawer [mesh [:objs :meshes mesh]
             materials [:objs :materials] 
             view-position [:obj :view-position]
             view-projection-mat [:objs :view-projection-mat]
             view-mat [:objs :view-mat]
             model-mat [:objs :model-mat]
             normal-mat [:objs :normal-mat]
             t [:time]]
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
    (c/draw-elements :triangles 0 tr-buf-count)))

(defn init-fn [state]
  (let [model (load-obj-model "res/magnet.obj")] 
    (doall (map #(c/add-drawer! (create-obj-drawer %)) (:meshes model)))
    (assoc 
      state
      :objs {:meshes (:meshes model)
             :materials (:materials model)
             :view-projection-mat (glm.mat4x4.Mat4. 1)
             :view-mat (glm.mat4x4.Mat4. 1)
             :model-mat (.scale 
                          (.rotate (glm.mat4x4.Mat4. 1) (float (* 0.5 Math/PI)) (glm.vec3.Vec3. 0 1 0))
                         1.5 1.5 1.5)
             :normal-mat (.transpose (.inverse (glm.mat3x3.Mat3. model-mat)))
             :view-position [0 0 0]
             :fov 60})))

(defn update-fn [state]
  (let [new-t (- (System/currentTimeMillis) (:start-time state)) 
        t (* 0.001 new-t)
        projection (.perspective glm.glm/INSTANCE 
                                 (Math/toRadians (-> state :objs :fov)) (float (/ (:width state) (:height state)))
                                 0.01
                                 100.0)
        view-position [(* 10 (Math/cos t)) 10.0 (* 10 (Math/sin t))]
        view-mat (.lookAt glm.glm/INSTANCE 
                          (glm.vec3.Vec3. (* 10 (Math/cos 1)) 10.0 (* 10 (Math/sin 1))) 
                          (glm.vec3.Vec3. 0 0 0) 
                          (glm.vec3.Vec3. 0 1 0))
        view-projection (.times view-matrix projection)
        
        ]
    (assoc
      state
      :objs {:view-projection-mat view-projection-mat
             :view-mat view-mat
             :view-position view-position}
      :time new-t)))
(defn demo []
  (c/add-update-fn! update-fn)
  (c/start! my-state-init-fn)
  (c/reset-global-state!)
  "Demo completed. Global State has been reset!")
(comment
  (demo)
  )
