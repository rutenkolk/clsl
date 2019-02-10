(ns clsl.obj-demo-fast
 (:require [clsl.core :as c])
 (:gen-class))

;recreation of the lwjgl3 assimp obj-renderer demo:
;https://github.com/LWJGL/lwjgl3-demos/blob/master/src/org/lwjgl/demo/opengl/assimp/WavefrontObjDemo.java

;optimized version using texture buffers
(defn partial-reduce-list [coll fn] 
  (map (partial reduce fn) (map #(take (inc %) coll) (range (count coll)))))

(defn create-meshes [ai-meshes]
  (doall 
    (for [mesh ai-meshes] 
      (let [vertices (for [vert (.mVertices mesh)] 
                       [(.x vert) (.y vert) (.z vert)])
            normals (for [normal (.mNormals mesh)] 
                      [(.x normal) (.y normal) (.z normal)])
            elem-count (* 3 (.mNumFaces mesh))
            faces (.mFaces mesh)
            elem-buffers (flatten
                           (doall 
                             (map #(.mIndices (.get faces %)) 
                                  (range (.mNumFaces mesh)))))
            elems (flatten
                    (for [triangle-elems elem-buffers] 
                      [(.get triangle-elems 0) 
                       (.get triangle-elems 1) 
                       (.get triangle-elems 2)]))] 
        {:vertices vertices
         :normals normals
         :elements elems
         :material-index (.mMaterialIndex mesh)
         :elem-count elem-count}))))

(defn fuse-meshes 
  "fuse meshes. append all vertices/normals. 
  appends all elements, but shifted, so that they still are valid indices." 
  [meshes]
  (let [vertices-counts (map (comp count :vertices) meshes)
        vertices-buf (c/buf (flatten (map :vertices meshes)))
        normals-buf (c/buf (flatten (map :normals meshes)))
        elem-offsets (partial-reduce-list (drop-last (cons 0 vertices-counts)) +)
        elements-corrected (map 
                             (fn [[v off]] (map (partial + off) v)) 
                             (partition 2 (interleave (map :elements meshes) 
                                                      elem-offsets)))
        elements-buf (c/buf (flatten elements-corrected))
        materials-index-buf (c/buf 
                              (flatten
                                (map
                                  (fn [[ind n]] (repeat n ind))
                                  (partition 2 
                                    (interleave 
                                      (map :material-index meshes) 
                                      vertices-counts)))))]
    {:vertices-buf vertices-buf
     :normals-buf normals-buf
     :elements-buf elements-buf
     :material-index-buf materials-index-buf
     :element-count (reduce + (map :elem-count meshes))}))

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
        [(.r ambient-color) (.g ambient-color) (.b ambient-color) 
         (.r diffuse-color) (.g diffuse-color) (.b diffuse-color)
         (.r specular-color) (.g specular-color) (.b specular-color)]))))

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
                           #(org.lwjgl.assimp.AIMesh/createSafe (.get (.mMeshes scene) %)) 
                           (range meshcount)))
        _ (println "ai-meshes read in!")
        meshes (create-meshes ai-meshes)
        _ (println "meshes created!")
        fused-mesh (fuse-meshes meshes)
        _ (println "meshes fused!")
        _ (println "materialcount" (.mNumMaterials scene))
        raw-materials (map #(.get (.mMaterials scene) %) (range (.mNumMaterials scene)))
        _ (clojure.pprint/pprint raw-materials)
        ai-materials (doall (map 
                              #(org.lwjgl.assimp.AIMaterial/createSafe 
                                 (.get (.mMaterials scene) %)) 
                              (range (.mNumMaterials scene))))
        _ (println "ai-materials read in!")
        materials (create-materials ai-materials)
        materials-buf (c/buf (flatten materials))
        materials-buf-tex (c/buf-as-texture materials-buf :vec3)
        _ (println "loading model finished.")
        _ (println "Assimp error String:" (org.lwjgl.assimp.Assimp/aiGetErrorString))
        _ (org.lwjgl.assimp.Assimp/aiReleaseImport scene)]
    {:vertices-buf (:vertices-buf fused-mesh)
     :normals-buf (:normals-buf fused-mesh)
     :elements-buf (:elements-buf fused-mesh)
     :element-count (:element-count fused-mesh)
     :material-index-buf (:material-index-buf fused-mesh)
     :materials materials
     :materials-buf-texture  materials-buf-tex}))

; --- SHADERS ---

(def obj-vert-shader
  (c/vertex-shader [aVertex aNormal material_index 
                    uModelMatrix uViewProjectionMatrix uNormalMatrix]
    [(c/mul uViewProjectionMatrix uModelMatrix (c/vec4 aVertex 1))
     (c/typed :vec3 (c/swizzle (c/mul uModelMatrix (c/vec4 aVertex 1)) :xyz))
     (c/typed :vec3 (c/mul uNormalMatrix aNormal))
     (c/typed :float (c/cast-to-float material_index))]))

(def obj-frag-shader
  (c/fragment-shader [uLightPosition uViewPosition 
                      vPosition vNormal material_tex_buf material_index_float]
    (let [ambientStrength  0.5
          diffuseStrength  0.5
          specularStrength 0.5
          shininess        4.0
          material_index (c/cast-to-int (c/add material_index_float 0.5))
          uAmbientColor (c/buffer-texel-fetch 
                             material_tex_buf :vec3 (c/add (c/mul material_index 3) 0))
          uDiffuseColor (c/buffer-texel-fetch 
                             material_tex_buf :vec3 (c/add (c/mul material_index 3) 1))
          uSpecularColor (c/buffer-texel-fetch 
                             material_tex_buf :vec3 (c/add (c/mul material_index 3) 2))
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
                        mat_tex_buf mat_ind]
      [(c/prime-shader obj-vert-shader 
         aVertex aNormal mat_ind uModelMatrix uViewProjectionMatrix uNormalMatrix) 
       (c/prime-shader obj-frag-shader 
         uLightPosition uViewPosition 
         (first vert-out) (second vert-out)
         mat_tex_buf (second (rest vert-out)))])))

; --- DRAWER ---

(defn create-obj-drawer []
  (c/drawer [mesh [:objs :model]
             materials [:objs :materials] 
             view-position [:objs :view-position]
             view-projection-mat [:objs :view-projection-mat]
             view-mat [:objs :view-mat]
             model-mat [:objs :model-mat]
             normal-mat [:objs :normal-mat]
             material-tex [:objs :materials-buf-texture]]
    obj-render-pipeline
    [(c/buf-take (:vertices-buf mesh) :vec3 (c/size-of-type :vec3) 0)
     (c/buf-take (:normals-buf mesh) :vec3 (c/size-of-type :vec3) 0)
     model-mat
     view-projection-mat
     normal-mat
     [-5 5 5]
     view-position
     material-tex
     (c/buf-take (:material-index-buf mesh) :int (c/size-of-type :int) 0)] 
    (c/draw-elements :triangles 0 (:element-count mesh) (:elements-buf mesh)))) 

(defn init-fn [state]
  (let [model (load-obj-model "res/magnet.obj")
        model-mat (glm.mat4x4.Mat4. 1)]
      (assoc state
        :objs {:model model
               :materials (:materials model)
               :materials-buf-texture (:materials-buf-texture model)
               :view-projection-mat (glm.mat4x4.Mat4. 1)
               :view-mat (glm.mat4x4.Mat4. 1)
               :model-mat model-mat
               :normal-mat (.transpose (.inverse (glm.mat3x3.Mat3. model-mat)))
               :view-position [0 0 0]
               :fov 60
               :start-time (System/currentTimeMillis)}
        :time 0)))

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
  (c/add-drawer! (create-obj-drawer))
  (c/start! init-fn {:fullscreen true})
  (println "fps stats:")
  (clojure.pprint/pprint (-> @c/global-state :internals :fps-stats))
  (c/reset-global-state!)
  "Demo completed. Global State has been reset!")

(comment
  (demo)
  (.clear c/keyinput-queue)
  (c/stop&reset!))
