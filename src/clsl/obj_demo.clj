(ns clsl.obj-demo
 (:require [clsl.core :as c])
 (:gen-class))

;recreation of the lwjgl3 assimp obj-renderer demo:
;https://github.com/LWJGL/lwjgl3-demos/blob/master/src/org/lwjgl/demo/opengl/assimp/WavefrontObjDemo.java

(def obj-vert-shader
  (c/vertex-shader [aVertex aNormal uModelMatrix uViewProjectionMatrix uNormalMatrix]
    [(c/mul uViewProjectionMatrix uModelMatrix aVertex model)
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
                        uNormalMatrix uLightPosition uViewPosition uAmbientColor
                        uDiffuseColor uSpecularColor]
      [(c/prime-shader obj-vert-shader 
         aVertex aNormal uModelMatrix uViewProjectionMatrix uNormalMatrix) 
       (c/prime-shader obj-frag-shader 
         uLightPosition uViewPosition uAmbientColor uDiffuseColor uSpecularColor 
         (first vert-out) (second vert-out))])))

(defn create obj-drawer
  (c/drawer [tr-buf [:objs :tr-buf]
             my-texture [:objs texture-id-lookup]
             tr-buf-count [:objs :tr-buf-count]
             t [:time]]
    obj-render-pipeline
    []
    (c/drawarrays :triangles 0 tr-buf-count)))
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
         (for [mesh ai-meshes]
           (let [vertices-buf (c/buf (* org.lwjgl.assimp.AIVector3D/SIZEOF (.remaining (.vertices mesh))) (.address (.vertices mesh)))
                 normals-buf (c/buf (* org.lwjgl.assimp.AIVector3D/SIZEOF (.remaining (.normals mesh))) (.address (.normals mesh)))
                 elem-count (* 3 (.mNumFaces mesh))
                 elem-host-buf (org.lwjgl.BufferUtils/createIntBuffer elem-count)

                 ])
           

           )
        ;TODO: port Mesh constructor
         _ (org.lwjgl.assimp.Assimp/aiReleaseImport scene)]
     )
  )
(comment
   (let [path "res/magnet.obj"
         scene (org.lwjgl.assimp.Assimp/aiImportFile 
                 path 
                 (bit-or org.lwjgl.assimp.Assimp/aiProcess_JoinIdenticalVertices 
                         org.lwjgl.assimp.Assimp/aiProcess_Triangulate))
         _ (println "is scene nil?" (nil? scene))
         _ (clojure.pprint/pprint scene)
         _ (println "Assimp error String:" (org.lwjgl.assimp.Assimp/aiGetErrorString))
         _ (org.lwjgl.assimp.Assimp/aiReleaseImport scene)]
     )
  )
