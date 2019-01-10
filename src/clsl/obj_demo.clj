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
