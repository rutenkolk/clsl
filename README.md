# Clsl

LWJGL3 based Graphics programming in Clojure.

[![Clojars Project](https://img.shields.io/clojars/v/clsl.svg)](https://clojars.org/clsl)

Write your shaders in clojure-like code and transpile to the OpenGL Shading Language glsl without actually using glsl. Hence the Name clsl, or Clojure shading language.

## Why?

Graphics programming is done through APIs like OpenGL, which expose heavily state-driven functionality and require a lot of care to use. You have to write "shader programs" in a certain shading language and are not free to choose. This is a stark contrast to the functional and dynamic way of dealing with problems, which clojure users are accustomed to. Furthermore, native APIS are painful to use from clojure, but luckily there is the excellent LWJGL project to help us out. While LWJGL simply mimicks the availabe libraries, this is a framework ontop of LWJGL3 to provide graphics programming in a functional fashion in clojure.    

Currently clsl mimicks only a subset of Clojure and "Clojure-like" code, as well as OpenGL functionality since there are limitations to this process. See below.

## Usage

Simply require clsl.core as some alias into your namespace and get going.

`project.clj:`
```clojure
:dependencies [[org.clojure/clojure "1.10.0"]
               [clsl "0.2.1"]]
```
`core.clj:`
```clojure
(ns ext-demo.core
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
     (.rotate (glm.mat4x4.Mat4.) (* t 0.001) (glm.vec3.Vec3. 0 0 -1))
     1]
    (c/drawarrays :triangles 0 tr-buf-count)))

(defn my-state-init-fn [state]
  (assoc state
         :objs {:tr-buf (c/buf (c/load-value-to-array interleaved-data))
                :tr-buf-count 3}
         :start-time (System/currentTimeMillis)
         :time 0)) 

(defn demo []
  (c/add-drawer! demo-triangle-drawer)
  (c/add-update-fn! (fn [state] (assoc state :time (- (System/currentTimeMillis) (:start-time state)))))
  (c/start! my-state-init-fn)
  (println "fps stats:")
  (clojure.pprint/pprint (-> @c/global-state :internals :fps-stats))
  (c/reset-global-state!)
  "Demo completed. Global State has been reset!")

(defn -main [& args]
  (demo))
```
## Implemented Features:

* Shader definition completely in clojure
* Independent shaders
* Shader input types depend on what is actually supplied (translation happens via the user extendable clojure-t->glsl-t protocoll)
* Texture support (loading textures and using in shaders)
* Buffer Texture Support (arbitrary lookup of data in a buffer in a shader)

## In depth:

This library in its current state expects some basic knowledge about OpenGL, but is generally targeted at beginners as well. Documentation may improve in the future.

The basic procedure to drawing data is: 

You have data per vertex point -> you specify which region of the data should be used for creating an image and how it's going to be interpreted (draw-call) -> the vertex-shader transforms this data and sets the final position of the vertex -> the image gets rasterized into "fragments" -> the fragment shader actually fills them with color -> the image gets displayed.

Vertex and fragment shaders are being specified in a function-like syntax *without* type information completely independent of each other.

You glue vertex and fragment shader together by creating a "simple-pipeline". This is again a construct akin to a function, taking arguments and passing them to the respective shaders. This is also where you determine which data from the vertex shader output corresponds to which fragment shader input. In CLSL this will be called priming a shader.

After this you can create a drawer which is again similar to a function, but this time it's actively querying data from some magically managed state. It specifies the pipeline to use and with what data to fill the pipeline. Here you can actually tell clsl: the first argument is from this buffer, or put a matrix in it, maybe a float will do?

If you have any experience with OpenGL/WebGL/Vulkan/Direct3D and therefore glsl/web-glsl/hlsl, you may notice, that we didn't specify any in/out/uniform, or varying, or anything of that kind. This is deliberate. The actual result the shaders compute doesn't depend on where the data comes from. Only when you specify a drawer and it's input to the pipeline, will the type-information propagate and the actual shaders be compiled. So you can imagine, creating a new drawer can be fairly expensive. 

In general clsl decouples draw-processes from each other. No more state set up when making a draw call. You specify a drawer that takes care of one individual draw-process.

If you want to draw something similar you can always reuse the same clsl-shaders or even the same pipeline to do something completely different. You can have two drawers use the same pipeline and use wildly different input types and the resulting glsl-shaders will have completely different input types. Furthermore what gets updated and when is completely up the programmer creating the drawers. 

Now this system is not without flaws of-course. This is very early and experimental and the jvm likes to crash a lot at this point. It is planned to have a system with custom drawer-keys that enable the user to sort drawers. Some drawers depend on each other and others don't. A subpass solution akin to vkSubpass is in planning. Also the possibility to make state-reuse possible with a key-based bucketed rendering would most probably be blessing for performance. 

There are still some unsolved problems right here and now though too. Drawer creation being tied to shader compilation is unfortunate and creating shaders beforehand would be nice. With the dynamic nature of the library and clojure itself I can only see a few options here like creating a myriad of shaders, or creating really complex ones and making the dynamic decision on the device-code site of things.

As you can see there is still a ton of work to be done.

In the meantime: Please check out the demos.clj file for things that absolutely should work right at this moment.


## Limitations

Unfortunately there are some limitations as to what is possible to translate from clojure to glsl. For example glsl does not allow for recursion or other nice dynamic features. This makes a port from most core functions infeasible. Additionally there are severe limits on datastructures in glsl. There exists virtually no dynamic memory management within a shader itself. There are no variable-arity functions, etc...

## Caveats

Vertex-Shaders *need* to output the final position. Therefore the first output of a vertex-shader is always the position assignment.

So be carful when you query (first (shader-outputs my-vert-shader)) as this will give you first output _after_ the position. So this will be the second value in the vector you typed as the output for the vertex-shader.

Fragment shaders *need* to output a color. So there can only be one output. OpenGL/Vulkan technically allow for more than this, but more than one framebuffer render target is not supported (yet?).

As of right now, vertex shaders need their output to be typed manually. So wrap all normal output-expressions from a vertex-shader with a (typed :my-type expr). This will hopefully be unnecessary in a future version but not break existing code.

## License

Copyright © 2018 Chris Rutenkolk

Distributed under the MIT License.
