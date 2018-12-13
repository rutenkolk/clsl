# Clsl

Graphics development in Clojure.

Write your shaders in clojure-like code and transpile to the OpenGL Shading Language glsl without actually using glsl. Hence the Name clsl, or Clojure shading Language.

While clsl certainly provides tools to produce shaders, it's actually just a Library emitting glsl in the end so you can inspect the process all the way down.

It mimicks only a Subset of Clojure and "Clojure-like" Code, since there are limitations to this process. For example glsl does not allow for recursion or other nice dynamic features. This makes a port from most core functions infeasible. Additionally there are severe limits on datastructures in glsl. There exists virtually no dynamic memory management within a shader itself. There are no variable-arity functions, etc...
 
In CLSL Shaders are thought of functions, taking some input and transforming it into something digestable for the next shader stage.
 
In principle this can lead to quite beneficial properties, but beware:

This is still very experimental!

One example would be a possible compile-path not to glsl but back to clojure functions making shaders unit-testable which is not possible in raw OpenGL/Vulkan.

## Usage

Simply require clsl.core as some alias into your namespace and get going. In this early state a simple triangle drawing demo is the -main function of this library.

Check out the demos.clj file for inspiration.

This library in its current state expects some basic knowledge about OpenGL, but is generally targeted at beginners as well. Documentation may improve in the future.

The basic procedure to drawing data is: 

You have data per vertex point -> you specify which region of the data should be used for creating an image and how it's going to be interpreted (draw-call) -> the vertex-shader transforms this data and sets the final position of the vertex -> the image gets rasterized into "fragments" -> the fragment shader actually fills them with color -> the image gets displayed.

Vertex and fragment shaders are being specified in a function-like syntax *without* type information completely independent of each other.

You glue vertex and fragment shader together by creating a "simple-pipeline". This is again a construct akin to a function, taking arguments and passing them to the respective shaders. This is also where you determine which data from the vertex shader output corresponds to which fragment shader input. In CLSL this will be called priming a shader.

After this you can create a drawer which is again similar to a function, but this time it's actively querying data from some magically managed state. It specifies the pipeline to use and with what data to fill the pipeline. Here you can actually tell clsl: the first argument is from this buffer, or put a matrix in it, maybe a float will do?

If you have any experience with OpenGL/WebGL/Vulkan/Direct3D and therefore glsl/web-glsl/hlsl, you may notice, that we didn't specify any in/out/uniform, or varying, or anything of that kind. This is deliberate. The actual result the shaders compute doesnt depend on where the data comes from. Only when you specify a drawer and it's input to the pipeline, will the type-information propagate and the actual shaders be compiled. So you can imagine, creating a new drawer can be fairly expensive. 

In general clsl decouples draw-processes from each other. No more state set up when making a draw call. You specify a drawer that takes care of one individual draw-process.

If you want to draw something similar you can always reuse the same clsl-shaders or even the same pipeline to do something completely different. You can have two drawers use the same pipeline and use wildly different input types and the resulting glsl-shaders will have completely different input types. Furthermore what gets updated and when is completely up the programmer creating the drawers. 

Now this system is not without flaws of-course. This is very early and experimental and the jvm likes to crash a lot at this point. Texture support is basically there but not quite functional yet. It is planned to have a system with custom drawer-keys that enable the user to sort drawers. Some drawers depend on each other and others don't. A subpass solution akin to vkSubpass is in planning. Also the possibility to make state-reuse possible with a key-based bucketed rendering would most probably be blessing for performance. 

There are still some unsolved problems right here and now though too. Drawer creation being tied to shader compilation is unfortunate and creating shaders beforehand would be nice. With the dynamic nature of the library and clojure itself I can only see a few options here like creating a myriad of shaders, or creating really complex ones and making the dynamic decision on the device-code site of things.

As you can see there is still a ton of work to be done.

In the meantime: Please check out the demos.clj file for things that absolutely should work right at this moment.

## Caveats

Vertex-Shaders *need* to output the final position. Therefore the first output of a vertex-shader is always the position assignment.
So be carful when you query (first (shader-outputs my-vert-shader)) as this will give you first output _after_ the position. So this will be the second value in the vector you typed as the output for the vertex-shader.

Fragment shaders *need* to output a color. So there can only be one output. OpenGL/Vulkan technically allow for more than this, but more than one framebuffer render target is not supported (yet?).

As of right now, vertex shaders need their output to be typed manually. So wrap all normal output-expressions from a vertex-shader with a (typed :my-type expr).

## License

Copyright © 2018 Chris Rutenkolk

Distributed under the MIT License.
