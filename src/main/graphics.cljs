(ns graphics)

(set! *warn-on-infer* false)

(defn create-shader-pipeline
  "Creates a WebGPU render pipeline with the given shader code"
  [device shader-code]
  (let [shader-module (.createShaderModule device
                                           #js {:code shader-code})
        ; pipeline-layout (.createPipelineLayout device
        ;                                        #js {:bindGroupLayouts #js []})
        pipeline (.createRenderPipeline device
                                        #js {:layout "auto" ; pipeline-layout
                                             :vertex #js {:module shader-module
                                                          :entryPoint "vs_main"
                                                          :buffers #js []}
                                             :fragment #js {:module shader-module
                                                            :entryPoint "fs_main"
                                                            :targets #js [#js {:format "bgra8unorm"}]}
                                             :primitive #js {:topology "triangle-strip"}})]
    pipeline))

(defn create-uniform-buffer
  "Creates a buffer for uniforms"
  [device size]
  (.createBuffer device
                 #js {:size size
                      :usage (bit-or js/GPUBufferUsage.UNIFORM
                                     js/GPUBufferUsage.COPY_DST)}))

(defn create-bind-group
  "Creates a bind group for uniforms"
  [device pipeline buffer]
  (let [layout (.getBindGroupLayout pipeline 0)]
    (.createBindGroup device
                      #js {:layout layout
                           :entries #js [#js {:binding 0
                                              :resource #js {:buffer buffer}}]})))

(defn update-uniforms
  "Updates uniform buffer with new values"
  [device buffer uniforms]
  (let [data (js/Float32Array. (js/Object.values (clj->js uniforms)))]
    (.writeBuffer (.-queue device) buffer 0 data)))

(defn render-frame
  "Renders a single frame"
  [device context pipeline uniform-buffer]
  (let [command-encoder (.createCommandEncoder device)
        texture-view (.getCurrentTexture context)
        render-pass (.beginRenderPass command-encoder
                                      #js {:colorAttachments
                                           #js [#js {:view (.createView texture-view)
                                                     :loadOp "clear"
                                                     :clearValue #js {:r 0.2 :g 0.2 :b 0.2 :a 1}
                                                     :storeOp "store"}]})]
    (.setPipeline render-pass pipeline)
    (.setBindGroup render-pass 0 (create-bind-group device pipeline uniform-buffer))
    (.draw render-pass 4)
    (.end render-pass)
    (.submit (.-queue device) #js [(.finish command-encoder)])))

(defn full-shader-code
  "Wraps fragment shader in full WGSL shader code"
  [fragment-glsl]
  (str "
struct VertexOutput {
  @builtin(position) position: vec4<f32>,
  @location(0) uv: vec2<f32>,
};

struct Uniforms {
  time: f32,
  mouse_x: f32,
  mouse_y: f32,
  resolution_x: f32,
  resolution_y: f32,
};

@group(0) @binding(0) var<uniform> uniforms: Uniforms;

@vertex
fn vs_main(@builtin(vertex_index) vertex_index: u32) -> VertexOutput {
  var positions = array<vec2<f32>, 4>(
    vec2<f32>(-1.0, -1.0),
    vec2<f32>(1.0, -1.0),
    vec2<f32>(-1.0, 1.0),
    vec2<f32>(1.0, 1.0)
  );
  
  var out: VertexOutput;
  out.position = vec4<f32>(positions[vertex_index], 0.0, 1.0);
  out.uv = positions[vertex_index] * 0.5 + 0.5;
  return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
  let coord = in.uv * 2.0 - 1.0;
  let x = coord.x;
  let y = coord.y;
  let time = uniforms.time;
  
  return vec4<f32>(" fragment-glsl ", 1.0);
}
"))

(defonce shader-canvases (atom {}))

(defn get-canvas-controls
  "Get the control functions for a canvas by ID"
  [canvas-id]
  (get @shader-canvases canvas-id))

(defn create-shader-canvas
  "Creates a new shader canvas with the given ID and initial shader expression.
   Returns a map with :canvas-id and control functions."
  [canvas-id initial-expr & {:keys [width height] :or {width 512 height 512}}]
  (if-not (contains? @shader-canvases canvas-id)
    (let [state (atom {:running true
                       :device nil
                       :context nil
                       :pipeline nil
                       :uniforms {:time 0
                                  :mouse-x 0
                                  :mouse-y 0
                                  :resolution-x width
                                  :resolution-y height}
                       :uniform-buffer nil
                       :expr initial-expr})]

      ;; Initialize WebGPU
      (-> (js/navigator.gpu.requestAdapter)
          (.then (fn [adapter]
                   (.requestDevice adapter)))
          (.then (fn [device]
                   (let [canvas (.getElementById js/document canvas-id)
                         context (.getContext canvas "webgpu")
                         format "bgra8unorm"]
                     (.configure context #js {:device device
                                              :format format})
                     (swap! state assoc :device device :context context)

                     ;; Create initial pipeline
                     (let [shader-code (full-shader-code initial-expr)
                           pipeline (create-shader-pipeline device shader-code)
                           uniform-buffer (create-uniform-buffer device (* 32 5))]
                       (swap! state assoc :pipeline pipeline :uniform-buffer uniform-buffer)

                       ;; Start animation loop
                       (letfn [(animate [time]
                                 (when (:running @state)
                                   (swap! state update-in [:uniforms :time] (constantly (/ time 1000.0)))
                                   (update-uniforms device uniform-buffer (:uniforms @state))
                                   (render-frame (:device @state)
                                                 (:context @state)
                                                 (:pipeline @state)
                                                 (:uniform-buffer @state))
                                   (js/requestAnimationFrame animate)))]
                         (js/requestAnimationFrame animate)))))))

      (let [controls
            {:set-shader! (fn [new-expr]
                            (swap! state assoc :expr new-expr)
                            (when-let [device (:device @state)]
                              (let [shader-code (full-shader-code new-expr)
                                    pipeline (create-shader-pipeline device shader-code)]
                                (swap! state assoc :pipeline pipeline))))

             :set-uniforms! (fn [uniform-map]
                              (swap! state update :uniforms merge uniform-map))

             :destroy! (fn []
                         (swap! state assoc :running false)
                         (swap! shader-canvases dissoc canvas-id))

             :get-state (fn [] @state)}]

        (swap! shader-canvases assoc canvas-id controls)
        controls))
    (let [controls (get-canvas-controls canvas-id)]
      ((:set-shader! controls) initial-expr))))

(defn shader-canvas-hiccup
  "Returns hiccup for a shader canvas element"
  [id width height]
  [:canvas {:id id
            :width width
            :height height
            :style {:border "1px solid #ccc"}}])
