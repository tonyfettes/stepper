/* Local Melange bindings for the Web Worker API.

   melange-webapi (>= 0.20) does not bind Worker, DedicatedWorkerGlobalScope,
   or MessageEvent, so the small slice of that API used by the stepper is
   declared here. Webapi.Url is still provided by melange-webapi and reused. */

module MessageEvent = {
  type t;

  [@mel.get] external data: t => 'a = "data";
};

module Worker = {
  type t;

  /* The options object (`{type: "module"}`) is required so the worker is
     loaded as an ES module; the compiled worker uses `import`, which a classic
     worker cannot run. Keeping this an [external] also lets the `new Worker(new
     URL(...), {...})` call inline at the call site, which is the shape Vite
     statically detects in order to bundle the worker. */
  [@mel.new]
  external makeWithUrl: (Webapi.Url.t, {. "type": string}) => t = "Worker";

  [@mel.send] external postMessage: ('a, [@mel.this] t) => unit = "postMessage";

  [@mel.send] external terminate: ([@mel.this] t) => unit = "terminate";

  [@mel.send]
  external addMessageEventListener:
    ([@mel.as "message"] _, MessageEvent.t => unit, [@mel.this] t) => unit =
    "addEventListener";

  [@mel.send]
  external removeMessageEventListener:
    ([@mel.as "message"] _, MessageEvent.t => unit, [@mel.this] t) => unit =
    "removeEventListener";
};

module DedicatedWorkerGlobalScope = {
  type t;

  [@mel.send] external postMessage: ('a, [@mel.this] t) => unit = "postMessage";

  [@mel.send]
  external addMessageEventListener:
    ([@mel.as "message"] _, MessageEvent.t => unit, [@mel.this] t) => unit =
    "addEventListener";
};

external dedicatedWorkerGlobalScope: DedicatedWorkerGlobalScope.t = "self";
