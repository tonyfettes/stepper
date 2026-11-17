/* Local Melange bindings for the Web Worker API.

   melange-webapi (>= 0.20) does not bind Worker, DedicatedWorkerGlobalScope,
   or MessageEvent, so the small slice of that API used by the stepper is
   declared here. Webapi.Url is still provided by melange-webapi and reused. */

module MessageEvent = {
  type t;

  [@mel.get] external data: t => 'a = "data";
};

module ErrorEvent = {
  type t;

  [@mel.get] external message: t => string = "message";
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

  [@mel.send.pipe: t] external postMessage: 'a => unit = "postMessage";

  [@mel.send] external terminate: t => unit = "terminate";

  [@mel.send.pipe: t]
  external addMessageEventListener:
    ([@mel.as "message"] _, MessageEvent.t => unit) => unit =
    "addEventListener";

  [@mel.send.pipe: t]
  external addErrorEventListener:
    ([@mel.as "error"] _, ErrorEvent.t => unit) => unit =
    "addEventListener";

  [@mel.send.pipe: t]
  external removeMessageEventListener:
    ([@mel.as "message"] _, MessageEvent.t => unit) => unit =
    "removeEventListener";
};

module DedicatedWorkerGlobalScope = {
  type t;

  [@mel.send.pipe: t] external postMessage: 'a => unit = "postMessage";

  [@mel.send.pipe: t]
  external addMessageEventListener:
    ([@mel.as "message"] _, MessageEvent.t => unit) => unit =
    "addEventListener";
};

external dedicatedWorkerGlobalScope: DedicatedWorkerGlobalScope.t = "self";
