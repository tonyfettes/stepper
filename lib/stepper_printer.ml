let to_buffer (buffer : Buffer.t) = PPrint.ToBuffer.pretty 1.0 64 buffer

let to_string (document : PPrint.document) : string =
  let buffer = Buffer.create 42 in
  to_buffer buffer document;
  Buffer.contents buffer
