type minimap = {enabled: option(bool)};
type t = {
  lineNumbers: option(string),
  scrollBeyondLastLine: option(bool),
  minimap,
  fontSize: option(string),
  fontFamily: option(string)
};
