(function(){
  var _a, key, val;
  var __hasProp = Object.prototype.hasOwnProperty;
  // Loader for CoffeeScript as a Node.js library.
  _a = require('./coffee-script');
  for (key in _a) { if (__hasProp.call(_a, key)) {
    val = _a[key];
    (exports[key] = val);
  }}
})();
