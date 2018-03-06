"use strict";

exports.onPopStateImpl = function onPopStateImpl(f,w) {
  w.onpopstate = function onPopStateImplHandler(event) {
    f(event.state);
  };
};


exports.queryParams = function queryParamsImpl(loc) {
  var match,
    pl     = /\+/g,  // Regex for replacing addition symbol with a space
    search = /([^&=]+)=?([^&]*)/g,
    decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    query  = loc.search.substring(1),
    result = {};
  while (match = search.exec(query))
    result[decode(match[1])] = decode(match[2]);

  return result;
}
