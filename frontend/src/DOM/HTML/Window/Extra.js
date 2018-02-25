"use strict";

exports.onPopStateImpl = function onPopStateImpl(f,w) {
  w.onpopstate = function onPopStateImplHandler(event) {
    f(event.state);
  };
};
