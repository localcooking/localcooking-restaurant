#!/bin/bash

./node_modules/.bin/bower i && \
    ./node_modules/.bin/pulp build && \
    ./node_modules/.bin/purs bundle output/**/*.js -m Main --main Main > index.tmp.js && \
    ./node_modules/.bin/browserify index.tmp.js > index.js && \
    ./node_modules/.bin/browserify index.tmp.js -g [ envify --NODE_ENV production ] -g uglifyify > index.js.min && \
    rm index.tmp.js
