#!/bin/bash

./node_modules/.bin/bower i && \
    ./node_modules/.bin/pulp build && \
    ./node_modules/.bin/purs bundle output/**/*.js -m Main --main Main > index.tmp.js && \
    echo "Bundled" && \
    ./node_modules/.bin/browserify index.tmp.js > index.js && \
    echo "Minifying" && \
    ./node_modules/.bin/browserify index.tmp.js -g [ envify --NODE_ENV production ] -g uglifyify | ./node_modules/.bin/uglifyjs --compress --mangle > index.js.min && \
    rm index.tmp.js
