# LocalCooking - Mitch

This edition of the LocalCooking.com application is the _first_ draft -
specifically, when Mitch Vogt (co-founder) is the first chef.

---------------


This source repository depends on a number of support packages which are
also accessed through our private repository - access to this one repo assumes
access to the others. You're expected to have SSH keys registered in the gitlab server
at [git.localcooking.com](https://git.localcooking.com) for TLS-enabled URIs a 'la
`git@git.localcooking.com:team/project.git`.


## Building

Currently, we only support [Ubuntu 16.04](https://ubuntu.org) as our target platform.

### Prerequisites

There are a few shared libraries and development tools we'll have to get before the
software is built:

#### Build Tools

Please follow the instructions for each tool to get it installed:

- [stack - the Haskell build tool](https://haskellstack.org)
- [nvm - the Node.js version manager](https://github.com/creationix/nvm)

We're using the Node.js version `v8.10.0`, so run `nvm install v8.10.0` and `use` it.

#### Shared Libraries

First, you'll need build tools and pkg-config for libsodium, and while we're at it, postgres's
client library:

```bash
sudo apt install build-essential autoconf automake libtool pkg-config libpq-dev
```

After that, find a place to clone the lastest [libsodium](https://github.com/jedisct1/libsodium),
then build and install it:

```bash
cd libsodium/
./autogen.sh
./configure
make
sudo make install
```

You'll have to copy the `.so` files to a different location too; for some reason, stack's search
path and make's install path aren't the same:

```bash
sudo cp /usr/local/lib/libsodium.so* /usr/lib/x86_64-linux-gnu/
```

#### Submodules

Lastly, you'll need to populate all the git submodules inside this repo's root folder `localcooking-mitch/`:

```bash
git submodule update --init --recursive --remote
```

### Building the Frontend

This should be pretty painless, if you have enough RAM:

```bash
cd frontend/
npm install
```

This will fetch the [PureScript](https://purescript.org) compiler,
[pulp](https://github.com/purescript-contrib/pulp) build tool, and
[browserify](https://browserify.org) Node.js module packing tool.

The frontend is built with PureScript wrappers around [React.js](https://reactjs.org)
and a number of other libraries; it has to do a lot.

But, in the end, it should spit out a `index.js`. This will be
packed in the server's executable file during compilation.

### Building the Backend

Alright, now for the backend - also should be pretty painless:

```bash
cd backend/
stack build
```

This should also build everything under-the-sun, so it might take a while.
As a result, though, it should have an executable built, accessible via `stack exec localcooking`.


## Running

To run the executable, you can see some of the arguments to the command with `--help`:

```bash
stack exec localcooking -- --help
```

There are a few required arguments - namely, the postgres database password. You can observe
the `localcooking.service` systemd file for example parameters.


## Installation

The easiest service management system on Ubuntu is systemd. We're also using an [NGINX](https://nginx.com)
reverse proxy to handle TLS connections (registered via [LetsEncrypt](https://letsencrypt.org)'s [CertBot](https://certbot.eff.org)). To install, copy the service file (after supplying the correct PostgreSQL role's password):


```bash
sudo cp localcooking.service /etc/systemd/system/
sudo systemctl daemon-reload
```

Also observe the `localcooking-mitch.nginx` config file (this would replace `/etc/nginx/sites-enabled/default`) - there's a masked static file redirection under `/var/www/html/static` - that needs to be created and populated as well:

```bash
sudo mkdir /var/www/html/static
sudo mkdir /var/www/html/static/images
cd /var/www/html/static/images

ln -s ~/localcooking-mitch/logo/*.png ./  # or wherever you cloned this repo to
ln -s ~/localcooking-mitch/logo/*.svg ./
```

Given all this, test and restart the services:

```bash
sudo nginx -t
sudo systemctl reload nginx.service
sudo systemctl start localcooking.service
```

Check the status to make sure all is well:

```bash
sudo systemctl status localcooking.service
```

If not, open an issue.
