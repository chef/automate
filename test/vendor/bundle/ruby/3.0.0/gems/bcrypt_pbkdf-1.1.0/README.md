# bcrypt_pbkdf-ruby

bcrypt_pbkdf is a ruby gem implementing bcrypt_pbkdf from OpenBSD. This is currently used by net-ssh to read password encrypted Ed25519 keys.

[![Build Status](https://travis-ci.org/mfazekas/bcrypt_pbkdf-ruby.png?branch=master)](https://travis-ci.org/mfazekas/bcrypt_pbkdf-ruby)

# Acknowledgements

* The gut of the code is based on OpenBSD's bcrypt_pbkdf.c implementation
* Some ideas/code were taken adopted bcrypt-ruby: https://github.com/codahale/bcrypt-ruby

# Links:

http://www.tedunangst.com/flak/post/bcrypt-pbkdf
http://cvsweb.openbsd.org/cgi-bin/cvsweb/src/lib/libutil/bcrypt_pbkdf.c?rev=1.13&content-type=text/x-cvsweb-markup

# Building

For windows cross build make sure you checked out the gem source under the home directory

```sh
gem install rake-compiler-dock
```

```sh
bundle exec rake compile
bundle exec rake test
bundle exec rake clean clobber
bundle exec rake gem:windows
bundle exec rake release
bundle exec rake gem:windows:release
```
