![TOML Logo](https://github.com/toml-lang/toml/blob/main/logos/toml-100.png)


toml-rb
=======

[![Gem Version](https://badge.fury.io/rb/toml-rb.svg)](http://badge.fury.io/rb/toml-rb)
[![Build Status](https://github.com/emancu/toml-rb/actions/workflows/ruby.yml/badge.svg)](https://github.com/emancu/toml-rb/actions/workflows/ruby.yml)
[![Code Climate](https://codeclimate.com/github/emancu/toml-rb/badges/gpa.svg)](https://codeclimate.com/github/emancu/toml-rb)
[![RubyGem](https://img.shields.io/gem/dt/toml-rb.svg)](https://rubygems.org/gems/toml-rb)

A [TOML](https://github.com/toml-lang/toml) parser using [Citrus](http://mjackson.github.io/citrus) library.

TOML specs supported: `1.0.0`

Installation
------------

    $ gem install toml-rb

Parser Usage
------------

```ruby
require 'toml-rb'

# From a file!
path = File.join(File.dirname(__FILE__), 'path', 'to', 'file')
TomlRB.load_file(path)

# From a stream!
stream = <<-EOS
  title = "wow!"

  [awesome]
    you    = true
    others = false
EOS
TomlRB.parse(stream)
# => {"title"=>"wow!", "awesome"=>{"you"=>true, "others"=>false}}

# You want symbols as your keys? No problem!
TomlRB.load_file(path, symbolize_keys: true) 
# Works the same for TomlRB.parse
```

Dumper Usage
------------

```ruby
require 'toml-rb'

# Simple example
TomlRB.dump( simple: true)
# => "simple = true\n"


# Complex example
hash = { 
    "title"=>"wow!", 
    "awesome"=> {
        "you"=>true, 
        "others"=>false
    }
}

TomlRB.dump(hash)
# => "title = \"wow!\"\n[awesome]\nothers = false\nyou = true\n"
```

Contributing
------------

1. Fork it
2. Create your feature branch `git checkout -b my-new-feature`
3. Add tests and commit your changes `git commit -am 'Add some feature'`
4. Run tests `$ rake`
5. Push the branch `git push origin my-new-feature`
6. Create new Pull Request

License
-------

MIT License

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
