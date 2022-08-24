# MuchFactory

MuchFactory is an API for generating randomized data.  This is intended to be brought into testing environments and used in test runs to help generate randomized test data.

Note: this was originally implemented in and extracted from [Assert](https://github.com/redding/assert).

## Usage

```ruby
require "much-factory"

MuchFactory.integer    #=> 15742
MuchFactory.integer(3) #=> 2
MuchFactory.float      #=> 87.2716908041922
MuchFactory.float(3)   #=> 2.5466638138805

MuchFactory.date       #=> #<Date: 4915123/2,0,2299161>
MuchFactory.time       #=> Wed Sep 07 10:37:22 -0500 2016
MuchFactory.datetime   #=> #<DateTime: 302518290593/43200,0,2299161>

MuchFactory.string     #=> "boxsrbazeq"
MuchFactory.string(3)  #=> "rja"
MuchFactory.symbol     #=> :sfdhortksj
MuchFactory.symbol(3)  #=> :emh
MuchFactory.text       #=> "khcwyizmymajfzzxlfwz"
MuchFactory.text(3)    #=> "qcy"
MuchFactory.slug       #=> "licia"
MuchFactory.slug(3)    #=> "luu"
MuchFactory.hex        #=> "48797adb33"
MuchFactory.hex(3)     #=> "2fe"
MuchFactory.url        #=> "/cdqz/hqeq/zbsl"
MuchFactory.email      #=> "vyojvtxght@gmrin.com"

MuchFactory.file_name  #=> "kagahm.ybb"
MuchFactory.path       #=> "jbzf/omyk/vbha"
MuchFactory.dir_path   #=> "fxai/lwnq/urqu"
MuchFactory.file_path  #=> "bcno/pzxg/gois/mpvlfo.wdr"

MuchFactory.binary     #=> "\000\000\003S"
MuchFactory.boolean    #=> false
```

You can also extend on your own factory class:

```ruby
module Factory
  extend MuchFactory

  def self.data
    { Factory.string => Factory.string }
  end
end
```

## Installation

Add this line to your application's Gemfile:

    gem "much-factory"

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install much-factory

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
