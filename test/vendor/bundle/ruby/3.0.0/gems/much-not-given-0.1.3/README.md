# MuchNotGiven

Add "not given" default values to your objects. This allows you to safely detect whether a method has been given argument values or not.

## Usage

```ruby
module MyNamespace
  include MuchNotGiven
end

MyNamespace.not_given                           # => MyNamespace.not_given
"some value" == MyNamespace.not_given           # => false
MyNamespace.not_given?("some value")            # => false
MyNamespace.not_given?(MyNamespace.not_given?)  # => true
MyNamespace.given?("some value")                # => true
MyNamespace.given?(MyNamespace.not_given?)      # => false

def my_method(value = MyNamespace.not_given)
  if MyNamespace.given?(value)
    # do something with the given value
  end
end

def my_method(arg_value = MyNamespace.not_given)
  value = MyNamespace.given?(value) ? value : "some default value"

  # do something with the optionally defaulted value
end
```

## Installation

Add this line to your application's Gemfile:

    gem "much-not-given"

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install much-not-given

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am "Added some feature"`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
