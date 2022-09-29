# MuchMixin

Enhanced mix-in API.

## Usage

```ruby
requre "much-mixin"

module MyMuchMixin
  include MuchMixin

  mixin_included do
    # do some stuff ...
    # - will be class eval'd in the scope of the receiver of `MyMuchMixin`
    # - will only be executed once per receiver, no matter how many times
    #   `MyMuchMixin` is included in that receiver
  end
end
```

Mix `MuchMixin` in on mix-ins. Define included hooks using `mixin_included` that will be class eval'd in the scope of the receiver.

This allows you to define multiple hooks separately and ensures each hook will only be executed once - even if your mix-in is mixed-in multiple times on the same receiver.

### `mixin_class_methods` / `mixin_instance_methods`

MuchMixin provides convenience methods for defining instance/class methods on receivers:

```ruby
requre "much-mixin"

module MyMuchMixin
  include MuchMixin

  mixin_class_methods do
    # define some methods ...
    # - these methods will become class methods on the receiver
  end

  mixin_instance_methods do
    # define some methods ...
    # - these methods will become instance methods on the receiver
  end
end
```

### `after_mixin_included`

These hooks work just like the `mixin_included` hooks, except they are evaluated _after_ any mix-in class/instance methods have been evaluated. E.g. use this to call a class method that the mix-in defines.

```ruby
requre "much-mixin"

module MyMuchMixin
  include MuchMixin

  after_mixin_included do
    configure_the_mixin
  end

  mixin_class_methods do
    def configure_the_mixin
      # ...
    end
  end
end
```

## Example

```ruby
requre "much-mixin"

module AnotherMixin
  def another
    "another"
  end
end

module MyMuchMixin
  include MuchMixin

  mixin_included do
    include AnotherMixin
  end

  mixin_class_methods do
    def a_class_method
      "a-class-method"
    end
  end

  mixin_instance_methods do
    def an_instance_method
      "an-instance-method"
    end
  end
end

class MyClass
  include MyMuchMixin
end

my_class = MyClass.new
my_class.another            # => "another"
my_class.an_instance_method # => "an-instance-method"
MyClass.a_class_method      # => "a-class-method"
```

## Installation

Add this line to your application's Gemfile:

    gem "much-mixin"

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install much-mixin

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
