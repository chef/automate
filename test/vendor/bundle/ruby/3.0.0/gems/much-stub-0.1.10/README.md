# MuchStub

MuchStub is a stubbing API for replacing method calls on objects in test runs.  This is intended to be brought into testing environments and used in test runs to stub out external dependencies.

All it does is replace method calls.  In general it tries to be friendly and complain if stubbing doesn't match up with the object/method being stubbed:

* each stub takes a block that is called in place of the method
* complains if you stub a method that the object doesn't respond to
* complains if you stub with an arity mismatch
* no methods are added to `Object` to support stubbing

Note: this was originally implemented in and extracted from [Assert](https://github.com/redding/assert).

## Usage

```ruby
# Given this object/API

my_class =
  Class.new do
    def my_method
      "my-method"
    end

    def my_value(value)
      value
    end
  end
my_object = my_class.new

my_object.my_method
  # => "my-method"
my_object.my_value(123)
  # => 123
my_object.my_value(456)
  # => 456

# Create a new stub for the :my_method method

MuchStub.(my_object, :my_method)
my_object.my_method
  # => StubError: `my_method` not stubbed.
MuchStub.(my_object, :my_method){ "stubbed-method" }
my_object.my_method
  # => "stubbed-method"
my_object.my_method(123)
  # => StubError: arity mismatch
MuchStub.(my_object, :my_method).with(123){ "stubbed-method" }
  # => StubError: arity mismatch

# Call the original method after it has been stubbed.

MuchStub.stub_send(my_object, :my_method)
  # => "my-method"

# Create a new stub for the :my_value method

MuchStub.(my_object, :my_value){ "stubbed-method" }
  # => StubError: arity mismatch
MuchStub.(my_object, :my_value).with(123){ |val| val.to_s }
my_object.my_value
  # => StubError: arity mismatch
my_object.my_value(123)
  # => "123"
my_object.my_value(456)
  # => StubError: `my_value(456)` not stubbed.

# Call the original method after it has been stubbed.

MuchStub.stub_send(my_object, :my_value, 123)
  # => 123
MuchStub.stub_send(my_object, :my_value, 456)
  # => 456

# Unstub individual stubs

MuchStub.unstub(my_object, :my_method)
MuchStub.unstub(my_object, :my_value)

# OR blanket unstub all stubs

MuchStub.unstub!

# The original API/behavior is preserved after unstubbing

my_object.my_method
  # => "my-method"
my_object.my_value(123)
  # => 123
my_object.my_value(456)
  # => 456
```

### Stubs for spying

```ruby
# Given this object/API

my_class =
  Class.new do
    def basic_method(value)
      value
    end

    def iterator_method(items, &block)
      items.each(&block)
    end
  end
my_object = my_class.new

# Store method call arguments/blocks for spying.

basic_method_called_with = nil
MuchStub.(my_object, :basic_method) { |*args|
  basic_method_called_with = MuchStub::Call.new(*args)
}
# OR
MuchStub.(my_object, :basic_method).on_call { |call|
  basic_method_called_with = call
}
# OR
MuchStub.on_call(my_object, :basic_method) { |call|
  # MucStub.on_call(...) { ... } is equivalent to
  # MuchStub.(...).on_call { ... }
  basic_method_called_with = call
}

my_object.basic_method(123)
basic_method_called_with.args
  # => [123]

basic_method_called_with = nil
MuchStub.(my_object, :basic_method).with(4, 5, 6) { |*args|
  basic_method_called_with = MuchStub::Call.new(*args)
}
# OR
MuchStub.(my_object, :basic_method).with(4, 5, 6).on_call { |call|
  basic_method_called_with = call
}

my_object.basic_method(4, 5, 6)
basic_method_called_with.args
  # => [4,5,6]

iterator_method_called_with = nil
MuchStub.(my_object, :iterator_method) { |*args, &block|
  iterator_method_called_with = MuchStub::Call.new(*args)
}
# OR
MuchStub.(my_object, :iterator_method).on_call { |call|
  iterator_method_called_with = call
}

my_object.iterator_method([1, 2, 3], &:to_s)
iterator_method_called_with.args
  # => [[1, 2, 3]]
iterator_method_called_with.block
  # => #<Proc:0x00007fb083a6feb0(&:to_s)>

# Count method calls for spying.

basic_method_call_count = 0
MuchStub.(my_object, :basic_method) {
  basic_method_call_count += 1
}

my_object.basic_method(123)
basic_method_call_count
  # => 1

# Count method calls and store arguments for spying.

basic_method_calls = []
MuchStub.(my_object, :basic_method) { |*args|
  basic_method_calls << MuchStub::Call.new(*args)
}
# OR
MuchStub.(my_object, :basic_method).on_call { |call|
  basic_method_calls << call
}

my_object.basic_method(123)
basic_method_calls.size
  # => 1
basic_method_calls.first.args
  # => [123]
```

### Stubs for test doubles.

```ruby
# Given this object/API ...

my_class =
  Class.new do
    def build_thing(thing_value);
      Thing.new(value)
    end
  end
my_object = my_class.new

# ... and this Test Double.
class FakeThing
  attr_reader :built_with

  def initialize(*args)
    @built_with = args
  end
end

# Stub in the test double.

MuchStub.(my_object, :build_thing) { |*args|
  FakeThing.new(*args)
}

thing = my_object.build_thing(123)
thing.built_with
  # => [123]
```

### `MuchStub.tap`

Use the `.tap` method to spy on method calls while preserving the original method return value and behavior.

```ruby
# Given this object/API

my_class =
  Class.new do
    def basic_method(value)
      value.to_s
    end
  end
my_object = my_class.new

# Normal stubs override the original behavior and return value...
basic_method_called_with = nil
MuchStub.(my_object, :basic_method) { |*args|
  basic_method_called_with = args
}

# ... in this case not converting the value to a String and returning it and
# instead returning the arguments passed to the method.
my_object.basic_method(123)
  # => [123]
basic_method_called_with
  # => [123]

# Use `MuchStub.tap` to preserve the methods behavior and also spy.

basic_method_called_with = nil
MuchStub.tap(my_object, :basic_method) { |value, *args|
  basic_method_called_with = MuchStub::Call.new(*args)
}
# OR
MuchStub.tap_on_call(my_object, :basic_method) { |value, call|
  basic_method_called_with = call
}

my_object.basic_method(123)
  # => "123"
basic_method_called_with.args
  # => [123]
```

#### Late-bound stubs using `MuchStub.tap`

Use the `.tap` method to stub any return values of method calls.

```ruby
# Given:

class Thing
  attr_reader :value

  def initialize(value)
    @value = value
  end
end

my_class =
  Class.new do
    def thing(value)
      Thing.new(value)
    end
  end
my_object = my_class.new

# Use `MuchStub.tap` to stub any thing instances created by `my_object.thing`
# (and also spy on the call arguments)

thing_built_with = nil
MuchStub.tap(my_object, :thing) { |thing, *args|
  thing_built_with = args
  MuchStub.(thing, :value) { 456 }
}

thing = my_object.thing(123)
  # => #<Thing:0x00007fd5ca9df510 @value=123>
thing_built_with
  # => [123]
thing.value
  # => 456
```

### `MuchStub.spy`

Use the `.spy` method to spy on method calls. This is especially helpful for spying on _chained_ method calls.

```ruby
# Given this object/API

myclass =
  Class.new do
    def one; self; end
    def two(val); self; end
    def three; self; end
    def ready?; false; end
  end
myobj = myclass.new

spy = MuchStub.spy(myobj :one, :two, :three, ready?: true)

assert_equal spy, myobj.one
assert_equal spy, myobj.two("a")
assert_equal spy, myobj.three

assert_true myobj.one.two("b").three.ready?

assert_kind_of MuchStub::CallSpy, spy
assert_equal 2, spy.one_call_count
assert_equal 2, spy.two_call_count
assert_equal 2, spy.three_call_count
assert_equal 1, spy.ready_predicate_call_count
assert_equal ["b"], spy.two_last_called_with.args
assert_true spy.ready_predicate_called?
```

## Installation

Add this line to your application's Gemfile:

    gem "much-stub"

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install much-stub

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am "Added some feature"`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
