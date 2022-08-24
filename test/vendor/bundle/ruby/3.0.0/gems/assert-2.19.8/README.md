# The Assert Testing Framework

## Usage

```ruby
# in test/my_tests.rb

require "assert"

class MyTests < Assert::Context
  test "something" do
    assert_that(1).equals(1)
  end
end
```

```
$ assert test/my_tests.rb
Loaded suite (1 test)
Running tests in random order, seeded with "56382"
.

1 result: pass

(0.000128 seconds, 7812.500000 tests/s, 7812.500000 results/s)
```

## What Assert is

* **Framework**: you define tests and the context they run in - Assert runs them.  Everything is pure ruby so use any 3rd party testing tools you like.  Create 3rd party tools that extend Assert behavior.
* **First Class**: everything is a first class object and can be extended to your liking (and should be)
* **MVC**: tests and how they are defined (M) and executed (C) are distinct from how you view the test results (V).

## What Assert is not

* **RSpec/spec-anything**: define tests using assertion statements.
* **Unit/Functional/Integration/etc**: Assert is agnostic - you define whatever kinds of tests you like and Assert runs them in context.
* **Mock/Spec/BDD/etc**: Assert is the framework and there are a variety of 3rd party tools to do such things. Feel free to use whatever you like.

## Description

Assert is an assertion-style testing framework, meaning you use assertion statements to define your tests and create results. Assert uses class-based contexts so if you want to nest your contexts, use inheritance.

### Features

* `assert` [executable](https://github.com/redding/assert#cli) for running tests
* run tests by tab completing test file paths
* run only test files that have been modified
* random order test execution
* class-based contexts
* multiple before/setup & after/teardown blocks
* around blocks
* `let` value declarations
* full backtrace for errors
* optionally pretty print objects in failure descriptions
* [stubbing API](https://github.com/redding/assert#stub)
* [factory API](https://github.com/redding/assert#factory)
* `skip` to skip tests
* `Ctrl+c` stops tests and prints failures

## Defining tests

**Note**: Assert is tested using itself.  The tests are a good place to look for examples and usage patterns.

## Stub

Assert comes with a stubbing API - all it does is replace method calls.  In general it tries to be friendly and complain if stubbing doesn't match up with the object/method being stubbed:

* each stub takes a block that is called in place of the method
* complains if you stub a method that the object doesn't respond to
* complains if you stub with an arity mismatch
* no methods are added to `Object` to support stubbing
* stubs are auto-unstubbed on test teardown

**Note**: Assert's stubbing logic has been extracted into a separate gem: [MuchStub](https://github.com/redding/much-stub/#muchstub).  However, the main `Assert.{stub|unstub|stub_send|stub_tap|etc}` api is still available (it just proxies to MuchStub now).

```ruby
myclass = Class.new do
  def my_method
    "my_method"
  end

  def my_value(value)
    value
  end
end
my_object = myclass.new

my_object.my_method
  # => "my_method"
my_object.my_value(123)
  # => 123
my_object.my_value(456)
  # => 456

Assert.stub(my_object, :my_method)
my_object.my_method
  # => StubError: `my_method` not stubbed.
Assert.stub(my_object, :my_method){ "stub-meth" }
my_object.my_method
  # => "stub-meth"
my_object.my_method(123)
  # => StubError: arity mismatch
Assert.stub(my_object, :my_method).with(123){ "stub-meth" }
  # => StubError: arity mismatch

# Call the original method after it has been stubbed.
Assert.stub_send(my_object, :my_method)
  # => "my_method"

Assert.stub(my_object, :my_value){ "stub-meth" }
  # => StubError: arity mismatch
Assert.stub(my_object, :my_value).with(123){ |val| val.to_s }
my_object.my_value
  # => StubError: arity mismatch
my_object.my_value(123)
  # => "123"
my_object.my_value(456)
  # => StubError: `my_value(456)` not stubbed.

# Call the original method after it has been stubbed.
Assert.stub_send(my_object, :my_value, 123)
  # => 123
Assert.stub_send(my_object, :my_value, 456)
  # => 456

# Stub a method while preserving its behavior and return value.
my_value_called_with = nil
Assert.stub_tap(my_object, :my_value) { |value, *args|
  my_value_called_with = args
  Assert.stub(value, :to_s) { "FIREWORKS!" }
}
value = my_object.my_value(123)
  # => 123
my_value_called_with
  # => [123]
value.to_s
  # =>"FIREWORKS!"

# Unstub individual stubs
Assert.unstub(my_object, :my_method)
Assert.unstub(my_object, :my_value)

# OR blanket unstub all stubs
Assert.unstub!

my_object.my_method
  # => "my_method"
my_object.my_value(123)
  # => 123
value = my_object.my_value(456)
  # => 456
value.to_s
  # => "456"
```

## Factory

```ruby
require "assert/factory"

Assert::Factory.integer    #=> 15742
Assert::Factory.integer(3) #=> 2
Assert::Factory.float      #=> 87.2716908041922
Assert::Factory.float(3)   #=> 2.5466638138805

Assert::Factory.date       #=> #<Date: 4915123/2,0,2299161>
Assert::Factory.time       #=> Wed Sep 07 10:37:22 -0500 2016
Assert::Factory.datetime   #=> #<DateTime: 302518290593/43200,0,2299161>

Assert::Factory.string     #=> "boxsrbazeq"
Assert::Factory.string(3)  #=> "rja"
Assert::Factory.text       #=> "khcwyizmymajfzzxlfwz"
Assert::Factory.text(3)    #=> "qcy"
Assert::Factory.slug       #=> "licia"
Assert::Factory.slug(3)    #=> "luu"
Assert::Factory.hex        #=> "48797adb33"
Assert::Factory.hex(3)     #=> "2fe"
Assert::Factory.url        #=> "/cdqz/hqeq/zbsl"
Assert::Factory.email      #=> "vyojvtxght@gmrin.com"

Assert::Factory.file_name  #=> "kagahm.ybb"
Assert::Factory.path       #=> "jbzf/omyk/vbha"
Assert::Factory.dir_path   #=> "fxai/lwnq/urqu"
Assert::Factory.file_path  #=> "bcno/pzxg/gois/mpvlfo.wdr"

Assert::Factory.binary     #=> "\000\000\003S"
Assert::Factory.boolean    #=> false
```

`Assert::Factory` is an API for generating randomized data.  The randomization is tied to the runner seed so re-running tests with the same seed should produce the same random values.

You can also extend on your own factory class:

```ruby
module Factory
  extend Assert::Factory

  def self.data
    { Factory.string => Factory.string }
  end
end
```

**Note**: Assert's factory logic has been extracted into a separate gem: [MuchFactory](https://github.com/redding/much-factory/#muchfactory).  However, the main api above is still available (it just delegates to MuchFactory now).

## CLI

```sh
$ assert --help
```

Assert ships with a CLI for running tests.  Test files must end in `_tests.rb` (or `_test.rb`).  The CLI globs any given file path(s), requires any test files, and runs the tests in them.

As an example, say your test folder has a file structure like so:

```
- test
|  - basic_tests.rb
|  - helper.rb
|  - complex_tests.rb
|  - complex
|  |  - fast_tests.rb
|  |  - slow_tests.rb
```

* `$ assert` - runs all tests ("./test" is used if no paths are given)
* `$ assert test/basic` - run all tests in basic_tests.rb
* `$ assert test/complex/fast_tests.rb` - runs all tests in fast_tests.rb
* `$ assert test/basic test/comp` - runs all tests in basic_tests.rb, complex_tests.rb, fast_tests.rb and slow_tests.rb

All you need to do is pass some sort of existing file path (use tab-completion!) and Assert will find any test files and run the tests in them.

## Configuring Assert

```ruby
Assert.configure do |config|
  # set your config options here
end
```

Assert uses a config pattern for specifying settings.  Using this pattern, you can configure settings, extensions, custom views, etc.  Settings can be configured in 4 different scopes and are applied in this order: User, Local, ENV, CLI.

### User settings

Assert will look for and require the file `$HOME/.assert/init.rb`.  Use this file to specify user settings.  User settings can be overridden by Local, ENV, and CLI settings.

### Local settings

Assert will look for and require the file `./.assert.rb`.  Use this file to specify project settings.  Local settings can be overridden by ENV, and CLI settings.

To specify a custom local settings file path, use the `ASSERT_LOCALFILE` env var.

### ENV settings

Assert uses ENV vars to drive certain settings.  Use these vars to specify specific environment settings.  ENV settings can be overridden by CLI settings.

### CLI settings

Assert accepts options from its CLI.  Use these options to specify absolute runtime settings.  CLI settings are always applied last and cannot be overridden.

## Running Tests

Assert uses its [`Assert::Runner`](/lib/assert/runner.rb) to run tests.  You can extend this default runner or use your own runner implementation.  Specify it in your user/local settings:

```ruby
require "my_awesome_runner_class"

Assert.configure do |config|
  config.runner MyAwesomeRunnerClass.new
end
```

### Test Dir

By default Assert expects tests in the `test` dir.  The is where it looks for the helper file and is the default path used when running `$ assert`.  To override this dir, do so in your user/local settings file:

```ruby
Assert.configure do |config|
  config.test_dir "testing"
end
```

### Test Helper File

By default Assert will look for a file named `helper.rb` in the `test_dir` and require it (if found) just before running the tests.  To override the helper file name, do so in your user/local settings file:

```ruby
Assert.configure do |config|
  config.test_helper "some_helpers.rb"
end
```

### Test Order

The default runner object runs tests in random order.  To run tests in a consistant order, pass a custom runner seed:

In user/local settings file:

```ruby
Assert.configure do |config|
  config.runner_seed 1234
end
```

Using an ENV var:

```sh
$ ASSERT_RUNNER_SEED=1234 assert
```

Using the CLI:

```sh
$ assert [-s|--runner-seed] 1234
```

### Run a single test

You can choose to run just a single test so that you can focus on making it pass without the overhead/noise from the output of the other tests in the file.

It is recommended that you only use this setting from the CLI.  To run just a single test, pass its file path and line number:

```sh
$ assert [-t|--single-test] ./path/for/tests.rb:123
```

**Note**: by default Assert outputs, in the details of each non-passing result, the cmd to re-run the result's test:

```
FAIL: should pass
./test/unit/test_tests.rb:124
assert -t ./test/unit/test_tests.rb:123
```

### Verbose Output

By default, Assert shows terse runtime test result information.  It provides a setting to turn on/off more verbose information:

In user/local settings file:

```ruby
Assert.configure do |config|
  config.verbose true
end
```

Using the CLI:

```sh
$ assert [-v|--verbose|--no-verbose]
```

### Capture Output

By default, Assert shows any output on `$stdout` produced while running a test.  It provides a setting to override whether to show this output or to "capture" it and display it in the test result details:

In user/local settings file:

```ruby
Assert.configure do |config|
  config.capture_output true
end
```

Using the CLI:

```sh
$ assert [-o|--capture-output|--no-capture-output]
```

### Failure Handling

By default, Assert will halt test execution when a test produces a Fail result.  It provides a setting to override this default:

In user/local settings file:

```ruby
Assert.configure do |config|
  config.halt_on_fail false
end
```

Using the CLI:

```sh
$ assert [-h|--halt-on-fail|--no-halt-on-fail]
```

### Changed Only

By default, Assert loads every test file in the path(s) it is given and runs the tests in those files.  At times it is convenient to only run certain test files while you are actively developing on a feature.  Assert can detect which test files have changes and only load those files:

In user/local settings file:

```ruby
Assert.configure do |config|
  config.changed_only true  # not recommended - use the CLI with the `-c` flag
end
```

Using the CLI:

```sh
$ assert [-c|--changed-only|--no-changed-only]
```

You can also optionally send a "reference" value to evaluate changes against.  This can be any value and it is passed to the proc that detects changes (see below).  Using the default git changed proc, this value would be any commit reference (ie HEAD, master, etc).

Using the CLI:

```sh
$ assert -c [-r|--changed-ref] REF_VALUE
```

Note: This option has no effect unless used with the `-c` option.  One practical use of this is to test changes 1) after they have been staged (`assert -cr HEAD`) or 2) after they have been committed to a branch (`assert -cr master`) (for example).

#### Changed Test File Detection

The changed files are detected using two git commands by default:

```sh
git diff --no-ext-diff --name-only {ref}  # changed files
git ls-files --others --exclude-standard  # added files
```

The git cmds have ` -- #{test_paths}` appended to them to scope their results to just the test paths specified by the CLI and are run together using ` && `.  The `{ref}` above is any reference value given using the `-r` CLI opt.

This, of course, assumes you are working in a git repository.  If you are not or you want to use custom logic to determine the changed files, configure a custom proc.  The proc should take two parameters: the config and an array of test paths specified by the CLI.

```ruby
Assert.configure do |config|
  config.changed_proc Proc.new do |config, test_paths|
    `git diff --name-only master -- #{test_paths.join(" ")}`.split("\n")  # or whatever
  end
end
```

If you just want to disable this feature completely:

```ruby
Assert.configure do |config|
  # run nothing if the `-c` flag is given
  config.changed_proc Proc.new{ |config, test_paths| [] }

  # run all test paths if the `-c` flag is given
  config.changed_proc Proc.new{ |config, test_paths| test_paths }
end
```

### Pretty Printing values in fail messages

By default, Assert uses `inspect` when outputting value details in failure messages.  At times you may want to pretty-print complex objects as their inspect value is not very human-readable.  You can tell Assert to pretty print the assertion objects instead.

In user/local settings file:

```ruby
Assert.configure do |config|
  config.pp_objects true
end
```

Using the CLI:

```sh
$ assert [-p|--pp-objects|--no-pp-objects]
```

#### Default pretty print processor

Assert uses the stdlib's `PP.pp` to pretty print objects by default.  This is provided by the `Assert::Utils.stdlib_pp_proc` util.

Using the default `inspect` (no pretty print):

```
$ assert

FAIL: a test that fails should fail
Expected nil, not {:now_usec=>164055, :string=>"a really really really really really really really really long string", :now=>Thu Nov 14 10:28:49 -0600 2013}.
```

Using the default pretty printing (`Assert::Utils.stdlib_pp_proc`):

```
$ assert -p

FAIL: a test that fails should fail
Expected nil, not
{:now_usec=>45127,
 :string=>
  "a really really really really really really really really long string",
 :now=>Thu Nov 14 10:28:35 -0600 2013}
.
```

You can customize the width used by `PP.pp` by overriding the default `pp_proc` setting:

```ruby
# set PP.pp width used to 1
Assert.configure do |config|
  config.pp_proc Assert::Utils.stdlib_pp_proc(1)
end
```
```
$ assert -p

FAIL: a test that fails should fail
Expected nil, not
{:now_usec=>
  984698,
 :string=>
  "a really really really really really really really really long string",
 :now=>
  Thu Nov 14 10:30:52 -0600 2013}
.
```

You can provide you own custom pretty-print processor if you like:

```ruby
# set not a very useful pretty print processor
Assert.configure do |config|
  config.pp_proc Proc.new{ |obj| "herp derp" }
end
```
```
$ assert -p

FAIL: a test that fails should fail
Expected nil, not herp derp.
```

Use this if you prefer a 3rd-party tool (like awesome_print or something) over the stdlib `PP.pp` for pretty printing.

## Viewing Test Results

`Assert::DefaultView` is the default view for test results.  Its output goes something like this:

* before the run starts, output some info about the test suite that is about to run
* print out result abbreviations as the test results are generated
* after the run finishes...
 * display any result details (from failing or error results) in reverse test/result order
 * output some summary info

You can run a test suite and get a feel for what this default outputs.  The view has a few options you can tweak:

* `styled`: whether to apply ansi styles to the output, default `true`
* `pass_styles`: how to style pass result output, default `:green`
* `fail_styles`: default `:red, :bold`
* `error_styles`: default `:yellow, :bold`
* `skip_styles`: default `:cyan`
* `ignore_styles`: default: `:magenta`

To override an option, do so in your user/local settings:

```ruby
Assert.configure do |config|
  config.view.styled false
end
```

However, the view handler you use is itself configurable.  Define you own view handler class and specify it in your user/local settings:

```ruby
class MyCustomView < Assert::View
  # define your view here...
end

Assert.configure do |config|
  config.view MyCustomView.new
end
```

### Anatomy of a View

A view class handles the logic and templating of test result output.  A view class should inherit from `Assert::View`.  This defines default callback handlers for the test runner and gives access to a bunch of common helpers for reading test result data.

Each view should implement the callback handler methods to output information at different points during the running of a test suite.  Callbacks have access to any view methods and should output information using `puts` and `prints`.  See the `DefaultView` template for a usage example.

Available callbacks from the runner, and when they are called:

* `before_load`: at the beginning, before the suite is loaded, the test files are passed as an arg
* `after_load`: after the suite is loaded, just before `on_start`
* `on_start`: when a loaded test suite starts running
* `before_test`: before a test starts running, the test is passed as an arg
* `on_result`: when a running tests generates a result, the result is passed as an arg
* `after_test`: after a test finishes running, the test is passed as an arg
* `on_finish`: when the test suite is finished running
* `on_info`: called when the INFO signal is triggered while running the test suite (Ctrl-T on Macs)
* `on_interrupt`: called when the test suite is interrupted while running

Beyond that, each view can do as it sees fit.  Initialize how you wish, take whatever settings you'd like, and output results as you see fit, given the available callbacks.

### Using 3rd party views

To use a 3rd party custom view, first require it in and then configure it.  Assert provides a helper for requiring in views.  It can be used in two ways.  You can pass a fully qualified path to the helper and if it exists, will require it in.

```ruby
Assert::View.require_user_view "/path/to/my/view"
```

Alternatively, you can install/clone/copy/write your view implementations in `~/.assert/views` and require it in by name.  To have assert require it by name, have it installed at `~/assert/views/view_name/lib/view_name.rb` (this structure is compatible with popular conventions for rubygem development). For example:

```ruby
# assuming ~/.assert/views/my-custom-view/lib/my-custom-view.rb exists
# this will require it in
Assert::View.require_user_view "my-custom-view"
```

Once your view class is required in, use it and configure it just as you would any view.

## Assert Models

### Suite

A `Suite` object is reponsible for collecting and structuring tests and defines the set of tests to run using the test `Runner`.  Tests are grouped within the suite by their context.  Suite provides access to the contexts, tests, and test results.  In addition, the Suite model provides some stats (ie. run_time, runner_seed, etc...).

### Runner

A `Runner` object is responsible for running a suite of tests and firing event callbacks to the `View`.  Any runner object should take the test suite and view as arguments and should provide a "run" method that runs the tests and renders the view.

### Context

A `Context` object is the scope that tests are run in.  When tests are run, a new instance of the test context is created and the test code is evaluated within the scope of this context instance.  Context provides methods for defining tests and test callbacks and for generating test results in running tests.  Subclass context classes to achieve nested context behavior.

### Test

A `Test` object defines the test code that needs to be run and the results generated by that test code.  Tests are aware of their context and are responsible for running their code in context.

### Result

A `Result` object defines the data related to a test result.  There are a few kinds of test results available:

* `Pass`
* `Fail`
* `Error`
* `Skip`
* `Ignore`

Tests produce results as they are executed.  Every `assert` statement produces a result.  Some results, like `Error` and `Skip`, will halt execution.  `Pass` and `Ignore` results do not halt execution.  `Fail` results, by default, halt execution but there is an option to have them not halt execution.  Therefore, tests can have many results of varying types.

### View

A `View` object is responsible for rendering test result output.  Assert provides a `Assert::View` object to provide common helpers and default runner callback handlers for building views.  Assert also provides an `Assert::DefaultView` that it renders its output with.  See the "Viewing Test Results" section below for more details.

### Macro

Macros are procs that define sets of test code and make it available for easy reuse.  Macros work nicely with the "should" and "test" context methods.

## Installation

```
$ gem install assert
```

## Contributing

The source code is hosted on Github.  Feel free to submit pull requests and file bugs on the issues tracker.

If submitting a Pull Request, please:

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am "Added some feature"`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

One note: please respect that Assert itself is intended to be the flexible, base-level, framework-type logic that should change little if at all.  Pull requests for niche functionality or personal testing philosphy stuff will likely not be accepted.

If you wish to extend Assert for your niche purpose/desire/philosophy, please do so in its own gem (preferrably named `assert-<whatever>`) that uses Assert as a dependency.
