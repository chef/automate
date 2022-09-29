# Gyoku

Gyoku translates Ruby Hashes to XML.

``` ruby
Gyoku.xml(:find_user => { :id => 123, "v1:Key" => "api" })
# => "<findUser><id>123</id><v1:Key>api</v1:Key></findUser>"
```

[![Build status](https://github.com/savonrb/gyoku/actions/workflows/ci.yml/badge.svg)](https://github.com/savonrb/gyoku/actions/workflows/ci.yml)
[![Gem Version](https://badge.fury.io/rb/gyoku.svg)](http://badge.fury.io/rb/gyoku)
[![Code Climate](https://codeclimate.com/github/savonrb/gyoku.svg)](https://codeclimate.com/github/savonrb/gyoku)
[![Coverage Status](https://coveralls.io/repos/savonrb/gyoku/badge.svg?branch=master)](https://coveralls.io/r/savonrb/gyoku)


## Installation

Gyoku is available through [Rubygems](http://rubygems.org/gems/gyoku) and can be installed via:

``` bash
$ gem install gyoku
```

or add it to your Gemfile like this:

``` ruby
gem 'gyoku', '~> 1.0'
```


## Hash keys

Hash key Symbols are converted to lowerCamelCase Strings.

``` ruby
Gyoku.xml(:lower_camel_case => "key")
# => "<lowerCamelCase>key</lowerCamelCase>"
```

You can change the default conversion formula to `:camelcase`, `:upcase` or `:none`.  
Note that options are passed as a second Hash to the `.xml` method.

``` ruby
Gyoku.xml({ :camel_case => "key" }, { :key_converter => :camelcase })
# => "<CamelCase>key</CamelCase>"
```

Custom key converters. You can use a lambda/Proc to provide customer key converters.
This is a great way to leverage active support inflections for domain specific acronyms.

``` ruby
# Use camelize lower which will hook into active support if installed.
Gyoku.xml({ acronym_abc: "value" }, key_converter: lambda { |key| key.camelize(:lower) })
# => "<acronymABC>value</acronymABC>"

```

Hash key Strings are not converted and may contain namespaces.

``` ruby
Gyoku.xml("XML" => "key")
# => "<XML>key</XML>"
```


## Hash values

* DateTime objects are converted to xs:dateTime Strings
* Objects responding to :to_datetime (except Strings) are converted to xs:dateTime Strings
* TrueClass and FalseClass objects are converted to "true" and "false" Strings
* NilClass objects are converted to xsi:nil tags
* These conventions are also applied to the return value of objects responding to :call
* All other objects are converted to Strings using :to_s

## Array values

Array items are by default wrapped with the containiner tag, which may be unexpected.

``` ruby
> Gyoku.xml({languages: [{language: 'ruby'},{language: 'java'}]})
# => "<languages><language>ruby</language></languages><languages><language>java</language></languages>"
```

You can set the `unwrap` option to remove this behavior.

``` ruby
> Gyoku.xml({languages: [{language: 'ruby'},{language: 'java'}]}, { unwrap: true})
# => "<languages><language>ruby</language><language>java</language></languages>"
```

## Special characters

Gyoku escapes special characters unless the Hash key ends with an exclamation mark.

``` ruby
Gyoku.xml(:escaped => "<tag />", :not_escaped! => "<tag />")
# => "<escaped>&lt;tag /&gt;</escaped><notEscaped><tag /></notEscaped>"
```


## Self-closing tags

Hash Keys ending with a forward slash create self-closing tags.

``` ruby
Gyoku.xml(:"self_closing/" => "", "selfClosing/" => nil)
# => "<selfClosing/><selfClosing/>"
```


## Sort XML tags

In case you need the XML tags to be in a specific order, you can specify the order  
through an additional Array stored under the `:order!` key.

``` ruby
Gyoku.xml(:name => "Eve", :id => 1, :order! => [:id, :name])
# => "<id>1</id><name>Eve</name>"
```


## XML attributes

Adding XML attributes is rather ugly, but it can be done by specifying an additional  
Hash stored under the`:attributes!` key.

``` ruby
Gyoku.xml(:person => "Eve", :attributes! => { :person => { :id => 1 } })
# => "<person id=\"1\">Eve</person>"
```

## Explicit XML Attributes

In addition to using the `:attributes!` key, you may also specify attributes through keys beginning with an "@" sign.
Since you'll need to set the attribute within the hash containing the node's contents, a `:content!` key can be used
to explicity set the content of the node. The `:content!` value may be a String, Hash, or Array.

This is particularly useful for self-closing tags.

**Using :attributes!**

``` ruby
Gyoku.xml(
  "foo/" => "", 
  :attributes! => {
    "foo/" => {
      "bar" => "1", 
      "biz" => "2", 
      "baz" => "3"
    }
  }
)
# => "<foo baz=\"3\" bar=\"1\" biz=\"2\"/>"
```

**Using "@" keys and ":content!"**

``` ruby
Gyoku.xml(
  "foo/" => {
    :@bar => "1",
    :@biz => "2",
    :@baz => "3",
    :content! => ""
  })
# => "<foo baz=\"3\" bar=\"1\" biz=\"2\"/>"
```

**Example using "@" to get Array of parent tags each with @attributes & :content!**

``` ruby
Gyoku.xml(
  "foo" => [
    {:@name => "bar", :content! => 'gyoku'}
    {:@name => "baz", :@some => "attr", :content! => 'rocks!'}
  ])
# => "<foo name=\"bar\">gyoku</foo><foo name=\"baz\" some=\"attr\">rocks!</foo>"
```

Unwrapping Arrays. You can specify an optional `unwrap` argument to modify the default Array
behavior. `unwrap` accepts a boolean flag (false by default) or an Array whitelist of keys to unwrap.
``` ruby
# Default Array behavior
Gyoku.xml({
  "foo" => [
    {:is => 'great' },
    {:is => 'awesome'}
  ]
})
# => "<foo><is>great</is></foo><foo><is>awesome</is></foo>"

# Unwrap Array behavior
Gyoku.xml({
  "foo" => [
    {:is => 'great' },
    {:is => 'awesome'}
  ]
}, unwrap: true)
# => "<foo><is>great</is><is>awesome</is></foo>"

# Unwrap Array, whitelist.
# foo is not unwrapped, bar is.
Gyoku.xml({
  "foo" => [
    {:is => 'great' },
    {:is => 'awesome'}
  ],
  "bar" => [
      {:is => 'rad' },
      {:is => 'cool'}
  ]
}, unwrap: [:bar])
# => "<foo><is>great</is></foo><foo><is>awesome</is></foo><bar><is>rad</is><is>cool</is></bar>"
```

Naturally, it would ignore :content! if tag is self-closing:

``` ruby
Gyoku.xml(
  "foo/" => [
    {:@name => "bar", :content! => 'gyoku'}
    {:@name => "baz", :@some => "attr", :content! => 'rocks!'}
  ])
# => "<foo name=\"bar\"/><foo name=\"baz\" some=\"attr\"/>"
```

This seems a bit more explicit with the attributes rather than having to maintain a hash of attributes.

For backward compatibility, `:attributes!` will still work. However, "@" keys will override `:attributes!` keys
if there is a conflict.

``` ruby
Gyoku.xml(:person => {:content! => "Adam", :@id! => 0})
# => "<person id=\"0\">Adam</person>"
```

**Example with ":content!", :attributes! and "@" keys**

``` ruby
Gyoku.xml({ 
  :subtitle => { 
    :@lang => "en", 
    :content! => "It's Godzilla!" 
  }, 
  :attributes! => { :subtitle => { "lang" => "jp" } } 
}
# => "<subtitle lang=\"en\">It's Godzilla!</subtitle>"
```

The example above shows an example of how you can use all three at the same time. 

Notice that we have the attribute "lang" defined twice.
The `@lang` value takes precedence over the `:attribute![:subtitle]["lang"]` value.

## Pretty Print

You can prettify the output XML to make it more readable. Use these options:
* `pretty_print` – controls pretty mode (default: `false`)
* `indent` – specifies indentation in spaces (default: `2`)
* `compact` – controls compact mode (default: `true`)

**This feature is not available for XML documents generated from arrays with unwrap option set to false as such documents are not valid**

**Examples**

``` ruby
puts Gyoku.xml({user: { name: 'John', job: { title: 'Programmer' }, :@status => 'active' }}, pretty_print: true)
#<user status='active'>
#  <name>John</name>
#  <job>
#    <title>Programmer</title>
#  </job>
#</user>
```

``` ruby
puts Gyoku.xml({user: { name: 'John', job: { title: 'Programmer' }, :@status => 'active' }}, pretty_print: true, indent: 4)
#<user status='active'>
#    <name>John</name>
#    <job>
#        <title>Programmer</title>
#    </job>
#</user>
```

``` ruby
puts Gyoku.xml({user: { name: 'John', job: { title: 'Programmer' }, :@status => 'active' }}, pretty_print: true, compact: false)
#<user status='active'>
#  <name>
#    John
#  </name>
#  <job>
#    <title>
#      Programmer
#    </title>
#  </job>
#</user>
```

**Generate XML from an array with `unwrap` option set to `true`**
``` ruby
puts Gyoku::Array.to_xml(["john", "jane"], "user", true, {}, pretty_print: true, unwrap: true)
#<user>
#  <user>john</user>
#  <user>jane</user>
#</user>
```

**Generate XML from an array with `unwrap` option unset (`false` by default)**
``` ruby
puts Gyoku::Array.to_xml(["john", "jane"], "user", true, {}, pretty_print: true)
#<user>john</user><user>jane</user>
```
