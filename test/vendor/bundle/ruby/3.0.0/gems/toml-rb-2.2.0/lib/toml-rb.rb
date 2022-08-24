require "citrus"

require_relative "toml-rb/errors"
require_relative "toml-rb/array"
require_relative "toml-rb/string"
require_relative "toml-rb/datetime"
require_relative "toml-rb/table"
require_relative "toml-rb/table_array"
require_relative "toml-rb/inline_table"
require_relative "toml-rb/keyvalue"
require_relative "toml-rb/parser"
require_relative "toml-rb/dumper"

File.dirname(File.expand_path(__FILE__)).tap do |root|
  Citrus.load "#{root}/toml-rb/grammars/helper.citrus"
  Citrus.load "#{root}/toml-rb/grammars/primitive.citrus"
  Citrus.load "#{root}/toml-rb/grammars/array.citrus"
  Citrus.load "#{root}/toml-rb/grammars/document.citrus"
end

module TomlRB
  # Public: Returns a hash from *TomlRB* content.
  #
  # content         - TomlRB string to be parsed.
  # :symbolize_keys - true | false (default: false).
  #
  #
  # Examples
  #
  #   TomlRB.parse('[group]')
  #   # => {"group"=>{}}
  #
  #   TomlRB.parse('title = "TomlRB parser"')
  #   # => {"title"=>"TomlRB parser"}
  #
  #   TomlRB.parse('[group]', symbolize_keys: true)
  #   # => {group: {}}
  #
  #   TomlRB.parse('title = "TomlRB parser"', symbolize_keys: true)
  #   # => {title: "TomlRB parser"}
  #
  #
  # Returns a Ruby hash representation of the content according to TomlRB spec.
  # Raises ValueOverwriteError if a key is overwritten.
  # Raises ParseError if the content has invalid TomlRB.
  def self.parse(content, symbolize_keys: false)
    Parser.new(content, symbolize_keys: symbolize_keys).hash
  end

  # Public: Returns a hash from a *TomlRB* file.
  #
  # path            - TomlRB File path
  # :symbolize_keys - true|false (optional).
  #
  #
  # Examples
  #
  #   TomlRB.load_file('/tmp/simple.toml')
  #   # => {"group"=>{}}
  #
  #   TomlRB.load_file('/tmp/simple.toml', symbolize_keys: true)
  #   # => {group: {}}
  #
  #
  # Returns a Ruby hash representation of the content.
  # Raises ValueOverwriteError if a key is overwritten.
  # Raises ParseError if the content has invalid TomlRB.
  # Raises Errno::ENOENT if the file cannot be found.
  # Raises Errno::EACCES if the file cannot be accessed.
  def self.load_file(path, symbolize_keys: false)
    TomlRB.parse(File.read(path), symbolize_keys: symbolize_keys)
  end

  # Public: Returns a *TomlRB* string from a Ruby Hash.
  #
  # hash - Ruby Hash to be dumped into *TomlRB*
  #
  #
  # Examples
  #
  #   TomlRB.dump(title: 'TomlRB dump')
  #   # => "simple = true\n"
  #
  #   hash = {
  #     "title"=>"wow!",
  #     "awesome"=> {
  #       "you"=>true,
  #       "others"=>false
  #     }
  #   }
  #
  #   TomlRB.dump(hash)
  #   # => "title = \"wow!\"\n[awesome]\nothers = false\nyou = true\n"
  #
  #
  # Returns a TomlRB string representing the hash.
  def self.dump(hash)
    Dumper.new(hash).toml_str
  end
end
