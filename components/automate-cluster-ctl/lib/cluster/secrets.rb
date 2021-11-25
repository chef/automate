#
# Author:: Faizan Fulara (<ffulara@progress.com>)
# Copyright:: Copyright (c) Chef Software Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
require 'openssl'
require 'base64'
require 'json'
require 'singleton'

module AutomateCluster
  class SecretsError < StandardError; end
  class SecretKeyNotFoundError < StandardError; end

  class Secrets
    include Singleton
    include Enumerable

    attr_reader :data, :secrets, :secret_key

    def initialize
      @data = { }
    end

    # Version is currently not being used by anything other than to output into the secrets json
    # The goal for this is if we need to update the format of the secrets store we can increment
    # this handled loading the older version and/or migrate to the new version
    def version
      0
    end

    def self.from_file(key_file, file)
      if File.exist?(key_file)
        key = File.read(key_file)
      else
        AutomateCluster.logger.warn "Secret storage has not been configured yet, please run `chef-automate secrets init`"
        key = generate_key
      end

      store = self.instance
      store.load_key(key)

      store.from_file(file) if File.exist?(file)

      store
    end

    def from_file(file)
      load_json File.read(file)
    end

    def self.cipher
      OpenSSL::Cipher::AES.new(256, :CBC)
    end

    def self.generate_key
      Base64.encode64(cipher.random_key).chomp
    end

    # Parse the given text as json and load it into the store
    # Expected format for the json is `{ "version": 0, "secrets": { ... } }``
    def load_json(text)
      json = JSON.parse(text)
      @data = json['secrets']
    end

    def generate_key
      load_key Secrets.generate_key
    end

    def load_key(key)
      @secret_key = key
    end

    def rotate!
      tmpdata = {}

      # save the current secrets
      names.each { |name| tmpdata[name] = fetch(name) }

      # make a new secrets key
      generate_key

      # resave the previous secrets
      tmpdata.each { |k,v| set(k,v) }

      secret_key
    end

    def cipher
      @cipher ||= Secrets.cipher
    end

    def encrypt(value)
      cipher.encrypt
      cipher.key = Base64.decode64(secret_key || generate_key)
      iv = cipher.random_iv

      [Base64.encode64(iv).chomp, Base64.encode64(cipher.update(value) + cipher.final).chomp]
    end

    def decrypt(value)
      iv, encrypted = *value

      cipher.decrypt
      cipher.key = Base64.decode64(secret_key)
      cipher.iv = Base64.decode64(iv)

      cipher.update(Base64.decode64(encrypted)) + cipher.final
    end

    def names
      @data.keys
    end

    def fetch(name)
      return nil unless @data.key?(name)
      decrypt(@data[name])
    rescue OpenSSL::Cipher::CipherError => e
      raise SecretsError, "Unable to decrypt secrets with the given key, please verify that it's correct"
    end

    def set(name, value)
      @data[name] = encrypt(value)
    rescue OpenSSL::Cipher::CipherError => e
      raise SecretsError, "Unable to encrypt secrets with the given key, please verify that it's correct"
    end

    def delete(name)
      @data.delete(name)
    end

    def clear!
      @data = {}
    end

    def [](name)
      fetch(name)
    end

    def []=(name, value)
      set(name, value)
    end

    def each(&block)
      @data.keys.each do |k|
        block.call(k,fetch(k))
      end
    end

    def to_json(*args)
      { version: version, secrets: @data }.to_json(*args)
    end

    def save(file)
      File.open(file, 'w') do |fp|
        fp << to_json
      end
      FileUtils.chmod(0600, file)
    end
  end
end
