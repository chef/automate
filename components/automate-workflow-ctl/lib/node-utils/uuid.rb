module NodeUtils
  module UUID
    def regex
      %r{[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}}
    end
    module_function :regex
  end
end
