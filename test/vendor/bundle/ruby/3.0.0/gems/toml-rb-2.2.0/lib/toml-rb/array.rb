module TomlRB
  module ArrayParser
    def value
      elements = captures[:array_elements].first
      elements ? elements.value : []
    end
  end
end
