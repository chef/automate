# frozen_string_literal: true

module MixinStuff
  def test_mixin_stuff
    "from mixin"
  end

  def mixednottestmeth
    "mixed in not test meth"
  end
end

class SuperStuff
  def superclass_stuff
    "from superclass"
  end

  def other_stuff
    "super not test meth"
  end

  def test_from_super
    "test from the super"
  end

  def test_repeated
    "repeated test from super"
  end
end

class SubStuff < SuperStuff
  include MixinStuff

  def test_subclass_stuff
    "from subclass"
  end

  def nottestmeth
    "not test meth"
  end

  def more_other_stuff
    "more other stuff"
  end

  def test_repeated
    "repeated test from sub"
  end
end
