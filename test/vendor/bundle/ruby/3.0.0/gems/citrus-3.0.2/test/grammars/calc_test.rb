require File.expand_path('../../helper', __FILE__)
require 'citrus/grammars'

Citrus.require 'calc'

class CalcTest < Test::Unit::TestCase
  # A helper method that tests the successful parsing and evaluation of the
  # given mathematical expression.
  def do_test(expr)
    match = ::Calc.parse(expr)
    assert(match)
    assert_equal(expr, match)
    assert_equal(expr.length, match.length)
    assert_equal(eval(expr), match.value)
  end

  def test_int
    do_test('3')
  end

  def test_float
    do_test('1.5')
  end

  def test_addition
    do_test('1+2')
  end

  def test_addition_multi
    do_test('1+2+3')
  end

  def test_addition_float
    do_test('1.5+3')
  end

  def test_subtraction
    do_test('3-2')
  end

  def test_subtraction_float
    do_test('4.5-3')
  end

  def test_multiplication
    do_test('2*5')
  end

  def test_multiplication_float
    do_test('1.5*3')
  end

  def test_division
    do_test('20/5')
  end

  def test_division_float
    do_test('4.5/3')
  end

  def test_complex
    do_test('7*4+3.5*(4.5/3)')
  end

  def test_complex_spaced
    do_test('7 * 4 + 3.5 * (4.5 / 3)')
  end

  def test_complex_with_underscores
    do_test('(12_000 / 3) * 2.5')
  end

  def test_modulo
    do_test('3 % 2 + 4')
  end

  def test_exponent
    do_test('2**9')
  end

  def test_exponent_float
    do_test('2**2.2')
  end

  def test_negative_exponent
    do_test('2**-3')
  end

  def test_exponent_exponent
    do_test('2**2**2')
  end

  def test_exponent_group
    do_test('2**(3+1)')
  end

  def test_negative
    do_test('-5')
  end

  def test_double_negative
    do_test('--5')
  end

  def test_complement
    do_test('~4')
  end

  def test_double_complement
    do_test('~~4')
  end

  def test_mixed_unary
    do_test('~-4')
  end

  def test_complex_with_negatives
    do_test('4 * -7 / (8.0 + 1_2)**2')
  end
end
