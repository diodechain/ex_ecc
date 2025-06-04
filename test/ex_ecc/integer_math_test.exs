defmodule ExEcc.IntegerMathTest do
  use ExUnit.Case
  alias ExEcc.IntegerMath

  test "pow" do
    assert IntegerMath.pow(2, 3, 10) == 8
    assert IntegerMath.pow(2, 3, 11) == 8

    # Test for huge numbers
    assert IntegerMath.pow(2, 100, 1000) == 376
    assert IntegerMath.pow(2, 101, 1000) == 752
    assert IntegerMath.pow(2, 1000, 1000) == 376
    assert IntegerMath.pow(2, 1001, 1000) == 752
    assert IntegerMath.pow(2, 100, 10000) == 5376
    assert IntegerMath.pow(2, 101, 10000) == 752
    assert IntegerMath.pow(2, 1000, 10000) == 9376
    assert IntegerMath.pow(2, 1001, 10000) == 8752
  end
end
