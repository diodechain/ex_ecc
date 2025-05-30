defmodule ExEcc.UtilsTest do
  use ExUnit.Case
  alias ExEcc.Utils

  describe "prime_field_inv/2" do
    test "returns 0 when a is 0" do
      assert Utils.prime_field_inv(0, 7) == 0
    end

    test "returns 0 when a equals n" do
      assert Utils.prime_field_inv(7, 7) == 0
    end

    test "returns 4 when a is 2 and n is 7" do
      assert Utils.prime_field_inv(2, 7) == 4
    end

    test "returns 5 when a is 10 and n is 7" do
      assert Utils.prime_field_inv(10, 7) == 5
    end
  end

  describe "deg/1" do
    test "returns 0 for empty polynomial" do
      assert Utils.deg([]) == 0
    end

    test "returns 0 for constant polynomial" do
      assert Utils.deg([1]) == 0
    end

    test "returns 1 for linear polynomial" do
      assert Utils.deg([1, 2]) == 1
    end

    test "returns 2 for quadratic polynomial" do
      assert Utils.deg([1, 2, 3]) == 2
    end

    test "ignores trailing zeros" do
      assert Utils.deg([1, 2, 0]) == 1
    end
  end
end
