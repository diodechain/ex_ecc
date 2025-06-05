defmodule ExEcc.BLS.PointCompressionTest do
  use ExUnit.Case
  alias ExEcc.BLS.PointCompression, as: PC
  alias ExEcc.BLS.Constants, as: Constants
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  import Bitwise

  @pow_2_381 Constants.pow_2_381()
  @pow_2_382 Constants.pow_2_382()
  @pow_2_383 Constants.pow_2_383()
  @pow_2_384 Constants.pow_2_384()
  @q Constants.q()

  describe "G1 compression and decompression" do
    test "compresses and decompresses points correctly" do
      test_cases = [
        # On curve points
        {fn -> Curve.g1() end, true, false},
        {fn -> Curve.multiply(Curve.g1(), 1) end, true, false},
        {fn -> Curve.multiply(Curve.g1(), 2) end, true, false},
        {fn -> Curve.multiply(Curve.g1(), 3) end, true, false},
        {fn -> Curve.multiply(Curve.g1(), 5) end, true, false},
        # Infinity point but still on curve
        {fn -> Curve.z1() end, true, true},
        # Not on curve
        {fn -> {FQ.new(5566), FQ.new(5566), FQ.new(1)} end, false, nil}
      ]

      for {pt, on_curve, is_infinity} <- test_cases do
        pt = pt.()
        assert Curve.is_on_curve(pt, Curve.b()) == on_curve
        z = PC.compress_g1(pt)

        if on_curve do
          x = Integer.mod(z, @pow_2_381)
          c_flag = div(Integer.mod(z, @pow_2_384), @pow_2_383)
          b_flag = div(Integer.mod(z, @pow_2_383), @pow_2_382)
          a_flag = div(Integer.mod(z, @pow_2_382), @pow_2_381)

          assert x < @q
          assert c_flag == 1

          if is_infinity do
            assert b_flag == 1
            assert a_flag == 0
            assert x == 0
          else
            assert b_flag == 0
            {pt_x, pt_y} = Curve.normalize(pt)
            assert a_flag == div(pt_y.n * 2, @q)
            assert x == pt_x.n
          end

          # Correct flags should decompress correct x, y
          assert Curve.normalize(PC.decompress_g1(z)) == Curve.normalize(pt)
        else
          assert_raise RuntimeError, fn ->
            PC.decompress_g1(z)
          end
        end
      end
    end

    test "handles edge cases correctly" do
      compressed_g1 = PC.compress_g1(Curve.g1())
      compressed_z1 = PC.compress_g1(Curve.z1())

      test_cases = [
        # baseline
        {compressed_g1, nil},
        # set c_flag to 0
        {compressed_g1 &&& ~~~(1 <<< 383), "c_flag should be 1"},
        # set b_flag to 1
        {compressed_g1 ||| 1 <<< 382, "b_flag should be 0"},
        # set b_flag to 0
        {compressed_z1 &&& ~~~(1 <<< 382), "b_flag should be 1"},
        # set a_flag to 1
        {compressed_z1 ||| 1 <<< 381, "a point at infinity should have a_flag == 0"},
        # field modulus and c_flag
        {@q ||| 1 <<< 383, "x value should be less than field modulus"}
      ]

      for {z, error_message} <- test_cases do
        if error_message == nil do
          PC.decompress_g1(z)
        else
          assert_raise RuntimeError, fn ->
            PC.decompress_g1(z)
          end
        end
      end
    end
  end

  describe "G2 compression and decompression" do
    test "compresses and decompresses points correctly" do
      test_cases = [
        # On curve points
        {Curve.g2(), true, false},
        {Curve.multiply(Curve.g2(), 5), true, false},
        # Infinity point but still on curve
        {Curve.z2(), true, true},
        # Not on curve
        {{FQ2.new({5566, 5566}), FQ2.new({5566, 5566}), FQ2.new({1, 0})}, false, nil}
      ]

      for {pt, on_curve, is_infinity} <- test_cases do
        if on_curve do
          {z1, z2} = PC.compress_g2(pt)
          x1 = Integer.mod(z1, @pow_2_381)
          c_flag1 = div(Integer.mod(z1, @pow_2_384), @pow_2_383)
          b_flag1 = div(Integer.mod(z1, @pow_2_383), @pow_2_382)
          a_flag1 = div(Integer.mod(z1, @pow_2_382), @pow_2_381)
          x2 = Integer.mod(z2, @pow_2_381)
          c_flag2 = div(Integer.mod(z2, @pow_2_384), @pow_2_383)
          b_flag2 = div(Integer.mod(z2, @pow_2_383), @pow_2_382)
          a_flag2 = div(Integer.mod(z2, @pow_2_382), @pow_2_381)

          assert x1 < @q
          assert x2 < @q
          assert c_flag2 == 0
          assert b_flag2 == 0
          assert a_flag2 == 0
          assert c_flag1 == 1

          if is_infinity do
            assert b_flag1 == 1
            assert a_flag1 == 0
            assert x1 == 0
            assert x2 == 0
          else
            assert b_flag1 == 0
            {_, y} = Curve.normalize(pt)
            {_, y_im} = y.coeffs
            # TODO: need a case for y_im == 0
            assert a_flag1 == div(y_im * 2, @q)
          end

          # Correct flags should decompress correct x, y
          assert Curve.normalize(PC.decompress_g2({z1, z2})) == Curve.normalize(pt)
        else
          assert_raise RuntimeError, fn ->
            PC.compress_g2(pt)
          end
        end
      end
    end

    test "handles edge cases correctly" do
      compressed_g2 = PC.compress_g2(Curve.g2())
      compressed_z2 = PC.compress_g2(Curve.z2())

      test_cases = [
        # baseline
        {compressed_g2, nil},
        # set c_flag1 to 0
        {{compressed_g2 |> elem(0) &&& ~~~(1 <<< 383), compressed_g2 |> elem(1)},
         "c_flag should be 1"},
        # set b_flag1 to 1
        {{compressed_g2 |> elem(0) ||| 1 <<< 382, compressed_g2 |> elem(1)},
         "b_flag should be 0"},
        # set b_flag1 to 0
        {{compressed_z2 |> elem(0) &&& ~~~(1 <<< 382), compressed_z2 |> elem(1)},
         "b_flag should be 1"},
        # x1 == q
        {{@q ||| 1 <<< 383, compressed_z2 |> elem(1)},
         "x1 value should be less than field modulus"},
        # set a_flag1 to 1
        {{compressed_z2 |> elem(0) ||| 1 <<< 381, compressed_z2 |> elem(1)},
         "a point at infinity should have a_flag == 0"},
        # set c_flag2 to 1
        {{compressed_g2 |> elem(0), compressed_z2 |> elem(1) ||| 1 <<< 383},
         "z2 point value should be less than field modulus"},
        # set b_flag2 to 1
        {{compressed_g2 |> elem(0), compressed_z2 |> elem(1) ||| 1 <<< 382},
         "z2 point value should be less than field modulus"},
        # set a_flag2 to 1
        {{compressed_g2 |> elem(0), compressed_z2 |> elem(1) ||| 1 <<< 381},
         "z2 point value should be less than field modulus"},
        # z2 value >= field modulus
        {{compressed_g2 |> elem(0), (compressed_g2 |> elem(1)) + @q},
         "z2 point value should be less than field modulus"}
      ]

      for {z, error_message} <- test_cases do
        if error_message == nil do
          PC.decompress_g2(z)
        else
          assert_raise RuntimeError, fn ->
            PC.decompress_g2(z)
          end
        end
      end
    end
  end
end
