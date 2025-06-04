defmodule ExEcc.BLS.PointCompression do
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.BLS.Constants, as: Constants
  alias ExEcc.IntegerMath, as: IntegerMath
  alias ExEcc.FieldMath, as: FieldMath
  import Bitwise

  @doc """
  The most-significant three bits of a G1 or G2 encoding should be masked away before
  the coordinate(s) are interpreted.
  These bits are used to unambiguously represent the underlying element
  The format: (c_flag, b_flag, a_flag, x)
  https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md#bls12-381-instantiation  # noqa: E501
  """
  def get_flags(z) do
    # The most significant bit.
    c_flag = z >>> 383 &&& 1
    # The second-most significant bit.
    b_flag = z >>> 382 &&& 1
    # The third-most significant bit.
    a_flag = z >>> 381 &&& 1
    {c_flag, b_flag, a_flag}
  end

  @doc """
  If z2 is None, the given z1 is a G1 point.
  Else, (z1, z2) is a G2 point.
  """
  def is_point_at_infinity(z1, z2 \\ nil) do
    rem(z1, Constants.pow_2_381()) == 0 and (z2 == nil or z2 == 0)
  end

  @doc """
  If z2 is None, the given z1 is a G1 point.
  Else, (z1, z2) is a G2 point.
  """
  def compress_g1(pt = {_x, _y, _z}) do
    if Curve.is_inf(pt) do
      # Set c_flag = 1 and b_flag = 1. leave a_flag = x = 0
      Constants.pow_2_383() + Constants.pow_2_382()
    else
      {x, y} = Curve.normalize(pt)
      # Record y's leftmost bit to the a_flag
      a_flag = div(y.n * 2, Constants.q())
      # Set c_flag = 1 and b_flag = 0
      x.n + a_flag * Constants.pow_2_381() + Constants.pow_2_383()
    end
  end

  @doc """
  A compressed point is a 384-bit integer with the bit order
  (c_flag, b_flag, a_flag, x), where the c_flag bit is always set to 1,
  the b_flag bit indicates infinity when set to 1,
  the a_flag bit helps determine the y-coordinate when decompressing,
  and the 381-bit integer x is the x-coordinate of the point.
  """
  def decompress_g1(z) when is_integer(z) do
    {c_flag, b_flag, a_flag} = get_flags(z)

    # c_flag == 1 indicates the compressed form
    # MSB should be 1
    if c_flag != 1 do
      raise "c_flag should be 1"
    end

    is_inf_pt = is_point_at_infinity(z)

    if b_flag == 1 != is_inf_pt do
      raise "b_flag should be #{is_inf_pt}"
    end

    if is_inf_pt do
      # 3 MSBs should be 110
      if a_flag do
        raise "a point at infinity should have a_flag == 0"
      end

      Curve.z1()
    else
      # Else, not point at infinity
      # 3 MSBs should be 100 or 101
      x = rem(z, Constants.pow_2_381())

      if x >= Constants.q() do
        raise "x value should be less than field modulus. Got #{x}"
      end

      # Try solving y coordinate from the equation Y^2 = X^3 + b
      # using quadratic residue
      y =
        IntegerMath.pow(
          rem(x ** 3 + Curve.b().n, Constants.q()),
          div(Constants.q() + 1, 4),
          Constants.q()
        )

      if IntegerMath.pow(y, 2, Constants.q()) != rem(x ** 3 + Curve.b().n, Constants.q()) do
        raise "The given point is not on G1: y**2 = x**3 + b"
      end

      # Choose the y whose leftmost bit is equal to the a_flag
      y =
        if div(y * 2, Constants.q()) != a_flag do
          Constants.q() - y
        else
          y
        end

      {FQ.new(x), FQ.new(y), FQ.new(1)}
    end
  end

  @doc """
  Given value=``x``, returns the value ``y`` such that ``y**2 % q == x``,
  and None if this is not possible. In cases where there are two solutions,
  the value with higher imaginary component is favored;
  if both solutions have equal imaginary component the value with higher real
  component is favored.
  """
  def modular_squareroot_in_FQ2(value) do
    candidate_squareroot = value ** div(Constants.fq2_order() + 8, 16)
    check = candidate_squareroot ** 2 / value

    if check in Enum.take_every(Constants.eighth_roots_of_unity(), 2) do
      x1 =
        FieldMath.div(
          candidate_squareroot,
          Enum.at(
            Constants.eighth_roots_of_unity(),
            FieldMath.div(Enum.find_index(Constants.eighth_roots_of_unity(), &(&1 == check)), 2)
          )
        )

      x2 = FieldMath.neg(x1)
      {x1_re, x1_im} = x1.coeffs
      {x2_re, x2_im} = x2.coeffs

      if x1_im > x2_im or (x1_im == x2_im and x1_re > x2_re) do
        x1
      else
        x2
      end
    end
  end

  @doc """
  The compressed point (z1, z2) has the bit order:
  z1: (c_flag1, b_flag1, a_flag1, x1)
  z2: (c_flag2, b_flag2, a_flag2, x2)
  where
  - c_flag1 is always set to 1
  - b_flag1 indicates infinity when set to 1
  - a_flag1 helps determine the y-coordinate when decompressing,
  - a_flag2, b_flag2, and c_flag2 are always set to 0
  """
  def compress_g2(pt) do
    if not Curve.is_on_curve(pt, Curve.b2()) do
      raise "The given point is not on the twisted curve over FQ**2"
    end

    if Curve.is_inf(pt) do
      {Constants.pow_2_383() + Constants.pow_2_382(), 0}
    else
      {x, y} = Curve.normalize(pt)
      {x_re, x_im} = FieldMath.coeffs(x)
      {y_re, y_im} = FieldMath.coeffs(y)

      # Record the leftmost bit of y_im to the a_flag1
      # If y_im happens to be zero, then use the bit of y_re
      a_flag1 =
        if y_im > 0 do
          div(y_im * 2, Constants.q())
        else
          div(y_re * 2, Constants.q())
        end

      # Imaginary part of x goes to z1, real part goes to z2
      # c_flag1 = 1, b_flag1 = 0
      z1 = x_im + a_flag1 * Constants.pow_2_381() + Constants.pow_2_383()
      # a_flag2 = b_flag2 = c_flag2 = 0
      z2 = x_re
      {z1, z2}
    end
  end

  @doc """
  Recovers x and y coordinates from the compressed point (z1, z2).
  """
  def decompress_g2(p) do
    {z1, z2} = p
    {c_flag1, b_flag1, a_flag1} = get_flags(z1)

    # c_flag == 1 indicates the compressed form
    # MSB should be 1
    if c_flag1 != 1 do
      raise "c_flag should be 1"
    end

    is_inf_pt = is_point_at_infinity(z1, z2)

    if b_flag1 == 1 != is_inf_pt do
      raise "b_flag should be #{is_inf_pt}"
    end

    if is_inf_pt do
      # 3 MSBs should be 110
      if a_flag1 != 0 do
        raise "a point at infinity should have a_flag == 0"
      end

      Curve.z2()
    else
      # Else, not point at infinity
      # 3 MSBs should be 100 or 101
      x1 = rem(z1, Constants.pow_2_381())
      # Ensure that x1 is less than the field modulus.
      if x1 >= Constants.q() do
        raise "x1 value should be less than field modulus. Got #{x1}"
      end

      # Ensure that z2 is less than the field modulus.
      if z2 >= Constants.q() do
        raise "z2 point value should be less than field modulus. Got #{z2}"
      end

      x2 = z2
      # x1 is the imaginary part, x2 is the real part
      x = FQ2.new({x2, x1})
      y = modular_squareroot_in_FQ2(FieldMath.add(FieldMath.pow(x, 3), Curve.b2()))

      if y == nil do
        raise "Failed to find a modular squareroot"
      end

      # Choose the y whose leftmost bit of the imaginary part is equal to the a_flag1
      # If y_im happens to be zero, then use the bit of y_re
      {y_re, y_im} = FieldMath.coeffs(y)

      y =
        if (y_im > 0 and div(y_im * 2, Constants.q()) != a_flag1) or
             (y_im == 0 and div(y_re * 2, Constants.q()) != a_flag1) do
          FQ2.new(FieldMath.mul(y, -1) |> FieldMath.coeffs())
        else
          y
        end

      if not Curve.is_on_curve({x, y, FQ2.new({1, 0})}, Curve.b2()) do
        raise "The given point is not on the twisted curve over FQ**2"
      end

      {x, y, FQ2.new({1, 0})}
    end
  end
end
