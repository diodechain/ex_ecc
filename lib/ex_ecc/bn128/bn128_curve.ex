defmodule ExEcc.Bn128.Bn128Curve do
  alias ExEcc.Fields.Bn128FQ, as: FQ
  alias ExEcc.Fields.Bn128FQP, as: FQP
  alias ExEcc.Fields.Bn128FQ2, as: FQ2
  alias ExEcc.Fields.Bn128FQ12, as: FQ12
  alias ExEcc.Fields.FieldProperties

  @field_modulus FieldProperties.field_properties()["bn128"]["field_modulus"]
  def field_modulus, do: @field_modulus

  @curve_order 21_888_242_871_839_275_222_246_405_745_257_275_088_548_364_400_416_034_343_698_204_186_575_808_495_617
  def curve_order, do: @curve_order

  # if not rem(2 ** @curve_order, @curve_order) == 2 do
  #   raise "Curve order is not prime"
  # end

  # if not rem(@field_modulus ** 12 - 1, @curve_order) == 0 do
  #   raise "Curve order is not a factor of field_modulus**12 - 1"
  # end

  # Curve is y**2 = x**3 + 3
  @b FQ.new(3)
  def b, do: @b
  # Twisted curve over FQ**2
  @b2 FieldMath.div(FQ2.new([3, 0]), FQ2.new([9, 1]))
  def b2, do: @b2
  # Extension curve over FQ**12; same b value as over FQ
  @b12 FQ12.new([3] ++ List.duplicate(0, 11))
  def b12, do: @b12

  # Generator for curve over FQ
  @g1 {FQ.new(1), FQ.new(2)}
  def g1, do: @g1

  @g2 {
    FQ2.new([
      108_570_469_990_230_571_359_445_707_622_328_294_813_707_563_595_785_180_869_905_199_932_856_558_527_81,
      115_597_320_329_863_871_079_910_040_213_922_857_839_258_128_618_211_925_309_174_031_514_523_918_056_34
    ]),
    FQ2.new([
      849_565_392_312_343_368_133_220_340_314_543_556_831_685_132_759_340_120_810_574_107_621_412_009_353_1,
      408_236_787_586_343_368_133_220_340_314_543_556_831_685_132_759_340_120_810_574_107_621_412_009_353_1
    ])
  }
  def g2, do: @g2

  # Point at infinity over FQ
  @z1 nil
  def z1, do: @z1
  # Point at infinity for twisted curve over FQ2
  @z2 nil
  def z2, do: @z2

  # Check if a point is the point at infinity
  def is_inf(pt) do
    is_nil(pt)
  end

  # Check that a point is on the curve defined by y**2 = x**3 + b
  def is_on_curve(pt, b) do
    if is_inf(pt) or is_nil(pt) do
      true
    else
      {x, y} = pt
      FieldMath.eq(b, y ** 2 - x ** 3)
    end
  end

  defmodule CheckIsOnCurve do
    if not is_on_curve(g1, b) do
      raise "G1 is not on the curve"
    end

    if not is_on_curve(g2, b2) do
      raise "G2 is not on the curve"
    end
  end

  # Elliptic curve doubling
  def double(pt) do
    if is_inf(pt) or is_nil(pt) do
      pt
    else
      {x, y} = pt
      m = 3 * x ** 2 / (2 * y)
      newx = m ** 2 - 2 * x
      newy = -m * newx + m * x - y
      {newx, newy}
    end
  end

  # Elliptic curve addition
  def add(p1, p2) do
    if is_inf(p1) or is_nil(p1) do
      p1
    else
      {x1, y1} = p1
      {x2, y2} = p2

      cond do
        x2 == x1 and y2 == y1 ->
          double(p1)

        x2 == x1 ->
          nil

        true ->
          m = (y2 - y1) / (x2 - x1)
          newx = m ** 2 - x1 - x2
          newy = -m * newx + m * x1 - y1

          if not newy == -m * newx + m * x2 - y2 do
            raise "Point addition is incorrect"
          end

          {newx, newy}
      end
    end
  end

  # Elliptic curve point multiplication
  def multiply(pt, n) do
    cond do
      n == 0 ->
        nil

      n == 1 ->
        pt

      rem(n, 2) == 0 ->
        multiply(double(pt), div(n, 2))

      true ->
        add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    p1 == p2
  end

  @w FQ12.new([0, 1] ++ List.duplicate(0, 10))
  def w, do: @w

  def neg(pt) do
    if is_inf(pt) or is_nil(pt) do
      pt
    else
      {x, y} = pt
      {x, -y}
    end
  end

  def twist(pt) do
    if is_inf(pt) or is_nil(pt) do
      pt
    else
      {x, y} = pt
      # Field isomorphism from Z[p] / x**2 to Z[p] / x**2 - 18*x + 82
      xcoeffs = [x.coeffs[0] - x.coeffs[1] * 9, x.coeffs[1]]
      ycoeffs = [y.coeffs[0] - y.coeffs[1] * 9, y.coeffs[1]]
      # Isomorphism into subfield of Z[p] / w**12 - 18 * w**6 + 82,
      # where w**6 = x
      nx = FQ12.new([xcoeffs[0]] ++ List.duplicate(0, 5) ++ [xcoeffs[1]] ++ List.duplicate(0, 5))
      ny = FQ12.new([ycoeffs[0]] ++ List.duplicate(0, 5) ++ [ycoeffs[1]] ++ List.duplicate(0, 5))
      # Divide x coord by w**2 and y coord by w**3
      {nx * w ** 2, ny * w ** 3}
    end
  end

  def g12, do: twist(g2)

  defmodule CheckTwist do
    if not is_on_curve(ExEcc.Bn128.Bn128Curve.g12(), ExEcc.Bn128.Bn128Curve.b12()) do
      raise "Twist creates a point not on curve"
    end
  end
end
