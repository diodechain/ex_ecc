defmodule ExEcc.OptimizedBN128.OptimizedCurve do
  alias ExEcc.Fields.OptimizedBN128FQ, as: FQ
  alias ExEcc.Fields.OptimizedBN128FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBN128FQ12, as: FQ12
  alias ExEcc.Fields.OptimizedFieldElements.FQP
  alias ExEcc.FieldMath

  # Curve is y**2 = x**3 + 3
  @b FQ.new(3)
  def b, do: @b

  # Twisted curve over FQ**2
  @b2 FieldMath.div(FQ2.new({3, 0}), FQ2.new({9, 1}))
  def b2, do: @b2

  # Extension curve over FQ**12; same b value as over FQ
  @b12 FQ12.new(List.to_tuple([3 | List.duplicate(FQ.zero(), 11)]))
  def b12, do: @b12

  # Generator for curve over FQ
  @g1 {FQ.new(1), FQ.new(2), FQ.new(1)}
  def g1, do: @g1

  # Generator for twisted curve over FQ2
  @g2 {
    FQ2.new({
      108_570_469_990_230_571_359_445_707_622_328_294_813_707_563_595_785_180_869_905_199_932_856_558_527_81,
      115_597_320_329_863_871_079_910_040_213_922_857_839_258_128_618_211_925_309_174_031_514_523_918_056_34
    }),
    FQ2.new({
      8_495_653_923_123_431_417_604_973_247_489_272_438_418_190_587_263_600_148_770_280_649_306_958_101_930,
      4_082_367_875_863_433_681_332_203_403_145_435_568_316_851_327_593_401_208_105_741_076_214_120_093_531
    }),
    FQ2.one()
  }
  def g2, do: @g2

  # Point at infinity over FQ
  @z1 {FQ.one(), FQ.one(), FQ.zero()}
  def z1, do: @z1

  # Point at infinity for twisted curve over FQ2
  @z2 {FQ2.one(), FQ2.one(), FQ2.zero()}
  def z2, do: @z2

  # Check if a point is the point at infinity
  def is_inf(pt) do
    {x, y, z} = pt
    z == FieldMath.zero(z)
  end

  # Check that a point is on the curve defined by y**2 == x**3 + b
  def is_on_curve(pt, b_val) do
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt
      y ** 2 * z - x ** 3 == b_val * z ** 3
    end
  end

  # Double a point on the curve
  def double(pt) do
    {x, y, z} = pt
    w = 3 * x * x
    s = y * z
    b = x * y * s
    h = w * w - 8 * b
    s_squared = s * s
    newx = 2 * h * s
    newy = w * (4 * b - h) - 8 * y * y * s_squared
    newz = 8 * s * s_squared
    {newx, newy, newz}
  end

  # Elliptic curve addition
  def add(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    {one, zero} = {FieldMath.one(x1), FieldMath.zero(x1)}

    if z1 == zero or z2 == zero do
      p1(if(z2 == zero))
    else
      p2
    else
      u1 = y2 * z1
      u2 = y1 * z2
      v1 = x2 * z1
      v2 = x1 * z2

      if v1 == v2 and u1 == u2 do
        double(p1)
      else
        if v1 == v2 do
          {one, one, zero}
        end

        u = u1 - u2
        v = v1 - v2
        v_squared = v * v
        v_squared_times_v2 = v_squared * v2
        v_cubed = v * v_squared
        w = z1 * z2
        a = u * u * w - v_cubed - 2 * v_squared_times_v2
        newx = v * a
        newy = u * (v_squared_times_v2 - a) - v_cubed * u2
        newz = v_cubed * w
        {newx, newy, newz}
      end
    end
  end

  # Elliptic curve point multiplication
  def multiply(pt, n) do
    {x, y, z} = pt

    cond do
      n == 0 -> {FieldMath.one(x), FieldMath.one(x), FieldMath.zero(x)}
      n == 1 -> pt
      not rem(n, 2) -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    x1 * z2 == x2 * z1 and y1 * z2 == y2 * z1
  end

  def normalize(pt) do
    {x, y, z} = pt
    {x / z, y / z}
  end

  @w FQ12.new(List.to_tuple([0, 1] ++ List.duplicate(FQ.zero(), 10)))
  def w, do: @w

  def neg(pt) do
    {x, y, z} = pt
    {x, -y, z}
  end

  def twist(pt) do
    {x, y, z} = pt
    xcoeffs = [x.coeffs[0] - x.coeffs[1] * 9, x.coeffs[1]]
    ycoeffs = [y.coeffs[0] - y.coeffs[1] * 9, y.coeffs[1]]
    zcoeffs = [z.coeffs[0] - z.coeffs[1] * 9, z.coeffs[1]]

    nx =
      FQ12.new(
        List.to_tuple(
          [xcoeffs[0]] ++
            List.duplicate(FQ.zero(), 5) ++ [xcoeffs[1]] ++ List.duplicate(FQ.zero(), 5)
        )
      )

    ny =
      FQ12.new(
        List.to_tuple(
          [ycoeffs[0]] ++
            List.duplicate(FQ.zero(), 5) ++ [ycoeffs[1]] ++ List.duplicate(FQ.zero(), 5)
        )
      )

    nz =
      FQ12.new(
        List.to_tuple(
          [zcoeffs[0]] ++
            List.duplicate(FQ.zero(), 5) ++ [zcoeffs[1]] ++ List.duplicate(FQ.zero(), 5)
        )
      )

    {nx * w ** 2, ny * w ** 3, nz}
  end

  def g12 do
    twist(@g2)
  end

  defmodule CheckTwist do
    alias ExEcc.OptimizedBN128.OptimizedCurve

    if not OptimizedCurve.is_on_curve(OptimizedCurve.g12(), OptimizedCurve.b12()) do
      raise "Twist creates a point not on curve"
    end
  end
end
