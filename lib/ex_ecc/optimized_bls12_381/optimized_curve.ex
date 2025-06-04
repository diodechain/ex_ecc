defmodule ExEcc.OptimizedBLS12381.OptimizedCurve do
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  alias ExEcc.FieldMath

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bls12_381"].field_modulus
  def field_modulus, do: @field_modulus

  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13
  def curve_order, do: @curve_order

  # Curve is y**2 = x**3 + 4
  @b FQ.new(4)
  def b, do: @b
  # Twisted curve over FQ**2
  @b2 FQ2.new({4, 4})
  def b2, do: @b2
  # Extension curve over FQ**12; same b value as over FQ
  @b12 FQ12.new(List.to_tuple([4 | List.duplicate(0, 11)]))
  def b12, do: @b12

  # Generator for curve over FQ
  @g1 {
    FQ.new(
      368_541_675_371_338_701_678_108_831_518_307_775_796_162_079_578_254_640_989_457_837_868_860_759_237_837_631_883_605_494_767_634_582_154_810_418_546_4507
    ),
    FQ.new(
      133_950_654_494_447_647_302_047_137_994_192_122_158_493_387_593_834_962_042_654_373_641_651_142_395_633_350_647_272_465_535_336_653_499_239_175_644_1569
    ),
    FQ.new(1)
  }
  def g1, do: @g1

  # Generator for twisted curve over FQ2
  @g2 {
    FQ2.new({
      352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160,
      305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_5758
    }),
    FQ2.new({
      198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_7905,
      927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582
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
    {_x, _y, z} = pt
    FieldMath.eq(z, FieldMath.type(z).zero())
  end

  # Check that a point is on the curve defined by y**2 == x**3 + b
  def is_on_curve(pt, b) do
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt
      a = FieldMath.sub(FieldMath.mul(FieldMath.pow(y, 2), z), FieldMath.pow(x, 3))
      b = FieldMath.mul(b, FieldMath.pow(z, 3))

      FieldMath.eq(
        a,
        b
      )

      # FieldMath.eq(
      #   FieldMath.sub(FieldMath.mul(FieldMath.pow(y, 2), z), FieldMath.pow(x, 3)),
      #   FieldMath.mul(b, FieldMath.pow(z, 3))
      # )
    end
  end

  # Elliptic curve doubling
  def double(pt) do
    {x, y, z} = pt
    w = FieldMath.mul(3, FieldMath.mul(x, x))
    s = FieldMath.mul(y, z)
    b = FieldMath.mul(x, y) |> FieldMath.mul(s)
    h = FieldMath.sub(FieldMath.mul(w, w), FieldMath.mul(8, b))
    s_squared = FieldMath.mul(s, s)
    newx = FieldMath.mul(2, FieldMath.mul(h, s))

    newy =
      FieldMath.sub(
        FieldMath.sub(
          FieldMath.mul(w, FieldMath.sub(FieldMath.mul(4, b), h)),
          FieldMath.mul(8, FieldMath.mul(y, y)) |> FieldMath.mul(s_squared)
        ),
        s_squared
      )

    newz = FieldMath.mul(8, s) |> FieldMath.mul(s_squared)
    {newx, newy, newz}
  end

  # Elliptic curve addition
  def add(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    {one, zero} = {FieldMath.type(x1).one(), FieldMath.type(x1).zero()}

    if FieldMath.eq(z1, zero) or FieldMath.eq(z2, zero) do
      if FieldMath.eq(z2, zero) do
        p1
      else
        p2
      end
    else
      u1 = FieldMath.mul(y2, z1)
      u2 = FieldMath.mul(y1, z2)
      v1 = FieldMath.mul(x2, z1)
      v2 = FieldMath.mul(x1, z2)

      cond do
        FieldMath.eq(v1, v2) and FieldMath.eq(u1, u2) ->
          double(p1)

        FieldMath.eq(v1, v2) ->
          {one, one, zero}

        true ->
          u = FieldMath.sub(u1, u2)
          v = FieldMath.sub(v1, v2)
          v_squared = FieldMath.mul(v, v)
          v_squared_times_v2 = FieldMath.mul(v_squared, v2)
          v_cubed = FieldMath.mul(v, v_squared)
          w = FieldMath.mul(z1, z2)

          a =
            FieldMath.sub(
              FieldMath.sub(FieldMath.mul(u, u), v_cubed),
              FieldMath.mul(2, v_squared_times_v2)
            )

          newx = FieldMath.mul(v, a)

          newy =
            FieldMath.sub(
              FieldMath.sub(
                FieldMath.mul(u, FieldMath.sub(v_squared_times_v2, a)),
                FieldMath.mul(v_cubed, u2)
              ),
              u2
            )

          newz = FieldMath.mul(v_cubed, w)
          {newx, newy, newz}
      end
    end
  end

  # Elliptic curve point multiplication
  def multiply(pt, n) when is_integer(n) do
    {x, _y, _z} = pt

    cond do
      n == 0 -> {FieldMath.type(x).one(), FieldMath.type(x).one(), FieldMath.type(x).zero()}
      n == 1 -> pt
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2

    FieldMath.eq(FieldMath.mul(x1, z2), FieldMath.mul(x2, z1)) and
      FieldMath.eq(FieldMath.mul(y1, z2), FieldMath.mul(y2, z1))
  end

  def normalize(pt) do
    {x, y, z} = pt
    {FieldMath.div(x, z), %{} = FieldMath.div(y, z)}
  end

  @w FQ12.new(List.to_tuple([0, 1] ++ List.duplicate(0, 10)))
  def w, do: @w

  # Convert P => -P
  def neg(pt) do
    {x, y, z} = pt
    {x, FieldMath.neg(y), z}
  end

  def twist(pt) do
    {x, y, z} = pt
    # Field isomorphism from Z[p] / x**2 to Z[p] / x**2 - 2*x + 2
    xcoeffs = [FieldMath.coeffs(x, 0) - FieldMath.coeffs(x, 1) * 9, FieldMath.coeffs(x, 1)]
    ycoeffs = [FieldMath.coeffs(y, 0) - FieldMath.coeffs(y, 1) * 9, FieldMath.coeffs(y, 1)]
    zcoeffs = [FieldMath.coeffs(z, 0) - FieldMath.coeffs(z, 1) * 9, FieldMath.coeffs(z, 1)]

    nx =
      {List.first(xcoeffs), 0, 0, 0, 0, 0, List.last(xcoeffs), 0, 0, 0, 0, 0}
      |> FQ12.new()

    ny =
      {List.first(ycoeffs), 0, 0, 0, 0, 0, List.last(ycoeffs), 0, 0, 0, 0, 0}
      |> FQ12.new()

    nz =
      {List.first(zcoeffs), 0, 0, 0, 0, 0, List.last(zcoeffs), 0, 0, 0, 0, 0}
      |> FQ12.new()

    {FieldMath.mul(nx, FieldMath.pow(@w, 2)), FieldMath.mul(ny, FieldMath.pow(@w, 3)), nz}
  end

  def g12 do
    twist(@g2)
  end
end

# Check that the twist creates a point that is on the curve
defmodule ExEcc.OptimizedBLS12381.OptimizedCurve.CheckTwist do
  alias ExEcc.OptimizedBLS12381.OptimizedCurve

  if not OptimizedCurve.is_on_curve(OptimizedCurve.g12(), OptimizedCurve.b12()) do
    raise "Twist creates a point not on curve"
  end
end
