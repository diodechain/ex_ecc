defmodule ExEcc.OptimizedBLS12381.OptimizedCurve do
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  # FQP is not used in this file, so I'm commenting it out for now.
  # alias ExEcc.Fields.OptimizedBLS12381FQP, as: FQP

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bls12_381"].field_modulus
  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  # Curve order should be prime
  # TODO: Port this check if ExEcc.Math.pow/3 is available and handles large numbers.
  # if :math.pow(2, @curve_order) |> round() |> rem(@curve_order) != 2 do
  #   raise ValueError, "Curve order is not prime"
  # end
  # Curve order should be a factor of field_modulus**12 - 1
  # TODO: Port this check if ExEcc.Math.pow/2 is available and handles large numbers.
  # if rem(:math.pow(@field_modulus, 12) - 1, @curve_order) != 0 do
  #   raise ValueError, "Curve order is not a factor of field_modulus**12 - 1"
  # end

  # Curve is y**2 = x**3 + 4
  @b FQ.new(4)
  # Twisted curve over FQ**2
  @b2 FQ2.new({4, 4})
  # Extension curve over FQ**12; same b value as over FQ
  @b12 FQ12.new(Tuple.pad({4}, 12, 0))

  # Generator for curve over FQ
  @g1 {
    FQ.new(
      36_854_167_537_133_870_167_810_883_151_830_777_579_616_207_957_825_464_098_945_783_786_886_075_923_783_763_188_360_549_476_763_458_215_481_041_854_645_07
    ),
    FQ.new(
      1_339_506_544_944_476_473_020_471_379_941_921_221_584_933_875_938_349_620_426_543_736_416_511_423_956_333_506_472_724_655_353_366_534_992_391_756_441_569
    ),
    FQ.new(1)
  }
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
  # Point at infinity over FQ
  @z1 {FQ.one(), FQ.one(), FQ.zero()}
  # Point at infinity for twisted curve over FQ2
  @z2 {FQ2.one(), FQ2.one(), FQ2.zero()}

  # Check if a point is the point at infinity
  def is_inf(pt) do
    elem(pt, 2) == elem(pt, 2).__struct__.zero()
  end

  # Check that a point is on the curve defined by y**2 == x**3 + b
  def is_on_curve(pt, b) do
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt
      FQ.sub(FQ.mul(FQ.mul(y, y), z), FQ.mul(x, x, x)) == FQ.mul(b, FQ.mul(z, z, z))
    end
  end

  # TODO: Port these checks if is_on_curve is verified to work correctly with FQ2 types as well.
  # if not is_on_curve(@g1, @b) do
  #   raise ValueError, "Generator is not on curve"
  # end
  # if not is_on_curve(@g2, @b2) do
  #   raise ValueError, "Generator is not on twisted curve"
  # end

  # Elliptic curve doubling
  def double(pt) do
    {x, y, z} = pt
    w = FQ.mul(FQ.new(3), FQ.mul(x, x))
    s = FQ.mul(y, z)
    big_b = FQ.mul(x, FQ.mul(y, s))
    h = FQ.sub(FQ.mul(w, w), FQ.mul(FQ.new(8), big_b))
    s_squared = FQ.mul(s, s)
    newx = FQ.mul(FQ.new(2), FQ.mul(h, s))

    newy =
      FQ.sub(
        FQ.mul(w, FQ.sub(FQ.mul(FQ.new(4), big_b), h)),
        FQ.mul(FQ.new(8), FQ.mul(y, FQ.mul(y, s_squared)))
      )

    newz = FQ.mul(FQ.new(8), FQ.mul(s, s_squared))
    {newx, newy, newz}
  end

  # Elliptic curve addition
  def add(p1, p2) do
    one = elem(p1, 0).__struct__.one()
    zero = elem(p1, 0).__struct__.zero()

    cond do
      elem(p1, 2) == zero or elem(p2, 2) == zero ->
        if elem(p2, 2) == zero, do: p1, else: p2

      true ->
        {x1, y1, z1} = p1
        {x2, y2, z2} = p2
        u1 = FQ.mul(y2, z1)
        u2 = FQ.mul(y1, z2)
        v1 = FQ.mul(x2, z1)
        v2 = FQ.mul(x1, z2)

        cond do
          v1 == v2 and u1 == u2 ->
            double(p1)

          v1 == v2 ->
            {one, one, zero}

          true ->
            u = FQ.sub(u1, u2)
            v = FQ.sub(v1, v2)
            v_squared = FQ.mul(v, v)
            v_squared_times_v2 = FQ.mul(v_squared, v2)
            v_cubed = FQ.mul(v, v_squared)
            # Renamed from W to avoid conflict with w in double/1
            w_val = FQ.mul(z1, z2)
            # Renamed A to a_val
            a_val =
              FQ.sub(
                FQ.sub(FQ.mul(FQ.mul(u, u), w_val), v_cubed),
                FQ.mul(FQ.new(2), v_squared_times_v2)
              )

            newx = FQ.mul(v, a_val)
            newy = FQ.sub(FQ.mul(u, FQ.sub(v_squared_times_v2, a_val)), FQ.mul(v_cubed, u2))
            newz = FQ.mul(v_cubed, w_val)
            {newx, newy, newz}
        end
    end
  end

  # Elliptic curve point multiplication
  def multiply(pt, n) do
    cond do
      n == 0 ->
        {elem(pt, 0).__struct__.one(), elem(pt, 0).__struct__.one(),
         elem(pt, 0).__struct__.zero()}

      n == 1 ->
        pt

      rem(n, 2) == 0 ->
        multiply(double(pt), div(n, 2))

      true ->
        add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    FQ.mul(x1, z2) == FQ.mul(x2, z1) and FQ.mul(y1, z2) == FQ.mul(y2, z1)
  end

  def normalize(pt) do
    {x, y, z} = pt
    {FQ.div(x, z), FQ.div(y, z)}
  end

  # "Twist" a point in E(FQ2) into a point in E(FQ12)
  @w FQ12.new(Tuple.pad({0, 1}, 12, 0))

  # Convert P => -P
  def neg(pt) do
    {x, y, z} = pt
    {x, FQ.neg(y), z}
  end

  def twist(pt) do
    {_x, _y, _z} = pt
    # Field isomorphism from Z[p] / x**2 to Z[p] / x**2 - 2*x + 2
    # Assuming _x, _y, _z are FQ2 instances which are tuples {coeff0, coeff1}
    xcoeffs = {FQ.sub(elem(_x, 0), elem(_x, 1)), elem(_x, 1)}
    ycoeffs = {FQ.sub(elem(_y, 0), elem(_y, 1)), elem(_y, 1)}
    zcoeffs = {FQ.sub(elem(_z, 0), elem(_z, 1)), elem(_z, 1)}

    nx_coeffs =
      List.to_tuple(
        List.duplicate(0, 12)
        |> List.replace_at(1, elem(xcoeffs, 0))
        |> List.replace_at(7, elem(xcoeffs, 1))
      )

    ny_coeffs =
      List.to_tuple(
        List.duplicate(0, 12)
        |> List.replace_at(0, elem(ycoeffs, 0))
        |> List.replace_at(6, elem(ycoeffs, 1))
      )

    nz_coeffs =
      List.to_tuple(
        List.duplicate(0, 12)
        |> List.replace_at(3, elem(zcoeffs, 0))
        |> List.replace_at(9, elem(zcoeffs, 1))
      )

    nx = FQ12.new(nx_coeffs)
    ny = FQ12.new(ny_coeffs)
    nz = FQ12.new(nz_coeffs)
    {nx, ny, nz}
  end

  # Check that the twist creates a point that is on the curve
  # @g12 twist(@g2) # This will not work directly with module attributes that way due to compile time evaluation.
  # TODO: Create a test case for this check.
  # if not is_on_curve(@g12, @b12) do
  #   raise ValueError, "Twist creates a point not on curve"
  # end
end
