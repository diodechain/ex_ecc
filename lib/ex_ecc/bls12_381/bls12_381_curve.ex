defmodule ExEcc.BLS12_381.BLS12381Curve do
  alias ExEcc.Fields.BLS12381FQ, as: FQ
  alias ExEcc.Fields.BLS12381FQ2, as: FQ2
  alias ExEcc.Fields.BLS12381FQ12, as: FQ12
  alias ExEcc.FieldMath

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bls12_381"].field_modulus
  def field_modulus, do: @field_modulus

  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13
  def curve_order, do: @curve_order

  # TODO: Port primality and factor checks if large number math is available
  # if :math.pow(2, @curve_order) |> round() |> rem(@curve_order) != 2 do
  #   raise ValueError, "Curve order is not prime"
  # end
  # if rem(:math.pow(@field_modulus, 12) - 1, @curve_order) != 0 do
  #   raise ValueError, "Curve order is not a factor of field_modulus**12 - 1"
  # end

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
    )
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
    })
  }
  def g2, do: @g2
  # Point at infinity over FQ
  @z1 nil
  def z1, do: @z1
  # Point at infinity for twisted curve over FQ2
  @z2 nil
  def z2, do: @z2

  def is_inf(pt) do
    pt == nil
  end

  def is_on_curve(pt, b) do
    if is_inf(pt) or is_nil(pt) do
      true
    else
      {x, y} = pt
      FieldMath.sub(FieldMath.pow(y, 2), FieldMath.pow(x, 3)) |> FieldMath.eq(b)
    end
  end

  # Elliptic curve doubling
  def double(pt) do
    if is_inf(pt) or is_nil(pt) do
      pt
    else
      {x, y} = pt
      m = FieldMath.div(FieldMath.mul(3, FieldMath.pow(x, 2)), FieldMath.mul(2, y))
      newx = FieldMath.pow(m, 2) |> FieldMath.sub(FieldMath.mul(2, x))

      newy =
        FieldMath.mul(FieldMath.neg(m), newx)
        |> FieldMath.add(FieldMath.mul(m, x))
        |> FieldMath.sub(y)

      {newx, newy}
    end
  end

  # Elliptic curve addition
  def add(p1, p2) do
    if is_inf(p1) or is_nil(p1) do
      p2
    else
      {x1, y1} = p1
      {x2, y2} = p2

      cond do
        FieldMath.eq(x1, x2) and FieldMath.eq(y1, y2) ->
          double(p1)

        FieldMath.eq(x1, x2) ->
          nil

        true ->
          m = FieldMath.div(FieldMath.sub(y2, y1), FieldMath.sub(x2, x1))
          newx = FieldMath.pow(m, 2) |> FieldMath.sub(x1) |> FieldMath.sub(x2)

          newy =
            FieldMath.mul(FieldMath.neg(m), newx)
            |> FieldMath.add(FieldMath.mul(m, x1))
            |> FieldMath.sub(y1)

          if not FieldMath.eq(
               newy,
               FieldMath.mul(FieldMath.neg(m), newx)
               |> FieldMath.add(FieldMath.mul(m, x2))
               |> FieldMath.sub(y2)
             ) do
            raise "Point addition is incorrect"
          end

          {newx, newy}
      end
    end
  end

  # Elliptic curve point multiplication
  def multiply(pt, n) do
    cond do
      n == 0 -> nil
      n == 1 -> pt
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    FieldMath.eq(p1, p2)
  end

  # "Twist" a point in E(FQ2) into a point in E(FQ12)
  @w FQ12.new(List.to_tuple([0, 1 | List.duplicate(0, 10)]))
  def w, do: @w

  # Convert P => -P
  def neg(pt) do
    if is_inf(pt) or is_nil(pt) do
      pt
    else
      {x, y} = pt
      {x, FieldMath.neg(y)}
    end
  end

  def twist(pt) do
    if is_inf(pt) or is_nil(pt) do
      pt
    else
      {x, y} = pt
      # Field isomorphism from Z[p] / x**2 to Z[p] / x**2 - 2*x + 2
      xcoeffs = [
        FieldMath.sub(FieldMath.coeffs(x, 0), FieldMath.coeffs(x, 1)),
        FieldMath.coeffs(x, 1)
      ]

      ycoeffs = [
        FieldMath.sub(FieldMath.coeffs(y, 0), FieldMath.coeffs(y, 1)),
        FieldMath.coeffs(y, 1)
      ]

      # Isomorphism into subfield of Z[p] / w**12 - 2 * w**6 + 2,
      # where w**6 = x
      nx =
        {Enum.at(xcoeffs, 0), 0, 0, 0, 0, 0, Enum.at(xcoeffs, 1), 0, 0, 0, 0, 0}
        |> FQ12.new()

      ny =
        {Enum.at(ycoeffs, 0), 0, 0, 0, 0, 0, Enum.at(ycoeffs, 1), 0, 0, 0, 0, 0}
        |> FQ12.new()

      # Divide x coord by w**2 and y coord by w**3
      {FieldMath.div(nx, FieldMath.pow(@w, 2)), FieldMath.div(ny, FieldMath.pow(@w, 3))}
    end
  end

  def g12, do: twist(@g2)
end

# defmodule ExEcc.BLS12_381.BLS12381CurveCheck do
#   use ExUnit.Case
#   alias ExEcc.BLS12_381.BLS12381Curve, as: Curve

#   if not Curve.is_on_curve(Curve.g12(), Curve.b12()) do
#     raise "Twist creates a point not on curve"
#   end
# end
