defmodule ExEcc.OptimizedBLS12381.OptimizedPairing do
  alias ExEcc.Fields.OptimizedFieldElements, as: FQ
  alias ExEcc.Fields.OptimizedFieldElements.FQP, as: FQ2
  alias ExEcc.Fields.Bls12381FQ12, as: FQ12
  alias ExEcc.Fields.OptimizedFieldElements.FQP, as: FQP
  alias ExEcc.Fields.FieldProperties

  @field_modulus FieldProperties.field_properties()["bls12_381"].field_modulus
  @fq2_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
  @fq12_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs

  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  # Original @b2 FQ2.new({4, 4}) -> FQP.new_fqp([FQ.new(4, @fm), FQ.new(4, @fm)], @fq2_coeffs, @fm)
  @b2 FQ2.new([4, 4], @fq2_modulus_coeffs, @field_modulus)

  # Original @b12 FQ12.new(Tuple.pad({4}, 12, 0))
  # This means coeffs are [4, 0, 0, ..., 0]
  @b12 FQ12.new([4 | List.duplicate(0, 11)], @field_modulus)

  @g1_x FQ.new(
          36_854_167_537_133_870_167_810_883_151_830_777_579_616_207_957_825_464_098_945_783_786_886_075_923_783_763_188_360_549_476_763_458_215_481_041_854_645_07,
          @field_modulus
        )
  @g1_y FQ.new(
          1_339_506_544_944_476_473_020_471_379_941_921_221_584_933_875_938_349_620_426_543_736_416_511_423_956_333_506_472_724_655_353_366_534_992_391_756_441_569,
          @field_modulus
        )
  @g1_z FQ.new(1, @field_modulus)
  @g1 {@g1_x, @g1_y, @g1_z}

  # G2 points are FQP structs (representing FQ2 elements) with integer coeffs
  @g2_x FQ2.new_fqp(
          [
            352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160,
            305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_5758
          ],
          @fq2_modulus_coeffs,
          @field_modulus
        )
  @g2_y FQ2.new_fqp(
          [
            198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_7905,
            927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582
          ],
          @fq2_modulus_coeffs,
          @field_modulus
        )
  # FQ2.one() -> FQP([1,0],...)
  @g2_z FQ2.new_fqp([1, 0], @fq2_modulus_coeffs, @field_modulus)
  @g2 {@g2_x, @g2_y, @g2_z}

  @z1 {FQ.new(1, @field_modulus), FQ.new(1, @field_modulus), FQ.new(0, @field_modulus)}
  # For @z2, FQP.one() and FQP.zero() are needed for optimized FQP
  @z2_one_coeffs [1 | List.duplicate(0, length(@fq2_modulus_coeffs) - 1)]
  @z2_zero_coeffs List.duplicate(0, length(@fq2_modulus_coeffs))
  @z2 {FQ2.new_fqp(@z2_one_coeffs, @fq2_modulus_coeffs, @field_modulus),
       FQ2.new_fqp(@z2_one_coeffs, @fq2_modulus_coeffs, @field_modulus),
       FQ2.new_fqp(@z2_zero_coeffs, @fq2_modulus_coeffs, @field_modulus)}

  # Helper to get the module (FQ or FQP) for point operations based on element type
  defp elem_FieldMath(%FQ{}), do: FQ
  defp elem_FieldMath(%FQP{}), do: FQP
  # FQP struct duck typing
  defp elem_FieldMath(%{coeffs: _coeffs, modulus_coeffs: _modulus_coeffs}), do: FQP
  # Default for safety, or raise error
  defp elem_FieldMath(_), do: FQ

  def is_inf(pt) do
    {_x, _y, z} = pt
    FieldMath = elem_FieldMath(z)
    # Compare with FQP.zero if z is FQP
    FieldMath.eq(z, FieldMath.zero(z.field_modulus, z.degree))
  end

  # b_val is an FQ struct
  def is_on_curve(pt, b_val) do
    if is_inf(pt) do
      true
    else
      # These are FQ structs
      {x, y, z} = pt
      # y^2 * z - x^3 == b_val * z^3  (projective)
      # All ops are FQ ops here because x,y,z are FQ for G1
      term1 = FQ.mul(FQ.mul(y, y), z)
      term2 = FQ.mul(FQ.mul(x, x), x)
      lhs = FQ.subtract(term1, term2)
      # Ensure b_val is an FQ struct for mul; if it's integer, convert
      b_fq = FQ.ensure_fq(b_val, @field_modulus)
      rhs = FQ.mul(b_fq, FQ.mul(FQ.mul(z, z), z))
      FQ.eq(lhs, rhs)
    end
  end

  def double(pt) do
    {x, y, z} = pt
    FieldMath = elem_FieldMath(x)
    _fm = x.field_modulus

    # Constants as FQ/FQP elements
    # For Optimized FQ, direct integers are used in FQP ops if not FQP elements themselves.
    # The optimized FQP operations handle integer scalars directly.
    fq_2 = FQ.new(2)
    fq_3 = FQ.new(3)
    fq_4 = FQ.new(4)
    fq_8 = FQ.new(8)

    # For G1 (FQ points): x, y, z are FQ structs
    # For G2 (FQP points)
    if FieldMath == FQ do
      # 3 * x^2
      w = FieldMath.mul(fq_3, FieldMath.mul(x, x))
      # y * z
      s = FieldMath.mul(y, z)
      # x * y * s
      big_b_val = FieldMath.mul(x, FieldMath.mul(y, s))
      # w^2 - 8 * B
      h = FieldMath.sub(FieldMath.mul(w, w), FieldMath.mul(fq_8, big_b_val))
      # s^2
      s_squared = FieldMath.mul(s, s)
      # 2 * h * s
      newx = FieldMath.mul(fq_2, FieldMath.mul(h, s))
      # w * (4B - h) - 8 * y^2 * s^2
      newy =
        FieldMath.sub(
          FieldMath.mul(w, FieldMath.sub(FieldMath.mul(fq_4, big_b_val), h)),
          FieldMath.mul(fq_8, FieldMath.mul(y, FieldMath.mul(y, s_squared)))
        )

      # 8 * s^3
      newz = FieldMath.mul(fq_8, FieldMath.mul(s, s_squared))
      {newx, newy, newz}
    else
      # Here x, y, z are FQP structs. Arithmetic should use FQP module methods.
      # The FQP methods in OptimizedFieldElements accept integers as scalars directly for some ops.
      # 3 * x^2
      w = FieldMath.mul(x, x) |> FieldMath.mul(3)
      # y * z
      s = FieldMath.mul(y, z)
      # x * y * s
      big_b_val = FieldMath.mul(x, FieldMath.mul(y, s))
      # w^2 - 8 * B
      h = FieldMath.sub(FieldMath.mul(w, w), FieldMath.mul(big_b_val, 8))
      # s^2
      s_squared = FieldMath.mul(s, s)
      # 2 * h * s
      newx = FieldMath.mul(h, s) |> FieldMath.mul(2)
      # w * (4B - h) - 8 * y^2 * s^2
      newy =
        FieldMath.sub(
          FieldMath.mul(w, FieldMath.sub(FieldMath.mul(big_b_val, 4), h)),
          FieldMath.mul(FieldMath.mul(y, FieldMath.mul(y, s_squared)), 8)
        )

      # 8 * s^3
      newz = FieldMath.mul(s, s_squared) |> FieldMath.mul(8)
      {newx, newy, newz}
    end
  end

  def add(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    # Assuming p1 and p2 are of the same type
    FieldMath = elem_FieldMath(x1)
    _fm = x1.field_modulus

    # FQ.one(fm) or FQP.one(...)
    one = FieldMath.one(FieldMath.field_modulus(), x1.degree, x1.modulus_coeffs)
    # FQ.zero(fm) or FQP.zero(...)
    zero = FieldMath.zero(FieldMath.field_modulus(), x1.degree)

    cond do
      # p1 is inf
      FieldMath.eq(z1, zero) ->
        p2

      # p2 is inf
      FieldMath.eq(z2, zero) ->
        p1

      true ->
        u1 = FieldMath.mul(y2, z1)
        u2 = FieldMath.mul(y1, z2)
        v1 = FieldMath.mul(x2, z1)
        v2 = FieldMath.mul(x1, z2)

        cond do
          FieldMath.eq(v1, v2) and FieldMath.eq(u1, u2) ->
            double(p1)

          # Point at infinity
          FieldMath.eq(v1, v2) ->
            {one, one, zero}

          true ->
            u = FieldMath.sub(u1, u2)
            v = FieldMath.sub(v1, v2)
            v_squared = FieldMath.mul(v, v)
            v_squared_times_v2 = FieldMath.mul(v_squared, v2)
            v_cubed = FieldMath.mul(v, v_squared)
            w_val = FieldMath.mul(z1, z2)
            # If FieldMath is FQ, integer 2 needs to be FQ(2)
            # If FieldMath is FQP, its mul handles integer scalars.
            two_val = if FieldMath == FQ, do: FQ.new(2), else: 2

            a_val =
              FieldMath.sub(
                FieldMath.sub(FieldMath.mul(FieldMath.mul(u, u), w_val), v_cubed),
                FieldMath.mul(v_squared_times_v2, two_val)
              )

            newx = FieldMath.mul(v, a_val)

            newy =
              FieldMath.sub(
                FieldMath.mul(u, FieldMath.sub(v_squared_times_v2, a_val)),
                FieldMath.mul(v_cubed, u2)
              )

            newz = FieldMath.mul(v_cubed, w_val)
            {newx, newy, newz}
        end
    end
  end

  def multiply(pt, n) when is_integer(n) do
    {_x, _y, z_coord} = pt
    FieldMath = elem_FieldMath(z_coord)
    _fm = z_coord.field_modulus

    # Determine degree and modulus_coeffs for zero point, handling FQ case where they are not in struct
    # Default to 1 for FQ like elements
    degree = Map.get(z_coord, :degree, 1)
    # Default for FQ like elements
    mod_coeffs = Map.get(z_coord, :modulus_coeffs, [1])

    inf_pt =
      {FieldMath.one(FieldMath.field_modulus(), degree, mod_coeffs),
       FieldMath.one(FieldMath.field_modulus(), degree, mod_coeffs),
       FieldMath.zero(FieldMath.field_modulus(), degree, mod_coeffs)}

    cond do
      n == 0 -> inf_pt
      n == 1 -> pt
      n < 0 -> multiply(neg(pt), -n)
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  # Points are {x,y,z} where coords are FQ or FQP structs
  def eq(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    # Assuming homogeneous coordinates
    FieldMath = elem_FieldMath(x1)

    FieldMath.eq(FieldMath.mul(x1, z2), FieldMath.mul(x2, z1)) and
      FieldMath.eq(FieldMath.mul(y1, z2), FieldMath.mul(y2, z1))
  end

  # Point {x,y,z} with FQ or FQP elements
  def normalize(pt) do
    {x, y, z} = pt
    FieldMath = elem_FieldMath(x)
    # Requires FQP.inv to be robust
    inv_z = FieldMath.inv(z)
    {FieldMath.mul(x, inv_z), FieldMath.mul(y, inv_z)}
  end

  # For @w, original was FQ12.new(Tuple.pad({0, 1}, 12, 0))
  # This is FQP([0,1,0,...0], fq12_mod_coeffs, fm)
  @w FQ12.new_fq12([0, 1 | List.duplicate(0, 10)], @field_modulus)

  def neg(pt) do
    {x, y, z} = pt
    FieldMath = elem_FieldMath(y)
    {x, FieldMath.neg(y), z}
  end

  # twist for optimized curve elements
  def twist(pt) do
    # pt is a G2 point {FQP, FQP, FQP}
    {x_fq2, y_fq2, z_fq2} = pt
    # Optimized FQP stores coeffs as integers.
    # x_fq2.coeffs is [re, im]
    # y_fq2.coeffs is [re, im]
    # z_fq2.coeffs is [re, im]

    # Field isomorphism from FQ2 (u^2=-1) to FQ2 (u^2 - xi = 0) is not directly applied here.
    # The py_ecc twist for optimized BLS12-381 is different and complex.
    # It involves (x * W**-2, y * W**-3), where W is an FQ12 element.
    # This requires FQP division/multiplication by FQ12 powers.
    # This current `twist` is likely from a different source or incorrect interpretation.
    # Reverting to a simpler placeholder or what py_ecc might do (mul by const FQ12 elements).

    # The original py_ecc twist for optimized does:
    # return (x * FQ12.one() / W2, y * FQ12.one() / W3, z * FQ12.one())
    # Where W2 = cls.FQ12_W**2, W3 = cls.FQ12_W**3
    # And FQ12_W is the FQ12 element [0,1,0,...0]

    # This requires FQP (representing FQ2) to be multiplied by FQ12 elements, resulting in FQ12.
    # This is not a simple coefficient shuffle as the old code implied.
    # For now, placeholder, as this needs careful implementation of FQP<->FQ12 ops.
    _ = {x_fq2, y_fq2, z_fq2}
    # Placeholder for actual twisted point {FQ12, FQ12, FQ12}
    nil
  end

  def w, do: @w
  def g2, do: @g2
  def z1, do: @z1
  def z2, do: @z2
  def g1, do: @g1
  def b2, do: @b2
  def b12, do: @b12
  def curve_order, do: @curve_order
  def fq12_modulus_coeffs, do: @fq12_modulus_coeffs
  def field_modulus, do: @field_modulus
  def fq2_modulus_coeffs, do: @fq2_modulus_coeffs
end
