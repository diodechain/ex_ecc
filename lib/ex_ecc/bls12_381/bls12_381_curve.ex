defmodule ExEcc.Bls12381.Curve do
  alias ExEcc.Fields.FieldElements, as: FQ
  alias ExEcc.Fields.FieldElements.FQP, as: FQP
  alias ExEcc.Fields.FieldProperties

  @bls12_381_field_modulus FieldProperties.field_properties()["bls12_381"].field_modulus
  @bls12_381_fq2_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
  @bls12_381_fq12_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs

  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  @b FQ.new_fq(4, @bls12_381_field_modulus)

  @b2_coeffs [FQ.new_fq(4, @bls12_381_field_modulus), FQ.new_fq(4, @bls12_381_field_modulus)]
  @b2 FQP.new_fqp(@b2_coeffs, @bls12_381_fq2_modulus_coeffs, @bls12_381_field_modulus)

  @b12_coeffs List.duplicate(FQ.zero(@bls12_381_field_modulus), 12) |> List.replace_at(0, FQ.new_fq(4, @bls12_381_field_modulus))
  @b12 FQP.new_fqp(@b12_coeffs, @bls12_381_fq12_modulus_coeffs, @bls12_381_field_modulus)

  @g1 {FQ.new_fq(
         368_541_675_371_338_701_678_108_831_518_307_775_796_162_079_578_254_640_989_457_837_868_860_759_237_837_631_883_605_494_767_634_582_154_810_418_546_450_7,
         @bls12_381_field_modulus
       ),
       FQ.new_fq(
         133_950_654_494_447_647_302_047_137_994_192_122_158_493_387_593_834_962_042_654_373_641_651_142_395_633_350_647_272_465_535_336_653_499_239_175_644_156_9,
         @bls12_381_field_modulus
       )}

  @g2_x_re FQ.new_fq(
    352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160,
    @bls12_381_field_modulus
  )
  @g2_x_im FQ.new_fq(
    305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_575_8,
    @bls12_381_field_modulus
  )
  @g2_y_re FQ.new_fq(
    198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_790_5,
    @bls12_381_field_modulus
  )
  @g2_y_im FQ.new_fq(
    927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582,
    @bls12_381_field_modulus
  )

  @g2 {FQP.new_fqp([@g2_x_re, @g2_x_im], @bls12_381_fq2_modulus_coeffs, @bls12_381_field_modulus),
       FQP.new_fqp([@g2_y_re, @g2_y_im], @bls12_381_fq2_modulus_coeffs, @bls12_381_field_modulus)}

  @z1 nil
  @z2 nil

  defguard is_point(pt) when is_tuple(pt) and tuple_size(pt) == 2 and is_struct(elem(pt, 0))
  defguard is_general_point(pt)
           when is_nil(pt) or (is_tuple(pt) and tuple_size(pt) == 2 and is_struct(elem(pt, 0)))

  def is_inf(pt)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    is_nil(pt)
  end

  # Helper to get the correct field module (FQ or FQP) for point operations
  defp elem_module(%FQ{}), do: FQ
  defp elem_module(%FQP{}), do: FQP
  defp elem_module(_), do: FQ # Default or error

  def is_on_curve(pt, b_val)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    if is_inf(pt) do
      true
    else
      {x, y} = pt
      op_module = elem_module(x)
      op_module.eq(op_module.sub(op_module.pow(y, 2), op_module.pow(x, 3)), b_val)
    end
  end

  def double(pt)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    if is_inf(pt) do
      pt
    else
      {x, y} = pt
      op_module = elem_module(x)
      # Ensure numbers like 2 and 3 are FQ elements in the correct field
      fq_2 = FQ.new_fq(2, x.field_modulus)
      fq_3 = FQ.new_fq(3, x.field_modulus)

      three_x_squared = op_module.mul(fq_3, op_module.pow(x, 2))
      two_y = op_module.mul(fq_2, y)
      m = op_module.div(three_x_squared, two_y)

      new_x = op_module.sub(op_module.pow(m, 2), op_module.mul(fq_2, x))
      new_y = op_module.sub(op_module.mul(m, op_module.sub(x, new_x)), y)
      {new_x, new_y}
    end
  end

  def add(p1, p2)
      when (is_nil(p1) or
              (is_tuple(p1) and tuple_size(p1) == 2 and
                 (is_nil(elem(p1, 0)) or is_struct(elem(p1, 0))))) and
             (is_nil(p2) or
                (is_tuple(p2) and tuple_size(p2) == 2 and
                   (is_nil(elem(p2, 0)) or is_struct(elem(p2, 0))))) do
    cond do
      is_inf(p1) ->
        p2
      is_inf(p2) ->
        p1
      true ->
        {x1, y1} = p1
        {x2, y2} = p2
        op_module = elem_module(x1)

        cond do
          op_module.eq(x1, x2) and op_module.eq(y1, y2) ->
            double(p1)
          op_module.eq(x1, x2) ->
            nil # Point at infinity
          true ->
            m_num = op_module.sub(y2, y1)
            m_den = op_module.sub(x2, x1)
            m = op_module.div(m_num, m_den)
            new_x = op_module.sub(op_module.sub(op_module.pow(m, 2), x1), x2)
            new_y = op_module.sub(op_module.mul(m, op_module.sub(x1, new_x)), y1)
            {new_x, new_y}
        end
    end
  end

  def multiply(pt, n)
      when (is_nil(pt) or
              (is_tuple(pt) and tuple_size(pt) == 2 and
                 (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0))))) and is_integer(n) do
    cond do
      n == 0 -> nil
      n == 1 -> pt
      n < 0 -> multiply(neg(pt), -n)
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2)
      when (is_nil(p1) or
              (is_tuple(p1) and tuple_size(p1) == 2 and
                 (is_nil(elem(p1, 0)) or is_struct(elem(p1, 0))))) and
             (is_nil(p2) or
                (is_tuple(p2) and tuple_size(p2) == 2 and
                   (is_nil(elem(p2, 0)) or is_struct(elem(p2, 0))))) do
    cond do
      is_inf(p1) and is_inf(p2) -> true
      is_inf(p1) or is_inf(p2) -> false
      true ->
        {x1, y1} = p1
        {x2, y2} = p2
        op_module1 = elem_module(x1)
        op_module2 = elem_module(x2)
        # Ensure points are of the same type before comparing coordinates
        if op_module1 != op_module2 do
          false
        else
          op_module1.eq(x1, x2) and op_module1.eq(y1, y2)
        end
    end
  end

  @w_coeffs List.duplicate(FQ.zero(@bls12_381_field_modulus), 12) |> List.replace_at(1, FQ.one(@bls12_381_field_modulus))
  @w FQP.new_fqp(@w_coeffs, @bls12_381_fq12_modulus_coeffs, @bls12_381_field_modulus)

  def neg(pt)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    if is_inf(pt) do
      nil
    else
      {x, y} = pt
      op_module = elem_module(y) # y should have a valid module
      {x, op_module.neg(y)}
    end
  end

  def twist(pt_fq2) do
    # {x_fq2, y_fq2} = pt_fq2
    # nx = FQP.mul(x_fq2, FQP.pow(@w, -2)) # This is conceptual, types need care
    # ny = FQP.mul(y_fq2, FQP.pow(@w, -3))
    # {nx, ny}
    # TODO: Implement BLS12-381 twist operation.
    # Requires FQ2 element access and FQ12 arithmetic.
    # For now, returning the input, which is incorrect.
    # Needs FQP.pow(fqp_base, exponent) where exponent can be negative.
    # And also needs careful construction of FQ12 elements for division or inverse.
    _ = pt_fq2 # placeholder
    nil # Placeholder, actual twisted point in FQ12 should be returned
  end

  # Accessors
  def g1(), do: @g1
  def g2(), do: @g2
  def z1(), do: @z1
  def z2(), do: @z2
  def b(), do: @b
  def b2(), do: @b2
  def b12(), do: @b12
  def curve_order(), do: @curve_order
  def g12(), do: twist(@g2) # g12 is G2 twisted into FQ12
end
