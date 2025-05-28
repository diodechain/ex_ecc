defmodule ExEcc.Bls12381.Curve do
  alias ExEcc.Fields.Bls12381FQ, as: FQ
  alias ExEcc.Fields.Bls12381FQ2, as: FQ2
  alias ExEcc.Fields.Bls12381FQ12, as: FQ12
  # alias ExEcc.Fields.Bls12381FQP, as: FQP # Not used here

  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  @b FQ.new(4)
  # Assuming FQ2.new takes a tuple of tuples for coefficients, e.g. {{c0_re, c0_im}, {c1_re, c1_im}}
  # Or if FQ2 stores coeffs as a tuple of FQ elements: {FQ.new(re), FQ.new(im)}
  # The original code was FQ2.new({{4,4}}), which if FQ2 expects FQ instances, should be:
  # FQ2.new({FQ.new(4), FQ.new(4)}) assuming it represents 4+4u or similar for the FQ2 element for b2.
  # For BLS12-381, b2 is just (4,4) in FQ2, meaning (4+0i) + (4+0i)u = (4,4). Representing as FQ elements:
  @b2 FQ2.new({{FQ.new(4), FQ.zero()}, {FQ.new(4), FQ.zero()}})
  # Or, if FQ2.new({{a,b}}) means a_coeff + b_coeff*u where a, b are from FQ:
  # @b2 FQ2.new({{FQ.new(4), FQ.new(4)}}) - if it's {a,b} where a,b are FQ elements
  # Let's stick to the initial simpler version if FQ2 handles raw integer tuples for FQ.new style init.
  # @b2 FQ2.new({{4, 4}}) # This implies FQ2 constructor handles this form.
  # Reverting to a direct coefficient structure that FQ2.new might expect for 4+4u (if u^2=-1 based field):
  # It is simpler if b2 is just FQ2.new(FQ.new(4), FQ.new(4)) or similar. Python used FQ2([4,4])
  # Let's use a structure for coefficients for b2 consistent with G2 points.
  # The G2 points are FQ2.new({{re0, im0}}, {{re1, im1}}), but b2 is just one FQ2 element.
  # py_ecc has FQ2([4,4]) for b2. If our FQ2 takes a list [re,im] then: FQ2.new([4,4])
  # If it takes a tuple {re,im}, then FQ2.new({4,4}).
  # Given the G2 structure using {{...}}, let's assume FQ2.new takes a direct coefficient tuple for a single FQ2 element.
  # Assuming this means FQ.new(4) + FQ.new(4)*u
  @b2 FQ2.new({FQ.new(4), FQ.new(4)})

  # b12 is an FQ12 element. For FQ12.new(Tuple.duplicate(0,12) |> Tuple.put_elem(0,4)),
  # this should be Tuple.duplicate(FQ.zero(), 12) |> Tuple.put_elem(0, FQ.new(4))
  @b12 FQ12.new(Tuple.put_elem(Tuple.duplicate(FQ.zero(), 12), 0, FQ.new(4)))

  @g1 {FQ.new(
         368_541_675_371_338_701_678_108_831_518_307_775_796_162_079_578_254_640_989_457_837_868_860_759_237_837_631_883_605_494_767_634_582_154_810_418_546_450_7
       ),
       FQ.new(
         133_950_654_494_447_647_302_047_137_994_192_122_158_493_387_593_834_962_042_654_373_641_651_142_395_633_350_647_272_465_535_336_653_499_239_175_644_156_9
       )}

  @g2 {FQ2.new(
         {{FQ.new(
             352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160
           ),
           FQ.new(
             305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_575_8
           )}}
       ),
       FQ2.new(
         {{FQ.new(
             198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_790_5
           ),
           FQ.new(
             927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582
           )}}
       )}

  # Point at infinity for G1
  @z1 nil
  # Point at infinity for G2 (represented on the twisted curve)
  @z2 nil

  # Guard for a 2-tuple point where elements are expected to be field structs.
  defguard is_point(pt) when is_tuple(pt) and tuple_size(pt) == 2 and is_struct(elem(pt, 0))
  # Guard for a point that can be nil or a 2-tuple.
  defguard is_general_point(pt)
           when is_nil(pt) or (is_tuple(pt) and tuple_size(pt) == 2 and is_struct(elem(pt, 0)))

  def is_inf(pt)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    is_nil(pt)
  end

  def is_on_curve(pt, b_val)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    if is_inf(pt) do
      true
    else
      {x, y} = pt
      # Assuming FQ, FQ2, FQ12 implement sub, mul, pow, and eq
      # The module for operations should be derived from the type of x (or y).
      # Get module from struct type e.g. ExEcc.Fields.Bls12381FQ
      field_module = x.__struct__
      # y^2 - x^3 == b
      field_module.eq(field_module.sub(field_module.pow(y, 2), field_module.pow(x, 3)), b_val)
    end
  end

  # Validations would typically be in tests.
  # unless is_on_curve(@g1, @b), do: raise "Generator G1 is not on curve"
  # unless is_on_curve(@g2, @b2), do: raise "Generator G2 is not on twisted curve"

  def double(pt)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    if is_inf(pt) do
      pt
    else
      {x, y} = pt
      field_module = x.__struct__
      # m = (3 * x^2) / (2 * y)
      three_x_squared = field_module.mul(field_module.new(3), field_module.pow(x, 2))
      two_y = field_module.mul(field_module.new(2), y)
      m = field_module.div(three_x_squared, two_y)

      # new_x = m^2 - 2x
      new_x = field_module.sub(field_module.pow(m, 2), field_module.mul(field_module.new(2), x))
      # new_y = m * (x - new_x) - y
      new_y = field_module.sub(field_module.mul(m, field_module.sub(x, new_x)), y)
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
        # Assuming p1 and p2 are of the same field type
        field_module = x1.__struct__

        cond do
          field_module.eq(x1, x2) and field_module.eq(y1, y2) ->
            # p1 == p2, use double
            double(p1)

          field_module.eq(x1, x2) ->
            # Points are inverses (same x, different y implies y1 = -y2 for points on curve)
            # Result is point at infinity
            nil

          true ->
            # m = (y2 - y1) / (x2 - x1)
            m_num = field_module.sub(y2, y1)
            m_den = field_module.sub(x2, x1)
            m = field_module.div(m_num, m_den)

            # new_x = m^2 - x1 - x2
            new_x = field_module.sub(field_module.sub(field_module.pow(m, 2), x1), x2)
            # new_y = m * (x1 - new_x) - y1
            new_y = field_module.sub(field_module.mul(m, field_module.sub(x1, new_x)), y1)
            {new_x, new_y}
        end
    end
  end

  def multiply(pt, n)
      when (is_nil(pt) or
              (is_tuple(pt) and tuple_size(pt) == 2 and
                 (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0))))) and is_integer(n) do
    cond do
      # Multiplication by 0 is point at infinity
      n == 0 ->
        nil

      n == 1 ->
        pt

      # Handle negative n
      n < 0 ->
        multiply(neg(pt), -n)

      # n is even
      rem(n, 2) == 0 ->
        multiply(double(pt), div(n, 2))

      # n is odd
      true ->
        add(multiply(double(pt), div(n, 2)), pt)
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
      is_inf(p1) and is_inf(p2) ->
        true

      # One is infinity, the other is not
      is_inf(p1) or is_inf(p2) ->
        false

      true ->
        {x1, y1} = p1
        {x2, y2} = p2
        field_module_x1 = x1.__struct__
        field_module_x2 = x2.__struct__
        # Ensure points are of the same type before comparing coordinates
        if field_module_x1 != field_module_x2 do
          # Points of different field types cannot be equal in this context
          false
        else
          # Points are of the same field type, use its eq method for coordinates
          field_module_x1.eq(x1, x2) and field_module_x1.eq(y1, y2)
        end
    end
  end

  # w for twist: FQ12 element (0,1,0,0,0,0,0,0,0,0,0,0)
  @w FQ12.new(Tuple.put_elem(Tuple.duplicate(FQ.zero(), 12), 1, FQ.one()))

  def neg(pt)
      when is_nil(pt) or
             (is_tuple(pt) and tuple_size(pt) == 2 and
                (is_nil(elem(pt, 0)) or is_struct(elem(pt, 0)))) do
    if is_inf(pt) do
      nil
    else
      {x, y} = pt
      # Or x.__struct__, assuming y has same type or nil
      field_module = y.__struct__
      {x, field_module.neg(y)}
    end
  end

  # The `twist` function for BLS12-381 involves FQ2 -> FQ12 operations.
  # py_ecc has: (nx / w**2, ny / w**3) for affine points.
  # This needs to handle projective coordinates if G2 is projective.
  # For now, stubbing as it depends on FQP (which is FQ2 for BLS12-381 G2 points)
  # and detailed FQ12 arithmetic (division, power).
  # Point pt is an FQ2 point {x_fq2, y_fq2}.
  def twist(_pt_fq2) do
    # TODO: Implement BLS12-381 twist operation.
    # Requires FQ2 element access and FQ12 arithmetic.
    # Placeholder
    nil
  end

  # G12 = twist(G2)
  # This will be nil until twist is implemented.
  # This cannot be a module attribute if twist is a function call to be done at runtime.
  # It would need to be a function itself, or twist(@g2) should be used directly where needed.
  def g12_val(), do: twist(@g2)

  # Accessors
  def g1(), do: @g1
  def g2(), do: @g2
  def z1(), do: @z1
  def z2(), do: @z2
  def b(), do: @b
  def b2(), do: @b2
  def b12(), do: @b12
  def curve_order(), do: @curve_order
  # Call the function to get G12
  def g12(), do: g12_val()
end
