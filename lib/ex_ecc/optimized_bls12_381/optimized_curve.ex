defmodule ExEcc.OptimizedBLS12381.OptimizedCurve do
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  # FQP is not directly used in optimized_curve.py for BLS12_381,
  # but might be a type for elements within FQ2, FQ12 if those are FQP-based.
  # For now, we'll alias FQ, FQ2, FQ12 directly as per python import structure.

  @field_modulus ExEcc.Fields.FieldProperties.field_properties()["bls12_381"].field_modulus
  @curve_order 524_358_751_751_261_904_794_477_405_081_859_658_376_905_525_005_276_378_226_036_586_999_385_811_845_13

  # TODO: Port primality and factor checks if large number math is available
  # Elixir's :math.pow behaves differently for large numbers, and direct modulo arithmetic is needed.
  # if :math.pow(2, @curve_order) |> round() |> rem(@curve_order) != 2 do
  #   raise ValueError, "Curve order is not prime"
  # end
  # if rem(:math.pow(@field_modulus, 12) - 1, @curve_order) != 0 do
  #   raise ValueError, "Curve order is not a factor of field_modulus**12 - 1"
  # end

  # Curve is y**2 = x**3 + 4
  @b FQ.new(4)
  # Twisted curve over FQ**2
  # In Python: FQ2((4,4)) assuming FQ2 constructor takes a tuple/list of FQ elements or integers
  @b2 FQ2.new([4, 4])
  # Extension curve over FQ**12; same b value as over FQ
  # Python: FQ12((4,) + (0,) * 11)
  @b12 FQ12.new(List.to_tuple([FQ.new(4) | List.duplicate(FQ.zero(), 11)]))

  # Generator for curve over FQ
  @g1 {
    FQ.new(
      368_541_675_371_338_701_678_108_831_518_307_775_796_162_079_578_254_640_989_457_837_868_860_759_237_837_631_883_605_494_767_634_582_154_810_418_546_4507
    ),
    FQ.new(
      133_950_654_494_447_647_302_047_137_994_192_122_158_493_387_593_834_962_042_654_373_641_651_142_395_633_350_647_272_465_535_336_653_499_239_175_644_1569
    ),
    FQ.one()
  }

  # Generator for twisted curve over FQ2
  @g2 {
    FQ2.new([
      352_701_069_587_466_618_187_139_116_011_060_144_890_029_952_792_775_240_219_908_644_239_793_785_735_715_026_873_347_600_343_865_175_952_761_926_303_160,
      305_914_434_424_421_370_997_125_981_475_378_163_698_647_032_547_664_755_865_937_320_629_163_532_476_895_843_243_350_956_310_434_701_783_788_576_336_5758
    ]),
    FQ2.new([
      198_515_060_228_729_193_556_805_452_117_717_163_830_086_897_821_565_573_085_937_866_506_634_472_637_382_371_842_386_910_426_333_398_464_149_434_034_7905,
      927_553_665_492_332_455_747_201_965_776_037_880_757_740_193_453_592_970_025_027_978_793_976_877_002_675_564_980_949_289_727_957_565_575_433_344_219_582
    ]),
    FQ2.one()
  }

  # Point at infinity over FQ
  @z1 {FQ.one(), FQ.one(), FQ.zero()}
  # Point at infinity for twisted curve over FQ2
  @z2 {FQ2.one(), FQ2.one(), FQ2.zero()}

  # Accessor functions for module attributes
  def g1, do: @g1
  def g2, do: @g2
  def z1, do: @z1
  def z2, do: @z2
  def b, do: @b
  def b2, do: @b2
  def b12, do: @b12
  def field_modulus, do: @field_modulus
  def curve_order, do: @curve_order

  # Helper functions for mixed-type arithmetic (adapted from OptimizedBN128)
  # These are simplified as BLS12-381 Python code doesn't use FQP directly for points,
  # and field operations are on FQ, FQ2, or FQ12 directly.

  defp get_field_module(element) do
    case element do
      %FQ{} ->
        FQ

      %FQ2{} ->
        FQ2

      %FQ12{} ->
        FQ12

      _ ->
        cond do
          # Assuming it's a point
          is_tuple(element) && tuple_size(element) == 3 -> elem(element, 0).__struct__
          # or raise an error
          true -> nil
        end
    end
  end

  # In BLS12-381, operations are generally between elements of the same field type
  # or a field element and an integer. The BN128 approach of converting to a common
  # super-field for all ops (e.g. FQ12) is not strictly followed in the py_ecc BLS12-381 optimized code.
  # Instead, operations like `y**2 * z` imply that y and z are of compatible types, or one is an integer.
  # We will rely on the specific field modules (FQ, FQ2, FQ12) to handle their operations.
  # If truly mixed operations are needed (e.g. FQ * FQ2), specific casting or FQ_POLY handlers are required.
  # For now, we assume operations are within the same field type, or with integers that can be cast.

  defp field_mul(a, b) do
    cond do
      is_integer(a) ->
        FieldModule = get_field_module(b)
        FieldModule.mul(FieldModule.new(a), b)

      is_integer(b) ->
        FieldModule = get_field_module(a)
        FieldModule.mul(a, FieldModule.new(b))

      true ->
        FieldModuleA = get_field_module(a)
        FieldModuleB = get_field_module(b)

        if FieldModuleA == FieldModuleB do
          FieldModuleA.mul(a, b)
        else
          # This case needs careful handling based on specific operation context.
          # For example, if a is FQ and b is FQ2, FQ2.mul might handle FQ(a) * b.
          # For now, assume types are compatible or raise error.
          raise "Mismatched field types for multiplication: #{inspect(a)} and #{inspect(b)}"
        end
    end
  end

  defp field_add(a, b) do
    cond do
      is_integer(a) ->
        FieldModule = get_field_module(b)
        FieldModule.add(FieldModule.new(a), b)

      is_integer(b) ->
        FieldModule = get_field_module(a)
        FieldModule.add(a, FieldModule.new(b))

      true ->
        FieldModuleA = get_field_module(a)
        FieldModuleB = get_field_module(b)

        if FieldModuleA == FieldModuleB do
          FieldModuleA.add(a, b)
        else
          raise "Mismatched field types for addition: #{inspect(a)} and #{inspect(b)}"
        end
    end
  end

  defp field_sub(a, b) do
    cond do
      is_integer(a) ->
        FieldModule = get_field_module(b)
        FieldModule.sub(FieldModule.new(a), b)

      is_integer(b) ->
        FieldModule = get_field_module(a)
        FieldModule.sub(a, FieldModule.new(b))

      true ->
        FieldModuleA = get_field_module(a)
        FieldModuleB = get_field_module(b)

        if FieldModuleA == FieldModuleB do
          FieldModuleA.sub(a, b)
        else
          raise "Mismatched field types for subtraction: #{inspect(a)} and #{inspect(b)}"
        end
    end
  end

  defp field_pow(base, exp) do
    FieldModule = get_field_module(base)
    FieldModule.pow(base, exp)
  end

  defp field_eq(a, b) do
    FieldModuleA = get_field_module(a)
    FieldModuleB = get_field_module(b)
    # Allow comparison with zero of the same field type
    cond do
      is_integer(b) and b == 0 -> FieldModuleA.eq(a, FieldModuleA.zero())
      is_integer(a) and a == 0 -> FieldModuleB.eq(FieldModuleB.zero(), b)
      FieldModuleA == FieldModuleB -> FieldModuleA.eq(a, b)
      # Mismatched types are not equal
      true -> false
    end
  end

  # Check if a point is the point at infinity
  def is_inf(pt) do
    # pt is {x, y, z}
    z_coord = elem(pt, 2)
    FieldMath = get_field_module(z_coord)
    FieldMath.eq(z_coord, FieldMath.zero())
  end

  # Check that a point is on the curve defined by y**2 == x**3 + b
  def is_on_curve(pt, curve_b) do
    if is_inf(pt) do
      true
    else
      {x, y, z} = pt
      # y**2 * z - x**3 == b * z**3
      # LHS: y^2 * z - x^3
      y_squared = field_mul(y, y)
      y_squared_z = field_mul(y_squared, z)
      x_cubed = field_mul(field_mul(x, x), x)
      lhs = field_sub(y_squared_z, x_cubed)

      # RHS: b * z^3
      z_squared = field_mul(z, z)
      z_cubed = field_mul(z_squared, z)
      rhs = field_mul(curve_b, z_cubed)

      field_eq(lhs, rhs)
    end
  end

  # TODO: Add assertions once is_on_curve is fully tested.
  # unless is_on_curve(@g1, @b) do
  #   raise ValueError, "Generator G1 is not on curve"
  # end
  # unless is_on_curve(@g2, @b2) do
  #  raise ValueError, "Generator G2 is not on twisted curve"
  # end

  # Elliptic curve doubling
  def double(pt) do
    {x, y, z} = pt
    # Assuming all components are of the same field type
    FieldMath = get_field_module(x)

    # W = 3 * x * x
    w_val = field_mul(3, field_mul(x, x))
    # S = y * z
    s_val = field_mul(y, z)
    # B = x * y * S
    b_val_internal = field_mul(x, field_mul(y, s_val))
    # H = W * W - 8 * B
    h_val = field_sub(field_mul(w_val, w_val), field_mul(8, b_val_internal))
    # S_squared = S * S
    s_squared = field_mul(s_val, s_val)
    # newx = 2 * H * S
    new_x = field_mul(2, field_mul(h_val, s_val))
    # newy = W * (4 * B - H) - 8 * y * y * S_squared
    term1_new_y = field_mul(w_val, field_sub(field_mul(4, b_val_internal), h_val))
    term2_new_y = field_mul(8, field_mul(field_mul(y, y), s_squared))
    new_y = field_sub(term1_new_y, term2_new_y)
    # newz = 8 * S * S_squared
    new_z = field_mul(8, field_mul(s_val, s_squared))

    {new_x, new_y, new_z}
  end

  # Elliptic curve addition
  def add(p1, p2) do
    # Get FieldMath from first element of p1
    FieldMath = get_field_module(elem(p1, 0))
    one = FieldMath.one()
    zero = FieldMath.zero()

    # if p1[2] == zero or p2[2] == zero:
    #     return p1 if p2[2] == zero else p2
    cond do
      field_eq(elem(p2, 2), zero) ->
        p1

      field_eq(elem(p1, 2), zero) ->
        p2

      true ->
        {x1, y1, z1} = p1
        {x2, y2, z2} = p2

        # U1 = y2 * z1
        u1 = field_mul(y2, z1)
        # U2 = y1 * z2
        u2 = field_mul(y1, z2)
        # V1 = x2 * z1
        v1 = field_mul(x2, z1)
        # V2 = x1 * z2
        v2 = field_mul(x1, z2)

        # if V1 == V2 and U1 == U2:
        #     return double(p1)
        # elif V1 == V2:
        #     return (one, one, zero)
        cond do
          field_eq(v1, v2) and field_eq(u1, u2) ->
            double(p1)

          field_eq(v1, v2) ->
            {one, one, zero}

          true ->
            # U = U1 - U2
            u = field_sub(u1, u2)
            # V = V1 - V2
            v = field_sub(v1, v2)
            # V_squared = V * V
            v_squared = field_mul(v, v)
            # V_squared_times_V2 = V_squared * V2
            v_squared_times_v2 = field_mul(v_squared, v2)
            # V_cubed = V * V_squared
            v_cubed = field_mul(v, v_squared)
            # W = z1 * z2
            w_val = field_mul(z1, z2)
            # A = U * U * W - V_cubed - 2 * V_squared_times_V2
            a_val_term1 = field_mul(field_mul(u, u), w_val)
            a_val_term2 = v_cubed
            a_val_term3 = field_mul(2, v_squared_times_v2)
            a_val = field_sub(field_sub(a_val_term1, a_val_term2), a_val_term3)

            # newx = V * A
            new_x = field_mul(v, a_val)
            # newy = U * (V_squared_times_V2 - A) - V_cubed * U2
            new_y_term1 = field_mul(u, field_sub(v_squared_times_v2, a_val))
            new_y_term2 = field_mul(v_cubed, u2)
            new_y = field_sub(new_y_term1, new_y_term2)
            # newz = V_cubed * W
            new_z = field_mul(v_cubed, w_val)
            {new_x, new_y, new_z}
        end
    end
  end

  # Elliptic curve point multiplication
  def multiply(pt, n) when is_integer(n) do
    FieldMath = get_field_module(elem(pt, 0))

    cond do
      n == 0 -> {FieldMath.one(), FieldMath.one(), FieldMath.zero()}
      n == 1 -> pt
      # Assumes neg/1 is defined
      n < 0 -> multiply(neg(pt), -n)
      rem(n, 2) == 0 -> multiply(double(pt), div(n, 2))
      true -> add(multiply(double(pt), div(n, 2)), pt)
    end
  end

  def eq(p1, p2) do
    {x1, y1, z1} = p1
    {x2, y2, z2} = p2
    # Assuming points are of same field type
    FieldMath = get_field_module(x1)
    zero = FieldMath.zero()

    # Python: x1 * z2 == x2 * z1 and y1 * z2 == y2 * z1
    # Handle cases where one or both points are infinity
    is_z1_zero = field_eq(z1, zero)
    is_z2_zero = field_eq(z2, zero)

    cond do
      # Both are infinity
      is_z1_zero and is_z2_zero ->
        true

      # One is infinity, the other is not
      is_z1_zero or is_z2_zero ->
        false

      true ->
        # Neither is infinity, compare cross-products
        lhs1 = field_mul(x1, z2)
        rhs1 = field_mul(x2, z1)
        lhs2 = field_mul(y1, z2)
        rhs2 = field_mul(y2, z1)
        field_eq(lhs1, rhs1) and field_eq(lhs2, rhs2)
    end
  end

  def normalize(pt) do
    {x, y, z} = pt
    FieldMath = get_field_module(x)

    # if z == 0: return (0, 0)
    if field_eq(z, FieldMath.zero()) do
      {FieldMath.zero(), FieldMath.zero()}
    else
      # return (x / z, y / z)
      z_inv = FieldMath.inv(z)
      {field_mul(x, z_inv), field_mul(y, z_inv)}
    end
  end

  # Convert P => -P
  def neg(pt) do
    {x, y, z} = pt
    FieldMath = get_field_module(y)
    {x, FieldMath.neg(y), z}
  end

  def negate(pt), do: neg(pt)

  # "Twist" a point in E(FQ2) into a point in E(FQ12)
  # w = FQ12([0, 1] + [0] * 10) # This w is used in py_ecc for pairing, not directly in twist function

  def twist(pt) do
    # pt is {x_fq2, y_fq2, z_fq2}
    {x_fq2, y_fq2, z_fq2} = pt

    # Python logic:
    # xcoeffs = [_x.coeffs[0] - _x.coeffs[1], _x.coeffs[1]]
    # ycoeffs = [_y.coeffs[0] - _y.coeffs[1], _y.coeffs[1]]
    # zcoeffs = [_z.coeffs[0] - _z.coeffs[1], _z.coeffs[1]]
    # nx = FQ12([0] + [xcoeffs[0]] + [0] * 5 + [xcoeffs[1]] + [0] * 4)
    # ny = FQ12([ycoeffs[0]] + [0] * 5 + [ycoeffs[1]] + [0] * 5)
    # nz = FQ12([0] * 3 + [zcoeffs[0]] + [0] * 5 + [zcoeffs[1]] + [0] * 2)

    # Assuming FQ2 elements have a .coeffs field which is a list/tuple [c0, c1] of FQ elements or integers
    # And FQ elements can be directly used or .coeffs accessed if they are FQP
    # This part needs to align with how FQ2 and FQ/FQP are structured in Elixir.
    # For now, assuming .coeffs gives a list of underlying base field elements or numbers.

    # Helper to extract coefficients, assuming FQ2 stores them as a list/tuple in :coeffs
    # and that these coeffs are FQ structs or integers that FQ.new can handle.
    auto_fq = fn val ->
      if is_struct(val, FQ) do
        val
      else
        FQ.new(val)
      end
    end

    x_c0 = auto_fq.(Enum.at(x_fq2.coeffs, 0))
    x_c1 = auto_fq.(Enum.at(x_fq2.coeffs, 1))
    y_c0 = auto_fq.(Enum.at(y_fq2.coeffs, 0))
    y_c1 = auto_fq.(Enum.at(y_fq2.coeffs, 1))
    z_c0 = auto_fq.(Enum.at(z_fq2.coeffs, 0))
    z_c1 = auto_fq.(Enum.at(z_fq2.coeffs, 1))

    # xcoeffs = [x_c0 - x_c1, x_c1]
    xcoeffs_0 = FQ.sub(x_c0, x_c1)
    xcoeffs_1 = x_c1

    # ycoeffs = [y_c0 - y_c1, y_c1]
    ycoeffs_0 = FQ.sub(y_c0, y_c1)
    ycoeffs_1 = y_c1

    # zcoeffs = [z_c0 - z_c1, z_c1]
    zcoeffs_0 = FQ.sub(z_c0, z_c1)
    zcoeffs_1 = z_c1

    # Construct FQ12 coefficients. All FQ.zero() unless specified.
    fq12_zero = FQ.zero()

    nx_coeffs = List.duplicate(fq12_zero, 12)
    nx_coeffs = List.replace_at(nx_coeffs, 1, xcoeffs_0)
    # index 7
    nx_coeffs = List.replace_at(nx_coeffs, 1 + 5 + 1, xcoeffs_1)
    nx = FQ12.new(List.to_tuple(nx_coeffs))

    ny_coeffs = List.duplicate(fq12_zero, 12)
    ny_coeffs = List.replace_at(ny_coeffs, 0, ycoeffs_0)
    # index 6
    ny_coeffs = List.replace_at(ny_coeffs, 0 + 5 + 1, ycoeffs_1)
    ny = FQ12.new(List.to_tuple(ny_coeffs))

    nz_coeffs = List.duplicate(fq12_zero, 12)
    nz_coeffs = List.replace_at(nz_coeffs, 3, zcoeffs_0)
    # index 9
    nz_coeffs = List.replace_at(nz_coeffs, 3 + 5 + 1, zcoeffs_1)
    nz = FQ12.new(List.to_tuple(nz_coeffs))

    {nx, ny, nz}
  end

  # Check that the twist creates a point that is on the curve
  # g12 = twist(@g2)
  # unless is_on_curve(g12, @b12) do
  #  raise ValueError, "Twist creates a point not on curve"
  # end
end
