defmodule ExEcc.Bn128AndBls12381Test do
  use ExUnit.Case

  # Import all the modules we need to test
  alias ExEcc.Bn128.Bn128Curve, as: Bn128
  alias ExEcc.Bn128.Bn128Pairing, as: Bn128Pairing
  alias ExEcc.Bls12_381.Bls12381Curve, as: Bls12381
  alias ExEcc.Bls12_381.Bls12381Pairing, as: Bls12381Pairing

  # Import field elements
  alias ExEcc.Fields.OptimizedFieldElements, as: FQ
  alias ExEcc.Fields.OptimizedBN128FQ, as: BN128FQ
  alias ExEcc.Fields.OptimizedBls12381FQ, as: BLS12381FQ
  alias ExEcc.Fields.OptimizedFieldElements.FQ12
  alias ExEcc.Fields.OptimizedBN128FQ2, as: BN128FQ2
  alias ExEcc.Fields.OptimizedBls12381FQ2, as: BLS12381FQ2

  # Import field properties
  alias ExEcc.Fields.FieldProperties, as: FieldProps

  # Define test modules to iterate over
  @modules [
    {Bn128, BN128FQ, BN128FQ2, FQ12},
    {Bls12381, BLS12381FQ, BLS12381FQ2, FQ12}
  ]

  # Helper function to get field modulus based on module
  def get_field_modulus(module) do
    case module do
      Bn128 -> FieldProps.get_field_properties("bn128")["field_modulus"]
      Bls12381 -> FieldProps.get_field_properties("bls12_381")["field_modulus"]
      _ -> raise "Unknown module: #{inspect(module)}"
    end
  end

  # Test FQ objects
  describe "FQ objects" do
    for {module, fq_module, _fq2_module, _fq12_module} <- @modules do
      mod = module
      fq_mod = fq_module
      test "#{inspect(mod)} FQ operations" do
        field_modulus = ExEcc.Bn128AndBls12381Test.get_field_modulus(unquote(mod))
        fq_module = unquote(fq_module)

        assert fq_module.multiply(fq_module.new(2, field_modulus), fq_module.new(2, field_modulus)) == fq_module.new(4, field_modulus)

        result1 = fq_module.add(
          fq_module.divide(fq_module.new(2, field_modulus), fq_module.new(7, field_modulus)),
          fq_module.divide(fq_module.new(9, field_modulus), fq_module.new(7, field_modulus))
        )
        expected1 = fq_module.divide(fq_module.new(11, field_modulus), fq_module.new(7, field_modulus))
        assert result1 == expected1

        result2 = fq_module.add(
          fq_module.multiply(fq_module.new(2, field_modulus), fq_module.new(7, field_modulus)),
          fq_module.multiply(fq_module.new(9, field_modulus), fq_module.new(7, field_modulus))
        )
        expected2 = fq_module.multiply(fq_module.new(11, field_modulus), fq_module.new(7, field_modulus))
        assert result2 == expected2

        assert fq_module.pow(fq_module.new(9, field_modulus), field_modulus) == fq_module.new(9, field_modulus)

        neg_one = fq_module.negate(fq_module.new(1, field_modulus))
        assert neg_one.n > 0
      end
    end
  end

  # Test FQ2 objects
  describe "FQ2 objects" do
    for {module, _fq_module, fq2_module, _fq12_module} <- @modules do
      mod = module
      fq2_mod = fq2_module
      test "#{inspect(mod)} FQ2 operations" do
        field_modulus = ExEcc.Bn128AndBls12381Test.get_field_modulus(unquote(mod))
        fq2_module = unquote(fq2_mod)

        x = fq2_module.new([1, 0], field_modulus)
        f = fq2_module.new([1, 2], field_modulus)
        fpx = fq2_module.new([2, 2], field_modulus)
        one = fq2_module.one(field_modulus)

        assert fq2_module.add(x, f) == fpx
        assert fq2_module.divide(f, f) == one

        result1 = fq2_module.add(
          fq2_module.divide(one, f),
          fq2_module.divide(x, f)
        )
        expected1 = fq2_module.divide(fq2_module.add(one, x), f)
        assert result1 == expected1

        result2 = fq2_module.add(
          fq2_module.multiply(one, f),
          fq2_module.multiply(x, f)
        )
        expected2 = fq2_module.multiply(fq2_module.add(one, x), f)
        assert result2 == expected2

        assert fq2_module.pow(x, field_modulus * field_modulus - 1) == one

        neg_one = fq2_module.negate(fq2_module.new([1, 1], field_modulus))
        {z1, z2} = neg_one.coeffs
        assert z1 > 0
        assert z2 > 0
      end
    end
  end

  # Test G1 objects
  describe "G1 objects" do
    for {module, _fq_module, _fq2_module, _fq12_module} <- @modules do
      mod = module
      test "#{inspect(mod)} G1 operations" do
        g1 = apply(unquote(mod), :g1, [])
        z1 = apply(unquote(mod), :z1, [])

        assert apply(unquote(mod), :eq, [g1, g1])

        g1_double = apply(unquote(mod), :double, [g1])
        assert apply(unquote(mod), :eq, [g1_double, apply(unquote(mod), :add, [g1, g1])])

        g1_add = apply(unquote(mod), :add, [g1, g1])
        assert apply(unquote(mod), :eq, [g1_add, apply(unquote(mod), :multiply, [g1, 2])])

        g1_mult = apply(unquote(mod), :multiply, [g1, apply(unquote(mod), :curve_order, [])])
        assert apply(unquote(mod), :is_inf, [g1_mult])

        assert apply(unquote(mod), :is_inf, [z1])
      end
    end
  end

  # Test G2 objects
  describe "G2 objects" do
    for {module, _fq_module, _fq2_module, _fq12_module} <- @modules do
      mod = module
      test "#{inspect(mod)} G2 operations" do
        g2 = apply(unquote(mod), :g2, [])
        z2 = apply(unquote(mod), :z2, [])

        assert apply(unquote(mod), :eq, [g2, g2])

        g2_double = apply(unquote(mod), :double, [g2])
        assert apply(unquote(mod), :eq, [g2_double, apply(unquote(mod), :add, [g2, g2])])

        g2_add = apply(unquote(mod), :add, [g2, g2])
        assert apply(unquote(mod), :eq, [g2_add, apply(unquote(mod), :multiply, [g2, 2])])

        g2_mult = apply(unquote(mod), :multiply, [g2, apply(unquote(mod), :curve_order, [])])
        assert apply(unquote(mod), :is_inf, [g2_mult])

        assert apply(unquote(mod), :is_inf, [z2])
        assert apply(unquote(mod), :is_on_curve, [g2])
      end
    end
  end

  # Test pairing
  describe "Pairing operations" do
    for {module, _fq_module, _fq2_module, fq12_module} <- @modules do
      mod = module
      fq12_mod = fq12_module
      pairing_module = if mod == Bn128, do: Pairing, else: Bls12381Pairing
      test "#{inspect(mod)} pairing operations" do
        g1 = apply(unquote(mod), :g1, [])
        g2 = apply(unquote(mod), :g2, [])
        curve_order = apply(unquote(mod), :curve_order, [])
        fq12_module = unquote(fq12_mod)
        pairing_module = unquote(pairing_module)

        neg_g1 = apply(unquote(mod), :neg, [g1])
        pairing1 = pairing_module.pairing(neg_g1, g2)
        pairing2 = pairing_module.pairing(g1, g2)
        assert fq12_module.divide(fq12_module.one, pairing2) == pairing1

        neg_g2 = apply(unquote(mod), :neg, [g2])
        pairing3 = pairing_module.pairing(g1, neg_g2)
        pairing4 = pairing_module.pairing(g1, g2)
        assert fq12_module.divide(fq12_module.one, pairing4) == pairing3

        pairing5 = pairing_module.pairing(g1, g2)
        assert fq12_module.pow(pairing5, curve_order) == fq12_module.one

        a = 2
        b = 3
        g1_a = apply(unquote(mod), :multiply, [g1, a])
        g1_b = apply(unquote(mod), :multiply, [g1, b])
        pairing6 = pairing_module.pairing(g1_a, g2)
        pairing7 = pairing_module.pairing(g1_b, g2)
        pairing8 = pairing_module.pairing(apply(unquote(mod), :add, [g1_a, g1_b]), g2)
        assert fq12_module.multiply(pairing6, pairing7) == pairing8

        assert pairing_module.pairing(g1, g2) != fq12_module.one

        g2_a = apply(unquote(mod), :multiply, [g2, a])
        g2_b = apply(unquote(mod), :multiply, [g2, b])
        pairing9 = pairing_module.pairing(g1, g2_a)
        pairing10 = pairing_module.pairing(g1, g2_b)
        pairing11 = pairing_module.pairing(g1, apply(unquote(mod), :add, [g2_a, g2_b]))
        assert fq12_module.multiply(pairing9, pairing10) == pairing11
      end
    end
  end
end
