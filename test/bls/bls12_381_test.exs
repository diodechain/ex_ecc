defmodule ExEcc.BLS.BLS12381Test do
  @doc """
  Ported from tests/core/test_bn128_and_bls12_381.py
  """
  use ExUnit.Case
  alias ExEcc.OptimizedBLS12381.OptimizedPairing, as: Pairing
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.FieldMath

  @field_modulus 0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB
  @g1 Curve.g1()
  @g2 Curve.g2()
  @g12 Curve.g12()
  @z1 Curve.z1()
  @z2 Curve.z2()
  @b2 Curve.b2()
  @b12 Curve.b12()

  test "FQ object" do
    assert FieldMath.eq(FieldMath.mul(FQ.new(2), FQ.new(2)), FQ.new(4))

    assert FieldMath.eq(
             FieldMath.add(
               FieldMath.div(FQ.new(2), FQ.new(7)),
               FieldMath.div(FQ.new(9), FQ.new(7))
             ),
             FieldMath.div(FQ.new(11), FQ.new(7))
           )

    assert FieldMath.eq(
             FieldMath.add(
               FieldMath.mul(FQ.new(2), FQ.new(7)),
               FieldMath.mul(FQ.new(9), FQ.new(7))
             ),
             FieldMath.mul(FQ.new(11), FQ.new(7))
           )

    assert FieldMath.eq(FieldMath.pow(FQ.new(9), @field_modulus), FQ.new(9))
    assert FQ.new(-1).n > 0
  end

  test "FQ2 object" do
    x = FQ2.new({1, 0})
    f = FQ2.new({1, 2})
    fpx = FQ2.new({2, 2})
    one = FQ2.one()
    {z1, z2} = FieldMath.coeffs(FieldMath.neg(FQ2.new({1, 1})))

    assert FieldMath.eq(FieldMath.add(x, f), fpx)
    assert FieldMath.eq(FieldMath.div(f, f), one)

    assert FieldMath.eq(
             FieldMath.add(FieldMath.div(one, f), FieldMath.div(x, f)),
             FieldMath.div(FieldMath.add(one, x), f)
           )

    assert FieldMath.eq(
             FieldMath.add(FieldMath.mul(one, f), FieldMath.mul(x, f)),
             FieldMath.mul(FieldMath.add(one, x), f)
           )

    assert FieldMath.eq(FieldMath.pow(x, @field_modulus * @field_modulus - 1), one)
    assert FieldMath.lt(z1, FQ2.zero())
    assert FieldMath.lt(z2, FQ2.zero())
  end

  test "FQ12 object" do
    x = FQ12.new(List.to_tuple([1] ++ List.duplicate(0, 11)))
    f = FQ12.new(List.to_tuple(Enum.to_list(1..12)))
    fpx = FQ12.new(List.to_tuple([2, 2] ++ Enum.to_list(3..12)))
    one = FQ12.one()
    zs = FieldMath.coeffs(FieldMath.neg(FQ12.new(List.to_tuple(List.duplicate(1, 12)))))

    assert FieldMath.eq(FieldMath.add(x, f), fpx)
    assert FieldMath.eq(FieldMath.div(f, f), one)

    assert FieldMath.eq(
             FieldMath.add(FieldMath.div(one, f), FieldMath.div(x, f)),
             FieldMath.div(FieldMath.add(one, x), f)
           )

    assert FieldMath.eq(
             FieldMath.add(FieldMath.mul(one, f), FieldMath.mul(x, f)),
             FieldMath.mul(FieldMath.add(one, x), f)
           )

    # This check takes too long
    # assert FieldMath.eq(FieldMath.pow(x, :math.pow(@field_modulus, 12) - 1), one)
    assert Enum.all?(Tuple.to_list(zs), fn z -> FieldMath.lt(z, FQ12.zero()) end)
  end

  test "G1 object" do
    assert Curve.eq(
             Curve.add(Curve.add(Curve.double(@g1), @g1), @g1),
             Curve.double(Curve.double(@g1))
           )

    assert not Curve.eq(Curve.double(@g1), @g1)

    assert Curve.eq(
             Curve.add(Curve.multiply(@g1, 9), Curve.multiply(@g1, 5)),
             Curve.add(Curve.multiply(@g1, 12), Curve.multiply(@g1, 2))
           )

    assert Curve.is_inf(Curve.multiply(@g1, Curve.curve_order()))
  end

  test "G2 object" do
    assert Curve.eq(
             Curve.add(Curve.add(Curve.double(@g2), @g2), @g2),
             Curve.double(Curve.double(@g2))
           )

    assert not Curve.eq(Curve.double(@g2), @g2)

    assert Curve.eq(
             Curve.add(Curve.multiply(@g2, 9), Curve.multiply(@g2, 5)),
             Curve.add(Curve.multiply(@g2, 12), Curve.multiply(@g2, 2))
           )

    assert Curve.is_inf(Curve.multiply(@g2, Curve.curve_order()))
    assert not Curve.is_inf(Curve.multiply(@g2, 2 * @field_modulus - Curve.curve_order()))
    assert Curve.is_on_curve(Curve.multiply(@g2, 9), @b2)
  end

  test "G12 object" do
    assert Curve.eq(
             Curve.add(Curve.add(Curve.double(@g12), @g12), @g12),
             Curve.double(Curve.double(@g12))
           )

    assert not Curve.eq(Curve.double(@g12), @g12)

    assert Curve.eq(
             Curve.add(Curve.multiply(@g12, 9), Curve.multiply(@g12, 5)),
             Curve.add(Curve.multiply(@g12, 12), Curve.multiply(@g12, 2))
           )

    assert Curve.is_on_curve(Curve.multiply(@g12, 9), @b12)
    assert Curve.is_inf(Curve.multiply(@g12, Curve.curve_order()))
  end

  test "Z1 object" do
    assert Curve.eq(@g1, Curve.add(@g1, @z1))
    assert Curve.eq(@z1, Curve.double(@z1))
    assert Curve.eq(@z1, Curve.multiply(@z1, 0))
    assert Curve.eq(@z1, Curve.multiply(@z1, 1))
    assert Curve.eq(@z1, Curve.multiply(@z1, 2))
    assert Curve.eq(@z1, Curve.multiply(@z1, 3))
    assert Curve.is_inf(Curve.neg(@z1))
  end

  test "Z2 object" do
    assert Curve.eq(@g2, Curve.add(@g2, @z2))
    assert Curve.eq(@z2, Curve.double(@z2))
    assert Curve.eq(@z2, Curve.multiply(@z2, 0))
    assert Curve.eq(@z2, Curve.multiply(@z2, 1))
    assert Curve.eq(@z2, Curve.multiply(@z2, 2))
    assert Curve.eq(@z2, Curve.multiply(@z2, 3))
    assert Curve.is_inf(Curve.neg(@z2))
    assert Curve.is_inf(Curve.twist(@z2))
  end

  test "pairing negative G1" do
    p1 = Pairing.pairing(@g2, @g1)
    pn1 = Pairing.pairing(@g2, Curve.neg(@g1))

    assert FieldMath.eq(FieldMath.mul(p1, pn1), FQ12.one())
  end

  test "pairing negative G2" do
    p1 = Pairing.pairing(@g2, @g1)
    pn1 = Pairing.pairing(@g2, Curve.neg(@g1))
    np1 = Pairing.pairing(Curve.neg(@g2), @g1)

    assert FieldMath.eq(FieldMath.mul(p1, np1), FQ12.one())
    assert FieldMath.eq(pn1, np1)
  end

  test "pairing output order" do
    p1 = Pairing.pairing(@g2, @g1)

    assert FieldMath.eq(FieldMath.pow(p1, Curve.curve_order()), FQ12.one())
  end

  test "pairing bilinearity on G1" do
    p1 = Pairing.pairing(@g2, @g1)
    p2 = Pairing.pairing(@g2, Curve.multiply(@g1, 2))

    assert FieldMath.eq(FieldMath.mul(p1, p1), p2)
  end

  test "pairing is non degenerate" do
    p1 = Pairing.pairing(@g2, @g1)
    p2 = Pairing.pairing(@g2, Curve.multiply(@g1, 2))
    np1 = Pairing.pairing(Curve.neg(@g2), @g1)

    assert not FieldMath.eq(p1, p2)
    assert not FieldMath.eq(p1, np1)
    assert not FieldMath.eq(p2, np1)
  end

  test "pairing bilinearity on G2" do
    p1 = Pairing.pairing(@g2, @g1)
    po2 = Pairing.pairing(Curve.multiply(@g2, 2), @g1)

    assert FieldMath.eq(FieldMath.mul(p1, p1), po2)
  end

  test "pairing composite check" do
    p3 = Pairing.pairing(Curve.multiply(@g2, 27), Curve.multiply(@g1, 37))
    po3 = Pairing.pairing(@g2, Curve.multiply(@g1, 999))

    assert FieldMath.eq(p3, po3)
  end
end
