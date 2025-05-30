defmodule ExEcc.Fields.FQ12 do
  alias ExEcc.Fields.FQ2

  @field_modulus 21888242871839275222246405745257275088696311157297823662689037894645226208583

  defstruct [:coeffs, :field_modulus]

  @type t :: %__MODULE__{
    coeffs: list(ExEcc.Fields.FQ2.t()),
    field_modulus: integer
  }

  def new(coeffs, field_modulus) when is_list(coeffs) and length(coeffs) == 6 and is_integer(field_modulus) do
    %__MODULE__{
      coeffs: Enum.map(coeffs, &FQ2.new(&1, field_modulus)),
      field_modulus: field_modulus
    }
  end

  def new(coeffs) when is_list(coeffs) and length(coeffs) == 6 do
    new(coeffs, @field_modulus)
  end

  def add(fq12_1 = %__MODULE__{}, fq12_2) do
    fq12_2 = ensure_fq12(fq12_2, fq12_1.field_modulus)
    new_coeffs = Enum.zip_with(fq12_1.coeffs, fq12_2.coeffs, &FQ2.add/2)
    %__MODULE__{coeffs: new_coeffs, field_modulus: fq12_1.field_modulus}
  end

  def subtract(fq12_1 = %__MODULE__{}, fq12_2) do
    fq12_2 = ensure_fq12(fq12_2, fq12_1.field_modulus)
    new_coeffs = Enum.zip_with(fq12_1.coeffs, fq12_2.coeffs, &FQ2.subtract/2)
    %__MODULE__{coeffs: new_coeffs, field_modulus: fq12_1.field_modulus}
  end

  def mul(fq12_1 = %__MODULE__{}, fq12_2) do
    fq12_2 = ensure_fq12(fq12_2, fq12_1.field_modulus)
    [a0, a1, a2, _a3, _a4, _a5] = fq12_1.coeffs
    [b0, b1, b2, b3, b4, b5] = fq12_2.coeffs
    p = fq12_1.field_modulus

    # FQ12 multiplication using Karatsuba algorithm
    # (a0 + a1*w + a2*w^2 + a3*w^3 + a4*w^4 + a5*w^5) * (b0 + b1*w + b2*w^2 + b3*w^3 + b4*w^4 + b5*w^5)
    # where w^6 = -1
    c0 = FQ2.add(FQ2.mul(a0, b0), FQ2.mul(FQ2.mul(a1, b5), FQ2.new([0, 1], p)))
    c1 = FQ2.add(FQ2.add(FQ2.mul(a0, b1), FQ2.mul(a1, b0)), FQ2.mul(FQ2.mul(a2, b5), FQ2.new([0, 1], p)))
    c2 = FQ2.add(FQ2.add(FQ2.mul(a0, b2), FQ2.mul(a1, b1)), FQ2.mul(a2, b0))
    c3 = FQ2.add(FQ2.add(FQ2.mul(a0, b3), FQ2.mul(a1, b2)), FQ2.mul(a2, b1))
    c4 = FQ2.add(FQ2.add(FQ2.mul(a0, b4), FQ2.mul(a1, b3)), FQ2.mul(a2, b2))
    c5 = FQ2.add(FQ2.add(FQ2.mul(a0, b5), FQ2.mul(a1, b4)), FQ2.mul(a2, b3))

    %__MODULE__{coeffs: [c0, c1, c2, c3, c4, c5], field_modulus: p}
  end

  def multiply(fq12_1, fq12_2), do: mul(fq12_1, fq12_2)

  def negate(fq12 = %__MODULE__{}) do
    new_coeffs = Enum.map(fq12.coeffs, &FQ2.negate/1)
    %__MODULE__{coeffs: new_coeffs, field_modulus: fq12.field_modulus}
  end

  def neg(fq12), do: negate(fq12)

  def equal?(fq12_1 = %__MODULE__{}, fq12_2) do
    fq12_2 = ensure_fq12(fq12_2, fq12_1.field_modulus)
    Enum.zip_with(fq12_1.coeffs, fq12_2.coeffs, &FQ2.equal?/2) |> Enum.all?()
  end

  def eq(fq12_1, fq12_2), do: equal?(fq12_1, fq12_2)

  defp ensure_fq12(val, field_modulus) when is_integer(val) do
    new(List.duplicate([val, 0], 6), field_modulus)
  end

  defp ensure_fq12(fq12 = %__MODULE__{}, field_modulus) do
    if fq12.field_modulus != field_modulus do
      raise "Cannot operate on FQ12 elements from different fields"
    end
    fq12
  end

  defp ensure_fq12(other, _field_modulus) do
    raise "Type error: Expected an integer or FQ12 element, got: #{inspect(other)}"
  end

  def zero do
    %__MODULE__{coeffs: List.duplicate(FQ2.zero(), 6), field_modulus: @field_modulus}
  end

  def zero(field_modulus) do
    %__MODULE__{coeffs: List.duplicate(FQ2.zero(field_modulus), 6), field_modulus: field_modulus}
  end

  def one do
    %__MODULE__{coeffs: [FQ2.one() | List.duplicate(FQ2.zero(), 5)], field_modulus: @field_modulus}
  end

  def one(field_modulus) do
    %__MODULE__{coeffs: [FQ2.one(field_modulus) | List.duplicate(FQ2.zero(field_modulus), 5)], field_modulus: field_modulus}
  end

  def divide(fq12_1 = %__MODULE__{}, fq12_2) do
    fq12_2 = ensure_fq12(fq12_2, fq12_1.field_modulus)
    multiply(fq12_1, inv(fq12_2))
  end

  def inv(fq12 = %__MODULE__{}) do
    # For FQ12, we use the fact that x^(p^12-1) = 1 in FQ12
    # So x^(-1) = x^(p^12-2)
    p = fq12.field_modulus
    power = :math.pow(p, 12) - 2 |> trunc()
    pow(fq12, power)
  end

  def pow(fq12 = %__MODULE__{}, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 -> one()
      exponent == 1 -> fq12
      rem(exponent, 2) == 0 ->
        half_pow = pow(fq12, Kernel.div(exponent, 2))
        multiply(half_pow, half_pow)
      true ->
        half_pow = pow(fq12, Kernel.div(exponent, 2))
        multiply(multiply(half_pow, half_pow), fq12)
    end
  end

  def sgn0(fq12 = %__MODULE__{}) do
    # For FQ12, sgn0 is determined by the first non-zero coefficient
    Enum.find_value(fq12.coeffs, 0, fn coeff ->
      if FQ2.equal?(coeff, FQ2.zero()) do
        nil
      else
        FQ2.sgn0(coeff)
      end
    end)
  end

  def conjugate(fq12 = %__MODULE__{}) do
    # For FQ12, conjugation is applying the Frobenius map 6 times
    frobenius(fq12, 6)
  end

  def frobenius(fq12 = %__MODULE__{}, power) do
    # Frobenius map: (a + b*w)^p = a^p + b^p*w^p
    # For FQ12, w^p = w^(p mod 12)
    p = fq12.field_modulus
    new_coeffs = Enum.map(fq12.coeffs, fn coeff ->
      FQ2.pow(coeff, p)
    end)
    %__MODULE__{coeffs: new_coeffs, field_modulus: p}
  end

  def is_zero(fq12 = %__MODULE__{}) do
    Enum.all?(fq12.coeffs, &FQ2.is_zero/1)
  end
end
