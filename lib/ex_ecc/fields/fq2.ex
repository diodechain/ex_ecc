defmodule ExEcc.Fields.FQ2 do
  alias ExEcc.Utils

  @field_modulus 21_888_242_871_839_275_222_246_405_745_257_275_088_696_311_157_297_823_662_689_037_894_645_226_208_583

  defstruct [:coeffs, :field_modulus]

  @type t :: %__MODULE__{
          coeffs: list(integer),
          field_modulus: integer
        }

  def new(coeffs, field_modulus)
      when is_list(coeffs) and length(coeffs) == 2 and is_integer(field_modulus) do
    %__MODULE__{
      coeffs: Enum.map(coeffs, &rem(&1, field_modulus)),
      field_modulus: field_modulus
    }
  end

  def new(coeffs) when is_list(coeffs) and length(coeffs) == 2 do
    new(coeffs, @field_modulus)
  end

  def add(fq2_1 = %__MODULE__{}, fq2_2) do
    fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)

    new_coeffs =
      Enum.zip_with(fq2_1.coeffs, fq2_2.coeffs, fn a, b ->
        rem(a + b, fq2_1.field_modulus)
      end)

    %__MODULE__{coeffs: new_coeffs, field_modulus: fq2_1.field_modulus}
  end

  def subtract(fq2_1 = %__MODULE__{}, fq2_2) do
    fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)

    new_coeffs =
      Enum.zip_with(fq2_1.coeffs, fq2_2.coeffs, fn a, b ->
        rem(a - b, fq2_1.field_modulus)
      end)

    %__MODULE__{coeffs: new_coeffs, field_modulus: fq2_1.field_modulus}
  end

  def mul(fq2_1 = %__MODULE__{}, fq2_2) do
    fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
    [a1, b1] = fq2_1.coeffs
    [a2, b2] = fq2_2.coeffs
    p = fq2_1.field_modulus

    # (a1 + b1*i) * (a2 + b2*i) = (a1*a2 - b1*b2) + (a1*b2 + a2*b1)*i
    a = rem(a1 * a2 - b1 * b2, p)
    b = rem(a1 * b2 + a2 * b1, p)

    %__MODULE__{coeffs: [a, b], field_modulus: p}
  end

  def multiply(fq2_1, fq2_2), do: mul(fq2_1, fq2_2)

  def negate(fq2 = %__MODULE__{}) do
    new_coeffs = Enum.map(fq2.coeffs, &rem(-&1, fq2.field_modulus))
    %__MODULE__{coeffs: new_coeffs, field_modulus: fq2.field_modulus}
  end

  def neg(fq2), do: negate(fq2)

  def equal?(fq2_1 = %__MODULE__{}, fq2_2) do
    fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
    Enum.zip_with(fq2_1.coeffs, fq2_2.coeffs, &(&1 == &2)) |> Enum.all?()
  end

  def eq(fq2_1, fq2_2), do: equal?(fq2_1, fq2_2)

  def conjugate(fq2 = %__MODULE__{}) do
    [a, b] = fq2.coeffs
    %__MODULE__{coeffs: [a, rem(-b, fq2.field_modulus)], field_modulus: fq2.field_modulus}
  end

  def frobenius(fq2 = %__MODULE__{}, _power) do
    # For FQ2, Frobenius map is conjugation
    conjugate(fq2)
  end

  defp ensure_fq2(val, field_modulus) when is_integer(val) do
    new([val, 0], field_modulus)
  end

  defp ensure_fq2(fq2 = %__MODULE__{}, field_modulus) do
    if fq2.field_modulus != field_modulus do
      raise "Cannot operate on FQ2 elements from different fields"
    end

    fq2
  end

  defp ensure_fq2(other, _field_modulus) do
    raise "Type error: Expected an integer or FQ2 element, got: #{inspect(other)}"
  end

  def one(field_modulus) do
    %__MODULE__{coeffs: [1, 0], field_modulus: field_modulus}
  end

  def zero do
    %__MODULE__{coeffs: [0, 0], field_modulus: @field_modulus}
  end

  def zero(field_modulus) do
    %__MODULE__{coeffs: [0, 0], field_modulus: field_modulus}
  end

  def divide(fq2_1 = %__MODULE__{}, fq2_2) do
    fq2_2 = ensure_fq2(fq2_2, fq2_1.field_modulus)
    multiply(fq2_1, inv(fq2_2))
  end

  def inv(fq2 = %__MODULE__{}) do
    [a, b] = fq2.coeffs
    p = fq2.field_modulus

    # For FQ2, the inverse of (a + b*i) is (a - b*i)/(a^2 + b^2)
    denominator = rem(a * a + b * b, p)
    denominator_inv = Utils.prime_field_inv(denominator, p)

    a_inv = rem(a * denominator_inv, p)
    b_inv = rem(-b * denominator_inv, p)

    %__MODULE__{coeffs: [a_inv, b_inv], field_modulus: p}
  end

  def sgn0(fq2 = %__MODULE__{}) do
    [a, b] = fq2.coeffs
    # sgn0(a + b*i) = sgn0(a) if a != 0, otherwise sgn0(b)
    cond do
      a != 0 -> rem(a, 2)
      true -> rem(b, 2)
    end
  end

  def pow(fq2 = %__MODULE__{}, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        one(fq2.field_modulus)

      exponent == 1 ->
        fq2

      rem(exponent, 2) == 0 ->
        half_pow = pow(fq2, Kernel.div(exponent, 2))
        multiply(half_pow, half_pow)

      true ->
        half_pow = pow(fq2, Kernel.div(exponent, 2))
        multiply(multiply(half_pow, half_pow), fq2)
    end
  end

  def is_zero(fq2 = %__MODULE__{}) do
    Enum.all?(fq2.coeffs, &(&1 == 0))
  end
end
