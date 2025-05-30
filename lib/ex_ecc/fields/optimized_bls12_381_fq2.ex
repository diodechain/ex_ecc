defmodule ExEcc.Fields.OptimizedBls12381FQ2 do
  alias ExEcc.Fields.OptimizedFieldElements.FQP
  alias ExEcc.Utils

  @field_modulus 0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB
  @modulus_coeffs [1, 0]  # For BLS12-381, the irreducible polynomial is x^2 + 1

  defstruct coeffs: [], field_modulus: @field_modulus

  def new(coeffs) when is_list(coeffs) do
    coeffs_mod = Enum.map(coeffs, fn c ->
      rem(rem(c, @field_modulus) + @field_modulus, @field_modulus)
    end)
    fqp = FQP.new_fqp(coeffs_mod, @modulus_coeffs, @field_modulus)
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def new(coeffs, field_modulus) do
    fqp = FQP.new_fqp(coeffs, @modulus_coeffs, field_modulus)
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def one(field_modulus \\ @field_modulus) do
    new([1, 0], field_modulus)
  end

  def zero(field_modulus \\ @field_modulus) do
    new([0, 0], field_modulus)
  end

  def neg(fq2), do: negate(fq2)
  def negate(fq2) do
    fqp = FQP.neg(%FQP{coeffs: fq2.coeffs, field_modulus: fq2.field_modulus})
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def equal?(fq2a, fq2b), do: eq(fq2a, fq2b)
  def eq(fq2a, fq2b) do
    FQP.equal?(%FQP{coeffs: fq2a.coeffs, field_modulus: fq2a.field_modulus},
               %FQP{coeffs: fq2b.coeffs, field_modulus: fq2b.field_modulus})
  end

  def add(fq2a, fq2b) do
    fqp = FQP.add(%FQP{coeffs: fq2a.coeffs, field_modulus: fq2a.field_modulus},
                  %FQP{coeffs: fq2b.coeffs, field_modulus: fq2b.field_modulus})
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def subtract(fq2a, fq2b) do
    fqp = FQP.subtract(%FQP{coeffs: fq2a.coeffs, field_modulus: fq2a.field_modulus},
                       %FQP{coeffs: fq2b.coeffs, field_modulus: fq2b.field_modulus})
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def multiply(fq2a, fq2b) do
    fqp = FQP.multiply(%FQP{coeffs: fq2a.coeffs, field_modulus: fq2a.field_modulus},
                       %FQP{coeffs: fq2b.coeffs, field_modulus: fq2b.field_modulus})
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def divide(fq2a, fq2b) do
    fqp = FQP.divide(%FQP{coeffs: fq2a.coeffs, field_modulus: fq2a.field_modulus},
                     %FQP{coeffs: fq2b.coeffs, field_modulus: fq2b.field_modulus})
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  def pow(fq2, exponent) do
    fqp = FQP.pow(%FQP{coeffs: fq2.coeffs, field_modulus: fq2.field_modulus}, exponent)
    %__MODULE__{coeffs: fqp.coeffs, field_modulus: fqp.field_modulus}
  end

  # Add missing field operations
  def sgn0(fq2) do
    [c0, c1] = fq2.coeffs
    cond do
      c0 != 0 -> rem(c0, 2)
      true -> rem(c1, 2)
    end
  end

  def compare(fq2a, fq2b) do
    [a0, a1] = fq2a.coeffs
    [b0, b1] = fq2b.coeffs
    case Kernel.compare(a1, b1) do
      0 -> Kernel.compare(a0, b0)
      result -> result
    end
  end

  def ensure_fq2(val, field_modulus) when is_integer(val) do
    new([val, 0], field_modulus)
  end

  def ensure_fq2(fq2 = %__MODULE__{}, field_modulus) do
    if fq2.field_modulus != field_modulus do
      raise "Cannot operate on FQ2 elements from different fields without explicit conversion."
    end
    fq2
  end

  def ensure_fq2(other, _field_modulus) do
    raise "Type error: Expected an integer or FQ2 element, got: #{inspect(other)}"
  end
end
