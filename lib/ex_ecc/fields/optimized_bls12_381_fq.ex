defmodule ExEcc.Fields.OptimizedBls12381FQ do
  alias ExEcc.Utils

  @field_modulus 0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB

  defstruct n: 0, field_modulus: @field_modulus

  def field_modulus, do: @field_modulus

  def new(n) when is_integer(n) do
    %__MODULE__{n: rem(rem(n, @field_modulus) + @field_modulus, @field_modulus)}
  end

  def new(n, field_modulus) when is_integer(n) and is_integer(field_modulus) do
    %__MODULE__{
      n: rem(rem(n, field_modulus) + field_modulus, field_modulus),
      field_modulus: field_modulus
    }
  end

  def one(field_modulus \\ @field_modulus) do
    %__MODULE__{n: 1, field_modulus: field_modulus}
  end

  def zero(field_modulus \\ @field_modulus) do
    %__MODULE__{n: 0, field_modulus: field_modulus}
  end

  def add(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n + fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def multiply(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n * fq2.n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def subtract(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    val = rem(fq1.n - fq2.n + fq1.field_modulus, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def divide(fq1 = %__MODULE__{}, fq2_val) do
    fq2 = ensure_fq(fq2_val, fq1.field_modulus)
    inv_fq2_n = Utils.prime_field_inv(fq2.n, fq1.field_modulus)
    val = rem(fq1.n * inv_fq2_n, fq1.field_modulus)
    %__MODULE__{n: val, field_modulus: fq1.field_modulus}
  end

  def pow(fq_base = %__MODULE__{}, exponent) when is_integer(exponent) do
    cond do
      exponent == 0 ->
        %__MODULE__{n: 1, field_modulus: fq_base.field_modulus}

      exponent == 1 ->
        fq_base

      rem(exponent, 2) == 0 ->
        half_pow = pow(fq_base, Kernel.div(exponent, 2))
        multiply(half_pow, half_pow)

      true ->
        half_pow = pow(fq_base, Kernel.div(exponent, 2))
        multiply(multiply(half_pow, half_pow), fq_base)
    end
  end

  def negate(fq = %__MODULE__{}) do
    %__MODULE__{
      n: rem(-fq.n + fq.field_modulus, fq.field_modulus),
      field_modulus: fq.field_modulus
    }
  end

  def neg(fq = %__MODULE__{}), do: negate(fq)

  def eq(fq1 = %__MODULE__{}, fq2_val), do: equal?(fq1, fq2_val)

  def equal?(fq1 = %__MODULE__{}, fq2_val) do
    cond do
      is_integer(fq2_val) ->
        fq1.n == fq2_val

      is_map(fq2_val) and Map.has_key?(fq2_val, :n) ->
        fq1.n == fq2_val.n and fq1.field_modulus == fq2_val.field_modulus

      true ->
        false
    end
  end

  def compare(fq1 = %__MODULE__{}, fq2_val) do
    fq2_n = if is_integer(fq2_val), do: fq2_val, else: ensure_fq(fq2_val, fq1.field_modulus).n

    cond do
      fq1.n > fq2_n -> 1
      fq1.n < fq2_n -> -1
      true -> 0
    end
  end

  def sgn0(fq = %__MODULE__{}) do
    rem(fq.n, 2)
  end

  defp ensure_fq(val, field_modulus) when is_integer(val) do
    new(val, field_modulus)
  end

  defp ensure_fq(fq = %__MODULE__{}, field_modulus) do
    if fq.field_modulus != field_modulus do
      raise "Cannot operate on FQ elements from different fields without explicit conversion."
    end

    fq
  end

  defp ensure_fq(other, _field_modulus) do
    raise "Type error: Expected an integer or FQ element, got: #{inspect(other)}"
  end
end
