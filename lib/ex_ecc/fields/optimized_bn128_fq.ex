defmodule ExEcc.Fields.OptimizedBN128FQ do
  @field_modulus 21888242871839275222246405745257275088548364400416034343698204186575808495617

  defstruct n: 0, field_modulus: nil

  def field_modulus, do: @field_modulus

  def new(n), do: %__MODULE__{n: rem(rem(n, @field_modulus) + @field_modulus, @field_modulus), field_modulus: @field_modulus}
  def new(n, field_modulus), do: %__MODULE__{n: rem(rem(n, field_modulus) + field_modulus, field_modulus), field_modulus: field_modulus}

  def zero, do: new(0)

  def one, do: new(1)

  def multiply(%__MODULE__{n: n1, field_modulus: m} = fq1, %__MODULE__{n: n2}) do
    %__MODULE__{fq1 | n: rem(n1 * n2, m)}
  end
  def multiply(%__MODULE__{n: n1, field_modulus: m} = fq1, n2) when is_integer(n2) do
    %__MODULE__{fq1 | n: rem(n1 * n2, m)}
  end
  def mul(a, b), do: multiply(a, b)
end
