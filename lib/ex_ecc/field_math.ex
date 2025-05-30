defmodule ExEcc.FieldMath do
  def add(a, b), do: call(:add, a, b)
  def mul(a, b), do: call(:mul, a, b)
  def sub(a, b), do: call(:sub, a, b)
  def div(a, b), do: call(:div, a, b)
  def pow(a, b), do: call(:pow, a, b)
  def inv(a), do: call(:inv, a)
  def conjugate(a), do: call(:conjugate, a)
  def frobenius(a, b), do: call(:frobenius, a, b)
  def equal?(a, b), do: call(:equal?, a, b)
  def not_equal?(a, b), do: call(:not_equal?, a, b)
  def less_than?(a, b), do: call(:less_than?, a, b)

  defp call(op, a, b) do
    apply(resolve(a, op, 2), op, [a, b])
  end

  defp call(op, a) do
    apply(resolve(a, op, 1), op, [a])
  end

  defp resolve(%type{}, op, params), do: resolve(type, op, params)

  defp resolve(type, op, params) do
    if function_exported?(type, op, length(params)) do
      type
    else
      resolve(type.super(), op, params)
    end
  end
end
