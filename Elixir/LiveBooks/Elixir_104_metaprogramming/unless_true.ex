defmodule UnlessTrue do
  defmacro unless(boolean_exp, do: false_case) do
    quote do
      if !unquote(boolean_exp), do: unquote(false_case)
    end
  end
end
