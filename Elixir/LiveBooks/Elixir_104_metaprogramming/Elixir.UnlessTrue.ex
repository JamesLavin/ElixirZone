defmodule UnlessTrue do
  defmacro unless(boolean_exp, do: false_case) do
    {:if, [context: UnlessTrue, imports: [{2, Kernel}]],
     [{:!, [context: UnlessTrue, imports: [{1, Kernel}]], [boolean_exp]}, [do: false_case]]}
  end
end