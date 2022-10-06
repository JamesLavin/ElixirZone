defmodule AreaMacro2 do
  defmacro __using__(opts) do
    case opts do

      :circle ->
        quote location: :keep, bind_quoted: [opts: opts] do
          def area(radius) do
            :math.pi() * :math.pow(radius, 2)
          end
        end

      :square ->
        quote location: :keep, bind_quoted: [opts: opts] do
          def area(side) do
            :math.pow(side, 2)
          end
        end
      other when is_binary(other) ->
        quote location: :keep, bind_quoted: [opts: opts] do
          other = other |> String.to_atom()
          IO.inspect("Do not recognize shape #{other}")
        end
    end
  end
end
