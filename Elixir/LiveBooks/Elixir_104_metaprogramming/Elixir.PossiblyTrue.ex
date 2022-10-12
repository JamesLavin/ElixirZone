defmodule PossiblyTrue do
  def do_the_thing(true_or_false) do
    case (case true_or_false do
            x when :erlang.orelse(:erlang."=:="(x, false), :erlang."=:="(x, nil)) -> true
            _ -> false
          end) do
      false -> nil
      true -> :fire_the_missiles
    end
  end
end