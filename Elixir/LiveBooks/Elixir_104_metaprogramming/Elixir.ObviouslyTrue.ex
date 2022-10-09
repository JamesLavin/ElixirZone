defmodule ObviouslyTrue do
  def do_the_thing() do
    case (case true do
            false -> true
            true -> false
          end) do
      false -> nil
      true -> :fire_the_missiles
    end
  end
end