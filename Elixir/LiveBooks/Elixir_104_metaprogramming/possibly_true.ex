defmodule PossiblyTrue do
  require UnlessTrue

  def do_the_thing(true_or_false) do
    UnlessTrue.unless(true_or_false, do: :fire_the_missiles)
  end
end
