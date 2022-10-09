defmodule ObviouslyTrue do
  require UnlessTrue

  def do_the_thing() do
    UnlessTrue.unless(true, do: :fire_the_missiles)
  end
end
