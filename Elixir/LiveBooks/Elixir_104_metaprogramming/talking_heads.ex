defmodule TalkingHeads do
  @funs [ask_yourself: "How did I get here?", no_party: "No disco!"]

  for {k, v} <- @funs do
    def unquote(k)(answer), do: answer
    def unquote(k)(), do: unquote(v)
  end
end
