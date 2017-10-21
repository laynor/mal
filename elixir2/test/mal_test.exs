defmodule MalTest do
  use ExUnit.Case
  doctest Mal

  test "greets the world" do
    assert Mal.hello() == :world
  end
end
