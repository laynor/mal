defmodule Mal.Arith do
  def plus(args) do
    Enum.reduce(args, 0, &+/2)
  end

  def minus([n]) do
    -n
  end

  def minus([n|ns]) do
    n - Enum.reduce(ns, &+/2)
  end

  def mul([]) do
    1
  end

  def mul(args) do
    Enum.reduce(args, 1, &*/2)
  end

  def div([x]) do
    1/x
  end

  def div([x|xs]) do
    x / Enum.reduce(xs, 1, &*/2)
  end

end
