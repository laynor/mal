defmodule Mix.Tasks.Step0Repl do
  use Mix.Task

  def read(input) do
    input
  end

  def print(output) do
    output
  end

  def eval(input) do
    input
  end

  def rep(input) do
    input
    |> read
    |> eval
    |> print
  end

  def readline(prompt) do
    IO.gets(prompt)
    |> String.trim("\n")
  end

  def loop() do
    readline("user> ")
    |> rep
    |> IO.puts
    loop()
  end

  def run(args) do
    loop()
  end
end
