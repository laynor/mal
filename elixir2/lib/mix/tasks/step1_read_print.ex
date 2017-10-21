defmodule Mix.Tasks.Step1ReadPrint do
  use Mix.Task
  import Mal.Reader
  import Mal.Printer

  def read(input) do
    read_str(input)
  end

  def print(output) do
    pr_str(output, print_readably: true)
  end

  def eval(input) do
    input
  end

  def rep(input) do
    try do
      input
      |> read
      |> eval
      |> print
    rescue
      e in RuntimeError -> IO.puts "Error: #{e.message}"
    end
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
