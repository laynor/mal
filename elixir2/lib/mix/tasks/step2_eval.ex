defmodule Mix.Tasks.Step2Eval do
  use Mix.Task
  import Mal.Reader
  import Mal.Printer
  import IEx
  

  def read(input) do
    read_str(input)
  end

  def print(output) do
    pr_str(output, print_readably: true)
  end


  def eval_ast(ast, env) do
    ev = &(eval(&1, env))

    case ast do
      _ when is_atom(ast)     -> env[ast]
      {:keyword, _}           -> ast
      _ when is_list(ast)     -> Enum.map(ast, ev)
      {:array, lst}           -> {:array, eval_ast(lst, env)}
      _ when is_map(ast)      ->
        Enum.map(ast, fn ({k, v}) -> {ev.(k), ev.(v)} end)
        |> Map.new
      _ when is_float(ast)    -> if round(ast) == ast, do: round(ast), else: ast
      _ when is_function(ast) -> "<function>" 
      _                       -> ast
    end
  end

  def float_int_conversion(x) when is_float(x) do
    if x == round(x) do
      round(x)
    else
      x
    end
  end
  def float_int_conversion(x) do
    x
  end

  def eval(ast, env) do
    evast = &(eval_ast(&1, env))
    ev = &(eval(&1, env))
    case ast do
      [] -> []
      [f|args] ->
        case eval_ast(f, env) do
          fun when is_function(fun) ->
            fun.(Enum.map(args, ev))
          nil -> raise "function '#{print(f)}' not found."
        end
        Enum.map(args, ev)
        |> evast.(f).()
        |> float_int_conversion
      _ -> evast.(ast)
    end
  end

  def rep(input) do
    repl_env = %{
      :+ => fn (xs) -> Enum.reduce(xs, 0, &+/2) end,
      :- => fn ([x|xs]) -> x - Enum.reduce(xs, 0, &+/2) end,
      :* => fn (xs) -> Enum.reduce(xs, 1, &*/2) end,
      :/ => fn ([x|xs]) -> x / Enum.reduce(xs, 1, &*/2) end
    }
    try do
      input
      |> read
      |> eval(repl_env)
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
