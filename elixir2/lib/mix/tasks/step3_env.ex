defmodule Mal.Types.Function do
  def identity([x]) do
    x
  end
  defstruct name: "identity", value: &Mal.Types.Function.identity/1
end

defmodule Mix.Tasks.Step3Env.Macros do
  alias Mal.Types.Function, as: Function
  defmacro defun(pattern, do: body) do
    quote do
      def unquote(pattern), do: unquote(body)
    end
  end
  
  defmacro defun(fun, args, do: body) do
    fname = Macro.to_string(fun)
    symbol = String.to_atom(fname)
    s2fref = fn(s) ->
      {f, _} = Code.eval_string("&Mix.Tasks.Step3Env.#{s}/1", [], __ENV__)
      f
    end
    quote do
      def unquote(symbol)(unquote(args)) do
        unquote(body)
      end
      Mal.Env.define(unquote(symbol), unquote(s2fref.(fname))) 
    end
  end
end

defmodule Mix.Tasks.Step3Env do
  use Mix.Task
  import Mix.Tasks.Step3Env.Macros
  import IEx

  def read(input) do
    Mal.Reader.read_str(input)
  end

 

  def print(output) do
    Mal.Printer.pr_str(output, print_readably: true)
  end


  def eval_ast(ast) do
    ev = &(eval(&1))

    case ast do
      sym when is_atom(ast) ->
        case Mal.Env.lookup(sym) do
          nil -> raise "symbol '#{print(sym)}' is unbound."
          res -> res
        end

      {:keyword, kw} ->
        ast

      _ when is_list(ast) ->
        Enum.map(ast, ev)

      {:array, lst} ->
        {:array, eval_ast(lst)}

      m when is_map(ast) ->
        Enum.map(m, fn ({k, v}) -> {ev.(k), ev.(v)} end)
        |> Map.new

      x when is_float(ast) ->
        if round(x) == x, do: round(x), else: x
      _ -> ast
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


  def eval(ast) do
    case ast do
      [] -> []
      [first_arg|args] ->
        case eval(first_arg) do
          fun when is_function(fun) ->
            fun.(Enum.map(args, &eval/1))
            |> float_int_conversion
          {:special, fun} when is_function(fun) ->
            fun.(args)
          _ -> raise "cannot call #{print(first_arg)} in #{print(ast)}"
          nil -> raise "function '#{print(first_arg)}' not found."
        end
      _ -> eval_ast(ast)
    end
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

  def define([name, value]) do
    val = eval(value)
    Mal.Env.define(name, val)
    val
  end

  def progn([]), do: nil
  def progn([f]), do: eval(f)
  def progn([f|fs]) do
    eval(f)
    progn(fs)
  end

  def let([{:array, bindings} | body]) do
    let([bindings|body])
  end

  def let([bindings | body]) do
    Mal.Env.let(bindings, &eval/1, fn () ->
      progn(body)
    end)
  end


  def run(args) do
    Mal.Env.start()
    Mal.Env.define(:+, &plus/1)
    Mal.Env.define(:-, &minus/1)
    Mal.Env.define(:*, &mul/1)
    Mal.Env.define(:/, &div/1)
    Mal.Env.define(:def!, {:special, &define/1})
    Mal.Env.define(:"let*", {:special, &let/1})
    loop()
  end
end
