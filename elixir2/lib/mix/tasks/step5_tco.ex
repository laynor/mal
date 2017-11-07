defmodule Mix.Tasks.Step4IfFnDo do
  use Mix.Task
  import IEx
  import Mal.Arith
  alias Mal.Env, as: Env

  # Utils
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

  def wrap_fn(fun) do
    {:fun, fun, :env, %{}}
  end

  def wrap_fn(fun, env) do
    {:fun, fun, :env, env}
  end

  def to_list(obj) do
    case obj do
      l when is_list(l)  -> l
      t when is_tuple(t) -> Tuple.to_list(t)
    end
  end


  # read
  def read(input) do
    Mal.Reader.read_str(input)
  end

  # print
  def print(output) do
    Mal.Printer.pr_str(output, print_readably: true)
  end


  # eval
  def eval_ast(ast, env) do
    ev     = &(eval(&1, env))

    evpair = fn({k, v}) ->
      {eval(k, env), eval(v, env)}
    end

    case ast do
      x when x in [nil, true, false] -> ast
      fun when is_function(fun)      -> ast
      {:keyword, _}                  -> ast
      x when is_float(x)             -> float_int_conversion(x)


      list when is_list(list) -> Enum.map(list, ev)

      {:array, lst} -> {:array, eval_ast(lst, env)}

      m when is_map(ast) -> Enum.map(evpair) |> Map.new

      sym when is_atom(sym) ->
        case Mal.Env.find(env, sym) do
          nil -> raise "symbol '#{print(sym)}' is unbound."
          res -> res
        end

      _ -> ast
    end
  end

  def eval(ast, env) do
    case ast do
      [] -> []
      [nil] -> nil
      [func|args] ->
        case eval(func, env) do
          fun when is_function(fun) ->
            args
            |> Enum.map(&(eval(&1, env)))
            |> fun.()
          {:special, fun} when is_function(fun) ->
            fun.(env, args)
          nil -> raise "function '#{print(func)}' not found."
          res -> raise "cannot call #{print(func)}/#{inspect(res)} in #{print(ast)}"
        end
      _ -> eval_ast(ast, env)
    end
  end

  # read eval print

  def rep(input, env) do
    try do
      input
      |> read
      |> eval(env)
      |> print
    rescue
      e in RuntimeError -> IO.puts "Error: #{e.message}"
    end
  end

  # terminal input output
  def readline(prompt) do
    IO.gets(prompt)
    |> String.trim("\n")
  end

  # main loop

  def loop(env) do
    readline("user> ")
    |> rep(env)
    |> IO.puts
    loop(env)
  end


  # def!
  def define(env, [sym, value]) do
    Mal.Env.put(env, sym, :placeholder)
    val = eval(value, env)
    Mal.Env.put(env, sym, val)
    val
  end

  # do support
  def eval_seq(env, []),    do: nil
  def eval_seq(env, [f]),   do: eval(f, env)
  def eval_seq(env, [f|fs]) do
    eval(f, env)
    eval_seq(env, fs)
  end

  # let
  def let(env, [{:array, bindings} | body]), do: let(env, [bindings|body])
  def let(env, [bindings | body]) do
    {:ok, new_env} = Mal.Env.start_link(Mal.Env.env(env))
    evbindings = bindings
    |> Enum.chunk_every(2)
    |> Enum.map(fn([sym, val]) -> Mal.Env.put(new_env, sym, eval(new_env, val)) end)
    eval_seq(new_env, body)
  end

  # if
  def if_(env, [condition, then]) do
    if eval(condition, env) do
      eval(then, env)
    else
      nil
    end
  end

  def if_(env, [condition, then, else_]) do
    if eval(condition, env) do
      eval(then, env)
    else
      eval(else_, env)
    end
  end

  # boolean
  def not_([x]) do
    case x do
      b when is_boolean(b)
        -> not b

      nil ->
        true

      _ ->
        false
    end
  end

  def and_(env, [])    , do: true
  def and_(env, [x])   , do: eval(env, x)
  def and_(env, [x|xs]), do: eval(env, x) && and_(env, xs)

  def or_(env, [])    , do: false
  def or_(env, [x|xs]), do: eval(x, env) || or_(env, xs)



  def bind_params(env, params, args) do
    {pos_params, more_params} = Enum.split_while(params, &(&1 != :&))

    {syms, vals} = case more_params do

                     [:&, more] ->
                       {pos_args, more_args} = Enum.split(args, Enum.count(pos_params))
                       {pos_params ++ [more], pos_args ++ [more_args]}

                     [] ->
                       {params, args}

                   end

    Enum.each syms, fn(sym) ->
      Mal.Env.del(env, sym)
    end

    Enum.zip(syms, vals)
    |> Enum.each(fn ({sym, val}) ->
      Mal.Env.put(env, sym, val)
    end)
  end

  def fn_star(env, [{:array, formal_params}|body]), do: fn_star(env, [formal_params|body])

  def fn_star(env, [params|body]) do
    fun = fn (args) ->
      {:ok, closed_env} = Mal.Env.start_link(env)
      bind_params(closed_env, params, args)
      eval([:do|body], closed_env)
    end
  end

  # print
  def print_([arg]) do
    arg
    |> print
    |> IO.puts
    arg
  end

  # prn
  def prn([]) do
    IO.puts("")
    nil
  end

  def prn(args) do
    args
    |> Enum.map(fn (arg) -> Mal.Printer.pr_str(arg, print_readably: true) end)
    |> Enum.join(" ")
    |> IO.puts
    nil
  end

  def println(args) do
    args
    |> Enum.map(&(Mal.Printer.pr_str(&1, print_readably: false)))
    |> Enum.join(" ")
    |> IO.puts
    nil
  end

  def pr_str([]) do
    ""
  end
  def pr_str(args) do
    args
    |> Enum.map(&(Mal.Printer.pr_str(&1, print_readably: true)))
    |> Enum.join(" ")
  end

  def str([]) do
    ""
  end
  def str([arg]) when is_binary(arg) do
    arg
  end
  def str(args) do
    args
    |> Enum.map(&(Mal.Printer.pr_str(&1, print_readably: false)))
    |> Enum.join("")
  end
  # list
  def list(args), do: args

  def list?([arg]), do: is_list(arg)

  # seq
  def empty?([{:array, ary}]), do: empty?([ary])
  def empty?([arg]), do: arg == []

  def count([nil]), do: 0
  def count([{:array, lst}]) do
    Enum.count(lst)
  end
  def count([arg]), do: Enum.count(arg)

  # eq
  def equal([{:array, ary}, x]) do
    equal([ary, x])
  end
  def equal([x, {:array, ary}]) do
    equal([x, ary])
  end
  def equal([[], []]) do
    true
  end
  def equal([[], _]) do
    false
  end
  def equal([_, []]) do
    false
  end
  def equal([[x|xs], [y|ys]]) do
    equal([x, y]) and (equal([xs, ys]))
  end
  def equal([arg1, arg2]), do: arg1 == arg2

  # comparison
  def lt([arg1, arg2]), do: arg1 < arg2
  def gt([arg1, arg2]), do: arg1 > arg2

  def lte([arg1, arg2]), do: arg1 <= arg2
  def gte([arg1, arg2]), do: arg1 >= arg2


  def with_env(fun) do
    fn (env, args) ->
      fun.(args)
    end
  end

  def quote_(env, [ast]) do
    ast
  end

  def core_env do
    core_ns = %{
      :def!     => {:special, &define/2},
      :if       => {:special, &if_/2},
      :"let*"   => {:special, &let/2},
      :do       => {:special, &eval_seq/2},
      :"fn*"    => {:special, &fn_star/2},
      :and      => {:special, &and_/2},
      :or       => {:special, &or_/2},
      :"quote"    => {:special, &quote_/2},
      :print    => &print_/1,
      :prn      => &prn/1,
      :read     => &read/1,
      :+        => &plus/1,
      :-        => &minus/1,
      :*        => &mul/1,
      :/        => &div/1,
      :list     => &list/1,
      :list?    => &list?/1,
      :empty?   => &empty?/1,
      :count    => &count/1,
      :=        => &equal/1,
      :<        => &lt/1,
      :>        => &gt/1,
      :<=       => &lte/1,
      :>=       => &gte/1,
      :not      => &not_/1,
      :println  => &println/1,
      :"pr-str" => &pr_str/1,
      :str      => &str/1
    }
    {:ok, env} = Mal.Env.start_link(core_ns)
    env
  end

  def run(_args) do
    loop(core_env)
  end
end
