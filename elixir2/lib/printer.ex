defmodule Mal.Printer do
  defp unescape_first(str) do
    case str do
      ""           -> ""
      "\n" <> rest -> "\\n" <> rest
      "\b" <> rest -> "\\b" <> rest
      "\t" <> rest -> "\\t" <> rest
      "\r" <> rest -> "\\r" <> rest
      "\"" <> rest -> "\\\"" <> rest
      _   -> str
    end
  end
  def unescape(s) do
    s
    |> String.split(~r/[\n\b\t\r"]/, include_captures: true)
    |> Enum.map(&unescape_first/1)
    |> Enum.join
  end

  def pr_str(obj, opts \\ [print_readably: false]) 

  def pr_str(obj, opts) when is_list(obj) do
    body = obj
    |> Enum.map(&(pr_str(&1, opts)))
    |> Enum.join(" ")

    "(#{body})"
  end

  def pr_str(obj, opts) when is_map(obj) do
    body = obj
    |> Map.to_list
    |> Enum.map(fn ({x, y}) -> [x, y] end)
    |> Enum.reduce([], &(&2 ++ &1))
    |> Enum.map(&(pr_str(&1, opts)))
    |> Enum.join(" ")

    "{#{body}}"
  end

  def pr_str({:array, lst}, opts) do
    Enum.map_join(lst, " ", fn (obj) ->
      pr_str(obj, opts)
    end)
    body = lst
    |> Enum.map(&(pr_str(&1, opts)))
    |> Enum.join(" ")

    "[#{body}]"
  end

  def pr_str(obj, opts) when is_binary(obj) do
    if opts[:print_readably] do
      "\"" <> unescape(obj) <> "\""
    else
      obj
    end
  end

  def pr_str(obj, opts) when is_function(obj) do
    "<function>"
  end

  def pr_str({:keyword, keyword}, opts) do
    keyword
  end

  def pr_str(obj, opts) do
    to_string(obj)
  end
end
