defmodule Mal.Reader do

  defp tokenizer(input_text) do
    regex = ~r/[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*|")/
    res = Regex.scan(regex, input_text)
    |> Enum.drop(-1)
    |> Enum.map(fn ([_, y]) -> y end)

    if Enum.member?(res, "\"") do
      raise "mismatched \""
    else
      res
    end
  end

  defp wrap_form(keyword, tokens) do
    {form, rest} = pread_form(tokens)
    {[keyword, form], rest}
  end

  defp pread_form(["'"|ts]), do: wrap_form(:quote, ts)
  defp pread_form(["`"|ts]), do: wrap_form(:quasiquote, ts)
  defp pread_form(["~"|ts]), do: wrap_form(:unquote, ts)
  defp pread_form(["~@"|ts]), do: wrap_form(:"splice-unquote", ts)
  defp pread_form(["@"|ts]), do: wrap_form(:deref, ts)
  defp pread_form(["^"|ts]) do
    {meta, rest1} = pread_form(ts)
    {form, rest2} = pread_form(rest1)
    {[:"with-meta", form, meta], rest2}
  end
  defp pread_form(["("|ts]), do: read_list(ts)
  defp pread_form(["["|ts]), do: read_array(ts)
  defp pread_form(["{"|ts]), do: read_map(ts)
  defp pread_form([t|ts]), do: {read_atom(t), ts}

  defp escape_first(""), do: ""
  defp escape_first(str) do
    case str do
      "n" <> rest -> "\n" <> rest
      "b" <> rest -> "\b" <> rest
      "t" <> rest -> "\t" <> rest
      "r" <> rest -> "\r" <> rest
      _   -> str
    end
  end
  defp escape(s) do
    [first|rest] = String.split(s, "\\")
    first <> Enum.map_join(rest, &escape_first/1)
  end

  defp read_atom(")"), do: raise "mismatched )"
  defp read_atom("]"), do: raise "mismatched ]"

  defp read_atom("\"" <> _ = token) do
    token
    |> String.slice(1..-2)
    |> escape
  end

  defp read_atom(token) do
    if Regex.match?(~r/^\d+$/, token) do
      {res, _} = Integer.parse(token)
      res
    else
      String.to_atom( token )
    end
  end

  defp read_delim([delim|ts], delim), do: {[], ts}
  defp read_delim([], delim), do: raise "expected '#{delim}', got EOF"

  defp read_delim(tokens, delim) do
    {form, rest} = pread_form(tokens)
    {list, rest2} = read_delim(rest, delim)
    {[form|list], rest2}
  end

  defp read_list(tokens), do: read_delim(tokens, ")")
  defp read_array(tokens) do
    {lst, rest} = read_delim(tokens, "]")
    {{:array, lst}, rest}
  end

  defp read_form(tokens) do
    {obj, _} = pread_form(tokens)
    obj
  end

  defp read_map(tokens) do
    {lst, rest} = read_delim(tokens, "}")
    map = lst
    |> Enum.chunk_every(2)
    |> Enum.map( fn ([a,b]) -> {a,b} end)
    |> Map.new
    {map, rest}
  end

  def read_str(input_text) do
    input_text |> tokenizer |> read_form
  end
end
