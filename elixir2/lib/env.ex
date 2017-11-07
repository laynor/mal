defmodule Mal.Env do
  use Agent

  @serial 1
  @parent 0

  def start_link do
    Agent.start_link fn -> %{@serial => 0} end
  end

  def empty_env(parent) do
    %{@parent => parent}
  end

  # top down
  # Mal.Env.new parent

  def new(state, name, parent) when is_map(state) do
    Map.put(state, name, empty_env(parent))
  end

  def new({pid, parent}, name) when is_pid(pid) or is_nil(parent) do
    Agent.update pid, fn state -> new(state, name, parent) end
    {pid, name}
  end

  def new(env) when not is_pid(env) do
    {pid, _} = env
    serial = Agent.get_and_update pid, fn state ->
      {
        state[@serial],
        Map.put(state, @serial, state[@serial] + 1)
      }
    end
    name = "anonyous_#{serial}"
    new(env, name)
  end

  def new(pid) when is_pid(pid) do
    new({pid, nil})
  end


  def find(state, id, sym) do
    env = state[id]
    case {env[sym], env[@parent]} do
      {nil, nil}       -> nil
      {nil, parent_id} -> find(state, parent_id, sym)
      {val, _}         -> val
    end
  end

  def get({pid, id}, sym) do
    Agent.get pid, &(find(&1, id, sym))
  end

  def put({pid, id}, sym, val) do
    Agent.update pid, fn state ->
      updated_env = Map.put(state[id], sym, val)
      Map.put(state, id, updated_env)
    end
    val
  end

  def del({pid, id}, sym) do
    Agent.update pid, fn state -> Map.delete(state[id], sym) end
  end

end
