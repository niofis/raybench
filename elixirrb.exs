defmodule Raybench do
  @width 1280
  @height 720
  @samples 50
  @max_depth 5

  defmodule Vector do
    defstruct x: 0, y: 0, z: 0
  end

  defp writeppm(data) do
    {ok, ppm} = File.open "elixirrb.ppm", [:write, {:encoding, :utf8}]
    IO.puts ppm "P3\n#{@width} #{@height}\n255"
    File.close ppm
  end

  def main do
    writeppm(1)
  end
end
