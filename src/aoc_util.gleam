import gleam/string
import gleam/dynamic.{type Dynamic}

@external(erlang, "aoc_util", "slurp")
fn slurp_impl(filename: Dynamic) -> Dynamic

pub fn slurp(filename: String) -> List(String) {
    let res = slurp_impl(dynamic.from(filename))
    let assert Ok(data) = res |> dynamic.list(of: dynamic.string)
    data
}

pub fn input(text: String) -> List(String) {
    string.split(text, on: "\n")
}
