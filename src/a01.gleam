import gleam/io
import aoc_util

pub fn main() {
    let data = aoc_util.slurp("inputs/01.txt")
    io.println("Hello from a01! got lines:")
    print_lines(data)
}

fn print_lines(lines: List(String)) -> Nil {
    case lines {
        [] -> Nil
        [line, ..rest] -> {
            io.println(line)
            print_lines(rest)
        }
    }
}
