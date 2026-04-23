package main

import "core:flags"
import "core:fmt"
import "core:os"
import "core:strings"
import "core:time"
import "core:unicode"

Error :: union {
  os.Error,
  bool,
}

run_process :: proc(cmd: []string) -> Error {
  process_desc := os.Process_Desc {
    command = cmd,
    stdin   = os.stdin,
    stdout  = os.stdout,
    stderr  = os.stderr,
  }
  handle := os.process_start(process_desc) or_return
  state := os.process_wait(handle) or_return
  return state.success
}

Target :: struct {
  name:  string,
  deps:  [dynamic]string,
  execs: [dynamic][dynamic]string,
}

Set :: struct {
  name:  string,
  value: string,
}

Item :: union {
  Target,
  Set,
}
Items :: [dynamic]Item

Parser :: struct {
  text:   string,
  cursor: int,
}

starts_with :: proc(parser: Parser, pattern: string) -> bool {return strings.has_prefix(parser.text[parser.cursor:], pattern)}
skip_whitespace :: proc(parser: ^Parser) {for strings.has_prefix(parser.text[parser.cursor:], " ") do parser.cursor += 1}
skip_newline :: proc(parser: ^Parser) {for parser.cursor < len(parser.text) && (parser.text[parser.cursor] == '\n' || parser.text[parser.cursor] == '\r') do parser.cursor += 1}
expect :: proc(parser: ^Parser, pattern: string) -> bool {
  skip_whitespace(parser)
  if starts_with(parser^, pattern) {
    parser.cursor += len(pattern)
    return true
  }
  fmt.eprintfln("Expected %v at location %v but got %v", pattern, parser.cursor, parser.text[parser.cursor:parser.cursor + len(pattern)])
  return false
}
current_symbol :: proc(parser: Parser) -> u8 {return parser.text[parser.cursor]}
advance :: proc(parser: ^Parser, n: int) -> bool {
  parser.cursor += n
  if parser.cursor >= len(parser.text) do return false
  return true
}
parse_name :: proc(parser: ^Parser) -> (res: string, ok: bool) {
  sb := strings.builder_make()
  quoted_shit := false
  if current_symbol(parser^) == '"' {
    advance(parser, 1) or_return
    quoted_shit = true
  }
  if quoted_shit {
    cs := current_symbol(parser^)
    escaped := false
    for cs != '"' || (cs == '"' && escaped) {
      escaped = false
      if !escaped && cs == '\\' {
        escaped = true
      } else {
        strings.write_byte(&sb, cs)
      }
      advance(parser, 1) or_return
      cs = current_symbol(parser^)
    }
    expect(parser, "\"") or_return
  } else {
    cs := current_symbol(parser^)
    for cs != ' ' && cs != '|' && cs != '\n' && cs != '\r' {
      strings.write_byte(&sb, cs)
      advance(parser, 1) or_return
      cs = current_symbol(parser^)
    }
  }
  return strings.to_string(sb), true
}

expand_vars :: proc(input: string, env: map[string]string) -> (res: string) {
  if !strings.contains(input, "$") do return input
  sb := strings.builder_make()
  lil_sb := strings.builder_make()
  gathering := false
  for c, idx in input {
    if c == '$' {
      if idx < len(input) - 1 && input[idx + 1] == '$' do continue
      strings.builder_reset(&lil_sb)
      gathering = true
      continue
    }
    if gathering {
      if !unicode.is_alpha(c) && !unicode.is_digit(c) && c != '_' {
        gathering = false
        strings.write_rune(&sb, c)
      } else {
        strings.write_rune(&lil_sb, c)
        partial := strings.to_string(lil_sb)
        if partial in env {
          strings.write_string(&sb, env[partial])
          gathering = false
        } else {
          val, found := os.lookup_env(partial, context.allocator)
          if found {
            strings.write_string(&sb, val)
            gathering = false
          }
        }
      }
    } else {
      strings.write_rune(&sb, c)
    }
  }
  return strings.to_string(sb)
}

parse_exec :: proc(parser: ^Parser) -> (res: [dynamic]string, ok: bool) {
  res = make([dynamic]string)
  skip_whitespace(parser)
  cs := current_symbol(parser^)
  if cs == '}' do return nil, true
  for cs != '\n' && cs != '\r' {
    thing := parse_name(parser) or_return
    append(&res, thing)
    skip_whitespace(parser)
    cs = current_symbol(parser^)
  }
  return res, true
}

parse_target :: proc(parser: ^Parser) -> (res: Target, ok: bool) {
  expect(parser, "target") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  expect(parser, "|") or_return
  skip_whitespace(parser)
  deps := make([dynamic]string)
  for current_symbol(parser^) != ')' {
    dep := parse_name(parser) or_return
    append(&deps, dep)
    skip_whitespace(parser)
  }
  expect(parser, ")") or_return
  expect(parser, "{") or_return
  skip_newline(parser)
  execs := make([dynamic][dynamic]string)
  for {
    exec := parse_exec(parser) or_return
    if exec == nil do break
    append(&execs, exec)
    skip_whitespace(parser)
    skip_newline(parser)
  }
  expect(parser, "}") or_return
  return {name = name, deps = deps, execs = execs}, true
}

parse_set :: proc(parser: ^Parser) -> (res: Set, ok: bool) {
  if starts_with(parser^, "target") do return res, false
  expect(parser, "set") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  expect(parser, "|") or_return
  skip_whitespace(parser)
  value := parse_name(parser) or_return
  expect(parser, ")")
  return {name = name, value = value}, true
}

parse_file :: proc(filename: string) -> (res: Items, err: Error) {
  data := os.read_entire_file(filename, context.allocator) or_return
  text := string(data)
  parser := Parser{text, 0}
  targets := make([dynamic]Item)
  for parser.cursor < len(text) {
    cursor := parser.cursor
    set, ok_set := parse_set(&parser)
    if !ok_set {
      parser.cursor = cursor
      target := parse_target(&parser) or_return
      skip_newline(&parser)
      append(&targets, target)
    } else {
      skip_newline(&parser)
      append(&targets, set)
    }
  }
  return targets, nil
}

run_target :: proc(target: Target, env: map[string]string, rerun: bool = false) -> bool {
  name := target.name
  outdated := true
  for dep in target.deps do if !os.exists(dep) {
    fmt.eprintfln("File does not exist %v", dep)
    return false
  }
  if os.exists(name) {
    name_stat, err1 := os.stat(name, context.allocator)
    if err1 != nil do return false
    outdated = false
    for dep in target.deps {
      dep_stat, err := os.stat(dep, context.allocator)
      if err != nil do return false
      if time.diff(dep_stat.modification_time, name_stat.modification_time) < 0 {
        outdated = true
        break
      }
    }
  }
  if outdated || rerun do for exec in target.execs {
    sb := strings.builder_make()
    processed_exec := make([dynamic]string)
    for thing in exec {
      new_thing := expand_vars(thing, env)
      append(&processed_exec, new_thing)
      strings.write_string(&sb, new_thing)
      strings.write_byte(&sb, ' ')
    }
    fmt.println("Running: ", strings.to_string(sb))
    process_result := run_process(processed_exec[:])
    switch v in process_result {
    case bool: if !v do fmt.eprintln("Process failed")
    case os.Error: if v != nil do fmt.eprintfln("Process failed with %v", v)
    }
  }
  return true
}

Options :: struct {
  rerun: bool `args:"name=B" usage:"Rerun no matter what"`,
}

main :: proc() {
  when ODIN_DEBUG {
    debug_stuff()
  }
  items, err := parse_file("test.caras")
  switch v in err {
  case bool: if !v do fmt.println("Fail")
  case os.Error: if v != nil do fmt.println(v)
  }
  opts: Options
  flags.parse_or_exit(&opts, os.args)
  env := make(map[string]string)
  for item in items {
    switch v in item {
    case Target: {
          success := run_target(v, env, opts.rerun)
          if success {
            fmt.printfln("Target %v run successfully", v.name)
          } else {
            fmt.printfln("Target %v failed", v.name)
          }
        }
    case Set: {
          value := expand_vars(v.value, env)
          env[v.name] = value
        }
    }
  }
}

