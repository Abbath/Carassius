package main

import "core:flags"
import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:time"
import "core:unicode"

Env :: distinct map[string]string

Error :: union {
  os.Error,
  bool,
}

run_process :: proc(cmd: []string) -> (exit_code: int, err: Error) {
  process_desc := os.Process_Desc {
    command = cmd,
    stdin   = os.stdin,
    stdout  = os.stdout,
    stderr  = os.stderr,
  }
  handle := os.process_start(process_desc) or_return
  state := os.process_wait(handle) or_return
  return state.exit_code, state.success
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

Unset :: struct {
  name: string,
}

Op :: enum {
  EQ,
  NE,
  LT,
  GT,
  LE,
  GE,
  AND,
  OR,
}

Operand :: union {
  Cond,
  string,
  int,
  bool,
}

Cond :: struct {
  operand1: ^Operand,
  operand2: ^Operand,
  operator: Op,
}

If :: struct {
  cond:      Operand,
  jump_else: int,
  jump_end:  int,
}

Else :: struct {
  exec: bool,
  jump: int,
}

EndIf :: struct {}

Fart :: struct {
  whisper: string,
}

Item :: union {
  Target,
  Set,
  Unset,
  If,
  Else,
  EndIf,
  Fart,
}
Items :: [dynamic]Item

Parser :: struct {
  text:   string,
  cursor: int,
}

compute_cond :: proc(cond: Cond, env: Env) -> Operand {
  switch cond.operator {
  case .AND: return op_to_bool(cond.operand1^, env) && op_to_bool(cond.operand2^, env)
  case .OR: return op_to_bool(cond.operand1^, env) || op_to_bool(cond.operand2^, env)
  case .EQ: return compute_op(cond.operand1^, env) == compute_op(cond.operand2^, env)
  case .NE: return compute_op(cond.operand1^, env) != compute_op(cond.operand2^, env)
  case .LT: return false
  case .GT: return false
  case .LE: return false
  case .GE: return false
  }
  return true
}

compute_op :: proc(op: Operand, env: Env) -> Operand {
  switch v in op {
  case Cond: return compute_cond(v, env)
  case string: return expand_vars(v, env)
  case int, bool: return v
  }
  return nil
}

op_to_bool :: proc(op: Operand, env: Env) -> bool {
  switch v in op {
  case Cond: return op_to_bool(compute_cond(v, env), env)
  case string:
    x := expand_vars(v, env)
    return x == "true" || x == "1"
  case int: return v != 0
  case bool: return v
  }
  return false
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
    for cs != ' ' && cs != '|' && cs != '\n' && cs != '\r' && cs != ')' {
      strings.write_byte(&sb, cs)
      advance(parser, 1) or_return
      cs = current_symbol(parser^)
    }
  }
  return strings.to_string(sb), true
}

expand_vars :: proc(input: string, env: Env) -> (res: string) {
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
  expect(parser, "set") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  expect(parser, "|") or_return
  skip_whitespace(parser)
  value := parse_name(parser) or_return
  expect(parser, ")") or_return
  return {name = name, value = value}, true
}

parse_unset :: proc(parser: ^Parser) -> (res: Unset, ok: bool) {
  expect(parser, "unset") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  expect(parser, ")") or_return
  return {name = name}, true
}

parse_term :: proc(parser: ^Parser) -> (res: Operand, ok: bool) {
  skip_whitespace(parser)
  thing := parse_name(parser) or_return
  val, ok_int := strconv.parse_int(thing)
  if ok_int {
    return val, true
  }
  if thing == "true" || thing == "false" {
    return strconv.parse_bool(thing)
  }
  return thing, true
}

parse_eq :: proc(parser: ^Parser) -> (res: Operand, ok: bool) {
  operand1 := parse_term(parser) or_return
  skip_whitespace(parser)
  eq := starts_with(parser^, "==")
  neq := starts_with(parser^, "/=")
  if eq || neq {
    advance(parser, 2) or_return
    cond: Cond
    cond.operand1 = new(Operand)
    cond.operand2 = new(Operand)
    cond.operator = .EQ if eq else .NE
    cond.operand1^ = operand1
    cond.operand2^ = parse_eq(parser) or_return
    return cond, true
  }
  return operand1, true
}

parse_or :: proc(parser: ^Parser) -> (res: Operand, ok: bool) {
  operand1 := parse_eq(parser) or_return
  skip_whitespace(parser)
  if starts_with(parser^, "||") {
    advance(parser, 2) or_return
    cond: Cond
    cond.operand1 = new(Operand)
    cond.operand2 = new(Operand)
    cond.operator = .OR
    cond.operand1^ = operand1
    cond.operand2^ = parse_or(parser) or_return
    return cond, true
  }
  return operand1, true
}

parse_op :: proc(parser: ^Parser) -> (res: Operand, ok: bool) {
  operand1 := parse_or(parser) or_return
  skip_whitespace(parser)
  if starts_with(parser^, "&&") {
    advance(parser, 2) or_return
    cond: Cond
    cond.operand1 = new(Operand)
    cond.operand2 = new(Operand)
    cond.operator = .AND
    cond.operand1^ = operand1
    cond.operand2^ = parse_op(parser) or_return
    return cond, true
  }
  return operand1, true
}

parse_if :: proc(parser: ^Parser) -> (res: If, ok: bool) {
  expect(parser, "if") or_return
  expect(parser, "(") or_return
  cond := parse_op(parser) or_return
  expect(parser, ")") or_return
  return {cond = cond}, true
}

parse_fart :: proc(parser: ^Parser) -> (res: Fart, ok: bool) {
  expect(parser, "whisper") or_return
  expect(parser, "(") or_return
  whisper := parse_name(parser) or_return
  expect(parser, ")") or_return
  return {whisper = whisper}, true
}

parse_file :: proc(filename: string) -> (res: Items, err: Error) {
  data := os.read_entire_file(filename, context.allocator) or_return
  text := string(data)
  parser := Parser{text, 0}
  items := make([dynamic]Item)
  for parser.cursor < len(text) {
    if starts_with(parser, "set") {
      set := parse_set(&parser) or_return
      skip_newline(&parser)
      append(&items, set)
    }
    if starts_with(parser, "unset") {
      unset := parse_unset(&parser) or_return
      skip_newline(&parser)
      append(&items, unset)
    }
    if starts_with(parser, "target") {
      target := parse_target(&parser) or_return
      skip_newline(&parser)
      append(&items, target)
    }
    if starts_with(parser, "if") {
      iff := parse_if(&parser) or_return
      skip_newline(&parser)
      append(&items, iff)
    }
    if starts_with(parser, "else") {
      expect(&parser, "else") or_return
      skip_newline(&parser)
      append(&items, Else{})
    }
    if starts_with(parser, "endif") {
      expect(&parser, "endif") or_return
      skip_newline(&parser)
      append(&items, EndIf{})
    }
    if starts_with(parser, "whisper") {
      fart := parse_fart(&parser) or_return
      skip_newline(&parser)
      append(&items, fart)
    }
  }
  return items, nil
}

run_target :: proc(target: Target, env: Env, rerun: bool = false) -> (success: bool, run: bool) {
  name := target.name
  outdated := true
  for dep in target.deps do if !os.exists(dep) {
    fmt.eprintfln("File does not exist %v", dep)
    return false, false
  }
  if os.exists(name) {
    name_stat, err1 := os.stat(name, context.allocator)
    if err1 != nil do return false, false
    outdated = false
    for dep in target.deps {
      dep_stat, err := os.stat(dep, context.allocator)
      if err != nil do return false, false
      if time.diff(dep_stat.modification_time, name_stat.modification_time) < 0 {
        outdated = true
        break
      }
    }
  }
  run = false
  success = true
  if outdated || rerun {
    run = true
    for exec in target.execs {
      sb := strings.builder_make()
      processed_exec := make([dynamic]string)
      for thing in exec {
        new_thing := expand_vars(thing, env)
        append(&processed_exec, new_thing)
        strings.write_string(&sb, new_thing)
        strings.write_byte(&sb, ' ')
      }
      fmt.println("Running:", strings.to_string(sb))
      exit_code, process_result := run_process(processed_exec[:])
      if exit_code != 0 {
        fmt.eprintfln("Process failed with code: %v", exit_code)
        success = false
      }
      switch v in process_result {
      case bool: if !v {
            fmt.eprintln("Process failed")
            success = false
          }
      case os.Error: if v != nil {
            fmt.eprintfln("Process failed with %v", v)
            success = false
          }
      }
    }
  }
  return success, run
}

grind_items :: proc(items: ^[dynamic]Item) -> bool {
  last_if_idx := -1
  last_else_idx := -1
  for &item, idx in items {
    #partial switch v in item {
    case If: last_if_idx = idx
    case Else:
      if last_if_idx != -1 {
        x, ok := &items[last_if_idx].(If)
        if !ok do return false
        x.jump_else = idx
      }
      last_else_idx = idx
    case EndIf:
      if last_if_idx != -1 {
        x, ok := &items[last_if_idx].(If)
        if !ok do return false
        x.jump_end = idx
        last_if_idx = -1
      }
      if last_else_idx != -1 {
        x, ok := &items[last_else_idx].(Else)
        if !ok do return false
        x.jump = idx
        last_else_idx = -1
      }
    }
  }
  return true
}

Options :: struct {
  target: string `args:"pos=0" usage:"Specific target name"`,
  rerun:  bool `args:"name=B" usage:"Rerun no matter what"`,
}

main :: proc() {
  when ODIN_DEBUG {
    debug_stuff()
  }
  opts: Options
  flags.parse_or_exit(&opts, os.args)
  items, err := parse_file("test.caras")
  switch v in err {
  case bool: if !v {
        fmt.eprintln("Fail")
        return
      }
  case os.Error: if v != nil do fmt.eprintln(v)
  }
  ifs_ok := grind_items(&items)
  if !ifs_ok {
    fmt.eprintln("Ifs and elses are borked")
    return
  }
  env := make(Env)
  for index := 0; index < len(items); index += 1 {
    item := items[index]
    switch v in item {
    case Target:
      if opts.target != "" && v.name != opts.target do continue
      success, run := run_target(v, env, opts.rerun)
      if success {
        if run do fmt.printfln("Target %v run successfully", v.name)
        else do fmt.println("Nothing to do")
      } else {
        fmt.printfln("Target %v failed", v.name)
      }
    case Set:
      value := expand_vars(v.value, env)
      env[expand_vars(v.name, env)] = value
    case Unset:
      name := expand_vars(v.name, env)
      if name in env {
        delete_key(&env, name)
      } else {
        fmt.eprintln("Key was not there in the first place")
      }
    case If:
      if v.jump_end == 0 {
        fmt.eprintln("Unbounded if")
        break
      }
      val := op_to_bool(v.cond, env)
      if v.jump_else != 0 {
        x, ok := &items[v.jump_else].(Else)
        if !ok {
          fmt.eprintln("This is not else")
          break
        }
        x.exec = !val
      }
      if !val do index = v.jump_else != 0 ? v.jump_else : v.jump_end
    case Else:
      if v.jump == 0 {
        fmt.eprintln("Unbounded else")
        break
      }
      if !v.exec do index = v.jump
    case EndIf: {}
    case Fart: fmt.println(expand_vars(v.whisper, env))
    }
  }
}

