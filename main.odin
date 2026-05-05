#+feature using-stmt
package main

import "core:flags"
import "core:fmt"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:time"
import "core:unicode"

SENTINEL :: -1

Entry :: union {
  string,
  [dynamic]string,
}

Env :: distinct map[string]Entry

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
Targets :: distinct map[string]Target

Set :: struct {
  name:   string,
  values: [dynamic]string,
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

Loop :: struct {
  name:     string,
  elements: [dynamic]string,
  cursor:   int,
  jump:     int,
}

EndLoop :: struct {
  jump: int,
}

Build :: struct {
  name: string,
}

Item :: union {
  Build,
  Set,
  Unset,
  If,
  Else,
  EndIf,
  Fart,
  Loop,
  EndLoop,
}
Items :: distinct [dynamic]Item

Parser :: struct {
  text:   string,
  cursor: int,
}

compare_ops :: proc(op: Op, op1: Operand, op2: Operand, env: Env) -> bool {
  using slice.Ordering
  if type_of(op1) == Cond && type_of(op2) != Cond {
    v := op1.(Cond) or_return
    return compare_ops(op, compute_cond(v, env), op2, env)
  }
  if type_of(op1) != Cond && type_of(op2) == Cond {
    v := op2.(Cond) or_return
    return compare_ops(op, op1, compute_cond(v, env), env)
  }
  if type_of(op1) != type_of(op2) do return false
  switch v1 in op1 {
  case Cond:
    v2 := op2.(Cond)
    return compare_ops(op, compute_cond(v1, env), compute_cond(v2, env), env)
  case int:
    v2 := op2.(int) or_return
    #partial switch op {
    case .LT: return v1 < v2
    case .GT: return v1 > v2
    case .LE: return v1 <= v2
    case .GE: return v1 >= v2
    }
  case bool:
    v2 := op2.(bool) or_return
    #partial switch op {
    case .LT: return int(v1) < int(v2)
    case .GT: return int(v1) > int(v2)
    case .LE: return int(v1) <= int(v2)
    case .GE: return int(v1) >= int(v2)
    }
  case string:
    v2 := op2.(string) or_return
    #partial switch op {
    case .LT: return v1 < v2
    case .GT: return v1 > v2
    case .LE: return v1 <= v2
    case .GE: return v1 >= v2
    }
  }
  return false
}

compute_cond :: proc(cond: Cond, env: Env) -> Operand {
  switch cond.operator {
  case .AND: return op_to_bool(cond.operand1^, env) && op_to_bool(cond.operand2^, env)
  case .OR: return op_to_bool(cond.operand1^, env) || op_to_bool(cond.operand2^, env)
  case .EQ: return compute_op(cond.operand1^, env) == compute_op(cond.operand2^, env)
  case .NE: return compute_op(cond.operand1^, env) != compute_op(cond.operand2^, env)
  case .LT, .GT, .LE, .GE: return compare_ops(cond.operator, compute_op(cond.operand1^, env), compute_op(cond.operand2^, env), env)
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
      if !escaped && cs == '\\' do escaped = true
      else {
        if escaped && slice.contains([]u8{'n', 'r', 't', 'b'}, cs) {
          switch cs {
          case 'n': cs = '\n'
          case 'r': cs = '\r'
          case 't': cs = '\t'
          case 'b': cs = '\b'
          }
        }
        strings.write_byte(&sb, cs)
        escaped = false
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

roll_string :: proc(loong_boi: string, sb: ^strings.Builder, env: Env) {
  for l := len(loong_boi); l >= 1; l -= 1 {
    partial := loong_boi[:l]
    if partial in env {
      entry := env[partial]
      switch v in entry {
      case string: strings.write_string(sb, v)
      case [dynamic]string: for s, i in v {
            if i != 0 do strings.write_byte(sb, ' ')
            strings.write_string(sb, s)
          }
      }
      break
    } else {
      val, found := os.lookup_env(partial, context.allocator)
      if found {
        strings.write_string(sb, val)
        break
      }
    }
  }
}

expand_vars :: proc(input: string, env: Env) -> (res: string) {
  if !strings.contains(input, "$") do return input
  sb := strings.builder_make()
  lil_sb := strings.builder_make()
  gathering := false
  skip := false
  for c, idx in input {
    if skip {
      skip = false
      continue
    }
    if c == '$' {
      if idx < len(input) - 1 && input[idx + 1] == '$' {
        strings.write_string(&sb, "$$")
        skip = true
        continue
      }
      strings.builder_reset(&lil_sb)
      gathering = true
      continue
    }
    if gathering {
      if !munch {
        if !unicode.is_alpha(c) && !unicode.is_digit(c) && c != '_' {
          gathering = false
          strings.write_rune(&sb, c)
        } else {
          strings.write_rune(&lil_sb, c)
          partial := strings.to_string(lil_sb)
          if partial in env {
            entry := env[partial]
            switch v in entry {
            case string: strings.write_string(&sb, v)
            case [dynamic]string: for s, i in v {
                  if i != 0 do strings.write_byte(&sb, ' ')
                  strings.write_string(&sb, s)
                }
            }
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
        if !unicode.is_alpha(c) && !unicode.is_digit(c) && c != '_' {
          loong_boi := strings.to_string(lil_sb)
          roll_string(loong_boi, &sb, env)
          gathering = false
          strings.write_rune(&sb, c)
        } else do strings.write_rune(&lil_sb, c)
      }
    } else do strings.write_rune(&sb, c)
  }
  if loong_boi := strings.to_string(lil_sb); munch && len(loong_boi) != 0 {
    roll_string(loong_boi, &sb, env)
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
  skip_whitespace(parser)
  deps := make([dynamic]string)
  if starts_with(parser^, "|") {
    expect(parser, "|") or_return
    skip_whitespace(parser)
    for current_symbol(parser^) != ')' {
      dep := parse_name(parser) or_return
      append(&deps, dep)
      skip_whitespace(parser)
    }
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

parse_build :: proc(parser: ^Parser) -> (res: Build, ok: bool) {
  expect(parser, "build") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  expect(parser, ")") or_return
  return {name = name}, true
}

parse_set :: proc(parser: ^Parser) -> (res: Set, ok: bool) {
  expect(parser, "set") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  skip_whitespace(parser)
  values := make([dynamic]string)
  if starts_with(parser^, "|") {
    expect(parser, "|") or_return
    skip_whitespace(parser)
    for current_symbol(parser^) != ')' {
      value := parse_name(parser) or_return
      append(&values, value)
      skip_whitespace(parser)
    }
  }
  expect(parser, ")") or_return
  return {name = name, values = values}, true
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
  if ok_int do return val, true
  if thing == "true" || thing == "false" do return strconv.parse_bool(thing)
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
  return {cond = cond, jump_end = SENTINEL, jump_else = SENTINEL}, true
}

parse_fart :: proc(parser: ^Parser) -> (res: Fart, ok: bool) {
  expect(parser, "whisper") or_return
  expect(parser, "(") or_return
  whisper := parse_name(parser) or_return
  expect(parser, ")") or_return
  return {whisper = whisper}, true
}

parse_loop :: proc(parser: ^Parser) -> (res: Loop, ok: bool) {
  expect(parser, "loop") or_return
  expect(parser, "(") or_return
  name := parse_name(parser) or_return
  expect(parser, "|") or_return
  skip_whitespace(parser)
  elems := make([dynamic]string)
  for current_symbol(parser^) != ')' {
    dep := parse_name(parser) or_return
    append(&elems, dep)
    skip_whitespace(parser)
  }
  expect(parser, ")") or_return
  return {name = name, elements = elems, jump = SENTINEL}, true
}

parse_file :: proc(filename: string) -> (res1: Items, res2: Targets, err: Error) {
  data := os.read_entire_file(filename, context.allocator) or_return
  text := string(data)
  parser := Parser{text, 0}
  items := make(Items)
  targets := make(Targets)
  entrypoint_found := false
  for parser.cursor < len(text) {
    skip_whitespace(&parser)
    if !entrypoint_found {
      if starts_with(parser, "target") {
        target := parse_target(&parser) or_return
        skip_newline(&parser)
        targets[target.name] = target
        continue
      }
      if token := "entrypoint"; starts_with(parser, token) {
        expect(&parser, token) or_return
        expect(&parser, "{") or_return
        skip_newline(&parser)
        entrypoint_found = true
        continue
      }
      fmt.eprintfln("Illegal instruction on toplevel %v", parser.text[parser.cursor:])
      break
    } else {
      if starts_with(parser, "build") {
        build := parse_build(&parser) or_return
        skip_newline(&parser)
        append(&items, build)
        continue
      }
      if starts_with(parser, "set") {
        set := parse_set(&parser) or_return
        skip_newline(&parser)
        append(&items, set)
        continue
      }
      if starts_with(parser, "unset") {
        unset := parse_unset(&parser) or_return
        skip_newline(&parser)
        append(&items, unset)
        continue
      }
      if starts_with(parser, "if") {
        iff := parse_if(&parser) or_return
        skip_newline(&parser)
        append(&items, iff)
        continue
      }
      if token := "else"; starts_with(parser, token) {
        expect(&parser, token) or_return
        skip_newline(&parser)
        append(&items, Else{jump = SENTINEL})
        continue
      }
      if token := "endif"; starts_with(parser, token) {
        expect(&parser, token) or_return
        skip_newline(&parser)
        append(&items, EndIf{})
        continue
      }
      if starts_with(parser, "whisper") {
        fart := parse_fart(&parser) or_return
        skip_newline(&parser)
        append(&items, fart)
        continue
      }
      if starts_with(parser, "loop") {
        loop := parse_loop(&parser) or_return
        skip_newline(&parser)
        append(&items, loop)
        continue
      }
      if token := "endloop"; starts_with(parser, token) {
        expect(&parser, token) or_return
        skip_newline(&parser)
        append(&items, EndLoop{jump = SENTINEL})
        continue
      }
      if token := "}"; starts_with(parser, "}") {
        expect(&parser, token) or_return
        skip_newline(&parser)
        entrypoint_found = false
        continue
      }
      fmt.eprintfln("Illegal instruction in entrypoint %v", parser.text[parser.cursor:])
      break
    }
  }
  return items, targets, nil
}

run_target :: proc(name: string, targets: Targets, env: Env, already_run: ^[dynamic]string, rerun: bool = false) -> (success: bool, run: bool) {
  append(already_run, name)
  outdated := true
  target := targets[name]
  for dep in target.deps {
    if dep in targets && !slice.contains(already_run[:], dep) {
      s, r := run_target(dep, targets, env, already_run, rerun)
      if r && !s {
        fmt.eprintln("Subtarget failed")
        return false, false
      }
      continue
    }
    if !os.exists(dep) {
      fmt.eprintfln("File does not exist %v", dep)
      return false, false
    }
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

grind_items :: proc(items: ^Items) -> bool {
  last_if_idx := SENTINEL
  last_else_idx := SENTINEL
  last_loop_idx := SENTINEL
  for &item, idx in items {
    #partial switch &v in item {
    case If: last_if_idx = idx
    case Else:
      if last_if_idx != SENTINEL {
        x := (&items[last_if_idx].(If)) or_return
        x.jump_else = idx
      }
      last_else_idx = idx
    case EndIf:
      if last_if_idx != SENTINEL {
        x := (&items[last_if_idx].(If)) or_return
        x.jump_end = idx
        last_if_idx = SENTINEL
      }
      if last_else_idx != SENTINEL {
        x := (&items[last_else_idx].(Else)) or_return
        x.jump = idx
        last_else_idx = SENTINEL
      }
    case Loop: last_loop_idx = idx
    case EndLoop: if last_loop_idx != SENTINEL {
          x := (&items[last_loop_idx].(Loop)) or_return
          x.jump = idx
          v.jump = last_loop_idx
          last_loop_idx = SENTINEL
        }
    }
  }
  return true
}

print_items :: proc(items: Items) {
  for item, idx in items do fmt.println(idx, item)
}

Options :: struct {
  input:   string `args:"name=I" usage:"Input file (default: build.caras)"`,
  vars:    [dynamic]string `args:"name=D,manifold" usage:"User defined vars"`,
  targets: [dynamic]string `args:"name=T,manifold" usage:"Specific target names"`,
  rerun:   bool `args:"name=B" usage:"Rerun no matter what"`,
  debug:   bool `usage:"Debug prints"`,
  munch:   bool `args:"name=M" usage:"Maximal Munch for vars"`,
}

A :: union {
  string,
  int,
}

munch: bool = false

main :: proc() {
  when ODIN_DEBUG {
    debug_stuff()
  }
  opts := Options {
    input = "build.caras",
  }
  flags.parse_or_exit(&opts, os.args, .Unix)
  munch = opts.munch
  items, targets, err := parse_file(opts.input)
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
  if opts.debug do print_items(items)
  env := make(Env)
  for var in opts.vars {
    parts, err := strings.split_n(var, "=", 2)
    if err != .None {
      fmt.eprintln("Allocation error")
      return
    }
    if len(parts) == 2 do env[parts[0]] = parts[1]
    if len(parts) == 1 do env[parts[0]] = "true"
  }
  for index := 0; index < len(items); index += 1 {
    item := &items[index]
    switch &v in item {
    case Build:
      if v.name not_in targets {
        fmt.eprintfln("Target %v does not exist", v.name)
        return
      }
      target := targets[v.name]
      already_run := make([dynamic]string)
      success, run := run_target(v.name, targets, env, &already_run, opts.rerun)
      if success {
        if run && opts.debug do fmt.printfln("Target %v run successfully", v.name)
        else do if len(target.deps) != 0 do fmt.println("Nothing to do")
      } else do fmt.printfln("Target %v failed", v.name)
    case Set:
      for &value in v.values do value = expand_vars(value, env)
      switch len(v.values) {
      case 0: env[expand_vars(v.name, env)] = "true"
      case 1: env[expand_vars(v.name, env)] = v.values[0]
      case: env[expand_vars(v.name, env)] = v.values
      }
    case Unset:
      name := expand_vars(v.name, env)
      if name in env do delete_key(&env, name)
      else do fmt.eprintln("Key was not there in the first place")
    case If:
      if v.jump_end == SENTINEL {
        fmt.eprintln("Unbounded if")
        break
      }
      val := op_to_bool(v.cond, env)
      if v.jump_else != SENTINEL {
        x, ok := &items[v.jump_else].(Else)
        if !ok {
          fmt.eprintln("This is not else")
          break
        }
        x.exec = !val
      }
      if !val do index = v.jump_else != SENTINEL ? v.jump_else : v.jump_end
    case Else:
      if v.jump == SENTINEL {
        fmt.eprintln("Unbounded else")
        break
      }
      if !v.exec do index = v.jump
    case EndIf: {}
    case Fart: fmt.println(expand_vars(v.whisper, env))
    case Loop: if v.cursor >= len(v.elements) do index = v.jump
        else {
          env[expand_vars(v.name, env)] = v.elements[v.cursor]
          v.cursor += 1
        }
    case EndLoop: index = v.jump - 1
    }
  }
}

