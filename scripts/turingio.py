#! /opt/homebrew/bin/python3

import re
import json
from pathlib import Path
from typing import Any, Literal

import yaml

Action = dict[str, str]
Match = dict[str, Action]
Table = dict[str, Match]
Dir = Literal["L", "R"]

def lr(d: Dir):
    match d:
        case "L":
            return "LEFT"
        case "R":
            return "RIGHT"


def lift_match(m: Match, state: str):
    def out(r: str, to: str | None, d: Literal["LEFT", "RIGHT"], w: str | None = None):
        return {"read": r, "to_state": to or state, "write": w or r, "action": d}

    def lift_kv(k: str, v: Dir | Action):
        match v:
            case "L" | "R" as d:
                return out(k, None, lr(d))

            case {"write": w, "L": to}:
                return out(k, to, "LEFT", w)
            case {"write": w, "R": to}:
                return out(k, to, "RIGHT", w)

            case {"L": to}:
                return out(k, to, "LEFT")
            case {"R": to}:
                return out(k, to, "RIGHT")


            case _:
                raise ValueError(f"Invalid action: {v}")

    return [lift_kv(k, v) for k, v in m.items()]


def gen(data: Any, name: str):
    blank: str = data["blank"]
    input_: str = data["input"]
    start_state: str = data["start state"]
    table: Table = data["table"]

    res = {
        "name": name,
        "alphabet": list(set(input_ + blank)),
        "blank": blank,
        "states": list(table.keys()),
        "initial": start_state,
        "finals": [state for state, match in table.items() if not match],
        "transitions": {s: lift_match(m, s) for s, m in table.items() if m},
    }
    return res


text = """
input: '11101'
blank: '.'
start state: check_move_right
table:
  check_move_right:
    0: {write: z, R: check_move_right_expect_zero}
    1: {write: o, R: check_move_right_expect_one}
    z: {L: move_to_start_accept}
    o: {L: move_to_start_accept}
  move_left:
    0: L
    1: L
    z: {write: z, R: check_move_right}
    o: {write: o, R: check_move_right}
    .: {write: ., R: check_move_right}
  check_move_right_expect_zero:
    0: R
    1: R
    z: {L: check_is_zero}
    o: {L: check_is_zero}
    .: {L: check_is_zero}
  check_move_right_expect_one:
    0: R
    1: R
    z: {L: check_is_one}
    o: {L: check_is_one}
    .: {L: check_is_one}
  check_is_zero:
    0: {write: z, L: move_left}
    z: {write: z, L: move_to_start_accept}
    o: {write: o, L: move_to_start_accept}
    1: {L: move_to_start_reject}
  check_is_one:
    1: {write: o, L: move_left}
    z: {write: z, L: move_to_start_accept}
    o: {write: o, L: move_to_start_accept}
    0: {L: move_to_start_reject}
  move_to_start_accept:
    1: L
    0: L
    z: L
    o: L
    .: {R: restore_value_accept}
  move_to_start_reject:
    1: L
    0: L
    z: L
    o: L
    .: {R: restore_value_reject}
  restore_value_accept:
    1: R
    0: R
    z: {write: 0, R: restore_value_accept}
    o: {write: 1, R: restore_value_accept}
    .: {write: y, R: accept}
  restore_value_reject:
    1: R
    0: R
    z: {write: 0, R: restore_value_reject}
    o: {write: 1, R: restore_value_reject}
    .: {write: n, R: reject}
  reject:
  accept:
"""

if __name__ == "__main__":
    from sys import argv

    name = argv[2] if len(argv) == 3 else "program"
    if len(argv) >= 2:
        text = Path(argv[1]).read_text()

    array_as_key = re.findall(r'\s*[^#].*\[.*\]: .*', text)
    replace_map = []
    for key in array_as_key:
      space, arr, value = re.search(r'^(\s*)(\[.*\]): (.*)', key).groups()
      arr_obj = eval(arr)
      arr = ''.join([*map(lambda x: f"{space}'{x}': {value}", arr_obj)])
      replace_map.append((key, arr))

    for k, v in replace_map:
      text = text.replace(k, v)

    data = yaml.load(text, Loader=yaml.BaseLoader)  # type: ignore
    res = json.dumps(gen(data, name), indent=2)
    print(res)