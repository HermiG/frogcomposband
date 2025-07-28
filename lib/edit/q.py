import re
from collections import defaultdict

# Default values for quest fields
DEFAULT_QUEST = {
    'level': None,
    'name': None,
    'flags': set(),
    'where': None,
    'goal': None,
    'file': None
}

# Known field codes
KNOWN_CODES = {'N', 'T', 'W', 'G', 'F'}

def parse_goal(goal_str):
    # Matches patterns like: KILL(thing), FIND(item), KILL(^warg$, 8)
    match = re.match(r'(\w+)\(([^)]+)\)', goal_str)
    if not match:
        return goal_str  # return raw if unrecognized
    action, params = match.groups()
    # Split and clean params
    parts = [p.strip() for p in params.split(',')]
    # Try converting numbers to int
    for i, part in enumerate(parts):
        if part.isdigit():
            parts[i] = int(part)
    return [action.lower()] + parts

def parse_quests(filename):
    quests = {}
    current_id = None
    current_quest = {}

    with open(filename, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()

            # Skip comments and blank lines
            if not line or line.startswith('#'):
                continue

            code = line[0]
            rest = line[2:]

            if code not in KNOWN_CODES:
                print(f"Error (line {line_num}): Unknown code '{code}' in line: {line}")
                return

            if code == 'N':
                if current_id is not None:
                    quests[current_id] = current_quest

                parts = rest.split(':', 2)
                if len(parts) != 3:
                    print(f"Warning (line {line_num}): Bad N line format: {line}")
                    continue
                current_id = int(parts[0])
                level = int(parts[1])
                name = parts[2].strip()

                current_quest = DEFAULT_QUEST.copy()
                current_quest['level'] = level
                current_quest['name'] = name
                current_quest['flags'] = set()
                current_quest['where'] = None
                current_quest['goal'] = None
                current_quest['file'] = None

            elif code == 'T':
                flags = {flag.strip() for flag in rest.split('|')}
                current_quest['flags'].update(flags)
                
                if 'GENERATE' in flags:
                  if current_quest['where']:
                    print(f"Error (line {line_num}): inconsistent location in line: {line}")
                    return
                  else:
                    current_quest['where'] = '<generated>'

            elif code == 'W':
                if current_quest['where']:
                    print(f"Error (line {line_num}): inconsistent location in line: {line}")
                    return
                else:
                  current_quest['where'] = rest.strip()

            elif code == 'G':
                current_quest['goal'] = parse_goal(rest)

            elif code == 'F':
                current_quest['file'] = rest.strip()

    # Add the quest
    if current_id is not None:
        quests[current_id] = current_quest

    return quests

# Example usage
if __name__ == '__main__':
    quest_file = 'q_info.txt'
    quest_data = parse_quests(quest_file)
    
    print()
    print()
    unassigned_qids = sorted(set(range(1,100)) - set(quest_data.keys()))
    print(f"{unassigned_qids = }")
    print()
    
    del quest_data[82] # placeholder quest

    # Simple query example
    for qid, q in quest_data.items():
        #if qid < 40 and q['level'] < 25:
        if 'TOWN' not in q['flags'] and 'RANDOM' not in q['flags']:
          print(f"  Q[{qid:3d}] {q['name']} (lvl {q['level']}) -> {q['goal']}")
          print(q)
          
        assert(q['where'])
        assert((q['where'] == '<generated>') == ('GENERATE' in q['flags']))
        
        
