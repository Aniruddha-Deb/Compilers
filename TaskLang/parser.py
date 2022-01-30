from lark import Lark
from lark.indenter import Indenter

class TreeIndenter(Indenter):
	NL_type = '_NL'
	OPEN_PAREN_types = []
	CLOSE_PAREN_types = []
	INDENT_type = '_INDENT'
	DEDENT_type = '_DEDENT'
	tab_len = 2

tree_grammar = r"""
	start: _NL* date_task_list+
	date_task_list: DATE _NL section+ 
	section: SENTENCE _NL [_INDENT task+ _DEDENT] 
	task: STATUS [priority] SENTENCE [taglist] _NL [_INDENT task+ _DEDENT]
	taglist: tag+
	tag: "#"CNAME
	priority: PRIORITY
	
	DATE: /@[0-9]{4}-(0[1-9]|10|11|12)-[0-3][0-9]/
	PRIORITY: "!"~1..3
	STATUS: ("-"|"X")
	_NL: /(\r?\n[\t ]*)+/
	SENTENCE: /[A-Za-z0-9][^#\r\n]+/
	%import common.WS_INLINE
	%import common.CNAME
	%ignore WS_INLINE
	%declare _INDENT _DEDENT
"""

l = Lark(tree_grammar, parser="lalr", postlex=TreeIndenter())

test_str = """
@2022-01-30
Operations
  - Cut nails, toenails #working #multipletags #blessed
    - ! Multi level task support #important
    - !! This task is supposed to be nested fml #today
    - More tasks here
COL226
  - Read parsing
    - nested?
   	X nested 2
      X arbitrary
        X nesting
ELL205
  - ! Catch up to lectures
COL216
  - !!! See lecs about diff instruction sets
  - Finish merge sort code
CVL100
  - Numericals, other lectures
HUL242
  - Finish lec6,7,8

"""

def test():
	print(l.parse(test_str).pretty())
	
if __name__ == "__main__":
	test()

