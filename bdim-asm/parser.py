from lark import Lark

with open('lang.lark', 'r') as f:
    data = f.read()

parser = Lark(data, parser='lalr')

def parse(fname):
	with open(fname, 'r') as f:
		data = f.read()
	print(parser.parse(data).pretty())

if __name__ == "__main__":
	parse("test.basm")
