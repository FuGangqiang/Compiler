CC=gcc
CFLAGS=-std=c89 -Werror -Wall -I.

ifneq (,$(findstring test,$(MAKECMDGOALS)))
  # test targets
  CFLAGS+=-g -O0 -DFU_TEST
else
  # not test targets
  CFLAGS+=-O2
endif

FU_CC=$(CC) $(CFLAGS)
ROOT_DIR=..
TEST_DIR=$(ROOT_DIR)/tests
TOOL_DIR=$(ROOT_DIR)/tools

alloc.o: alloc.c alloc.h
hash.o: hash.c hash.h
char.o: char.c char.h def.h
bytes.o: bytes.c bytes.h alloc.o
str.o: str.c str.h char.o alloc.o
vec.o: vec.c vec.h def.h alloc.o
map.o: map.c map.h def.h hash.o alloc.o
set.o: set.c set.h def.h hash.o alloc.o
bitset.o: bitset.c bitset.h def.h alloc.o

kind.o: kind.c parse.h keyword.def token.def symbol.def node.def node_arm.def node_expr.def node_ge_arg.def \
    node_ge_param.def node_lit.def node_op.def node_pat.def node_use.def node_variant.def node_vis.def
context.o: context.c parse.h str.o vec.o map.o set.o keyword.def symbol.def node.o
span.o: span.c parse.h str.o error.o context.o
error.o: error.c error.h parse.h span.o
token.o: token.c parse.h kind.o span.o error.o
lexer.o: lexer.c parse.h token.o str.o char.o hash.o set.o span.o error.o
node.o: node.c parse.h alloc.o kind.o
parser.o: parser.c parse.h node.o alloc.o error.o
type.o: type.c parse.h
scope.o: scope.c parse.h

# dep objs

DEP_OBJS=char.o bytes.o str.o hash.o span.o context.o vec.o map.o set.o error.o alloc.o kind.o node.o type.o scope.o

# tools

lexer_dump: $(TOOL_DIR)/lexer_dump.c lexer.o token.o $(DEP_OBJS)
	$(FU_CC) -o $(TOOL_DIR)/$@.bin $^
parser_dump: $(TOOL_DIR)/parser_dump.c parser.o lexer.o token.o $(DEP_OBJS)
	$(FU_CC) -o $(TOOL_DIR)/$@.bin $^

# tests

ifneq (,$(findstring test-mem,$(MAKECMDGOALS)))
  MEM_CHECK_CMD=valgrind --leak-check=full --show-leak-kinds=all --error-exitcode=1
else
  MEM_CHECK_CMD=
endif

test: char_test bytes_test str_test vec_test map_test set_test bitset_test context_test alloc_test \
	  lexer_test parser_test
test-mem: test

char_test: $(TEST_DIR)/char_test.c char.o
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

bytes_test: $(TEST_DIR)/bytes_test.c bytes.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

str_test: $(TEST_DIR)/str_test.c $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

vec_test: $(TEST_DIR)/vec_test.c vec.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

map_test: $(TEST_DIR)/map_test.c map.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

set_test: $(TEST_DIR)/set_test.c set.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

bitset_test: $(TEST_DIR)/bitset_test.c bitset.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

context_test: $(TEST_DIR)/context_test.c context.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

alloc_test: $(TEST_DIR)/alloc_test.c $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

lexer_test: lexer_dump
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(TOOL_DIR)/lexer_dump.bin lexer/literal.fu > lexer/literal.fu.tokens.tmp
	cmp -s $(TEST_DIR)/lexer/literal.fu.tokens  $(TEST_DIR)/lexer/literal.fu.tokens.tmp

	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(TOOL_DIR)/lexer_dump.bin lexer/expr.fu > lexer/expr.fu.tokens.tmp
	cmp -s $(TEST_DIR)/lexer/expr.fu.tokens  $(TEST_DIR)/lexer/expr.fu.tokens.tmp

	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(TOOL_DIR)/lexer_dump.bin parser/point.fu > parser/point.fu.tokens.tmp
	cmp -s $(TEST_DIR)/parser/point.fu.tokens  $(TEST_DIR)/parser/point.fu.tokens.tmp

parser_test: parser_dump
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(TOOL_DIR)/parser_dump.bin parser/tmp.fu > parser/tmp.fu.nodes.tmp
	cmp -s $(TEST_DIR)/parser/tmp.fu.nodes  $(TEST_DIR)/parser/tmp.fu.nodes.tmp

clean:
	rm -rf $(TOOL_DIR)/*.bin
	rm -rf $(TEST_DIR)/*.bin
	rm -rf $(TEST_DIR)/lexer/*.tmp
	rm -rf $(TEST_DIR)/parser/*.tmp
	rm -rf *.o
	rm -rf *.h.gch
	rm -rf vgcore.*
