CC=gcc
CFLAGS=-std=c89 -Werror -Wall -I.

# realpath need it
CFLAGS+=-D_DEFAULT_SOURCE

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
FUC=$(ROOT_DIR)/fuc

alloc.o: alloc.c alloc.h
hash.o: hash.c hash.h
char.o: char.c char.h def.h
bytes.o: bytes.c bytes.h alloc.o
str.o: str.c str.h char.o alloc.o
vec.o: vec.c vec.h def.h alloc.o
map.o: map.c map.h def.h hash.o alloc.o
set.o: set.c set.h def.h hash.o alloc.o
bitset.o: bitset.c bitset.h def.h alloc.o

driver.o: driver.c driver.h alloc.o driver_emit.def
unix.o: unix.c unix.h str.o

kind.o: kind.c parse.h keyword.def token.def symbol.def node.def node_arm.def node_expr.def node_ge_arg.def \
    node_ge_param.def node_lit.def node_op.def node_pat.def node_use.def node_variant.def node_vis.def type_op.def
span.o: span.c parse.h str.o log.o node.o
log.o: log.c log.h parse.h span.o
token.o: token.c parse.h kind.o span.o log.o
lexer.o: lexer.c parse.h token.o str.o char.o hash.o set.o span.o log.o
node.o: node.c parse.h alloc.o kind.o keyword.def symbol.def
parser.o: parser.c parse.h node.o alloc.o log.o unix.o
type.o: type.c parse.h type.def
scope.o: scope.c parse.h

# dep objs

DEP_OBJS=char.o bytes.o str.o hash.o span.o vec.o map.o set.o log.o alloc.o kind.o node.o token.o lexer.o parser.o \
    scope.o type.o unix.o driver.o

fuc: main.c $(DEP_OBJS)
	$(FU_CC) -o $(ROOT_DIR)/fuc $^

# tests

ifneq (,$(findstring test-mem,$(MAKECMDGOALS)))
  MEM_CHECK_CMD=valgrind --leak-check=full --show-leak-kinds=all --error-exitcode=1
else
  MEM_CHECK_CMD=
endif

test: char_test bytes_test str_test vec_test map_test set_test bitset_test alloc_test pkg_test fuc_test
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

alloc_test: $(TEST_DIR)/alloc_test.c alloc.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

pkg_test: $(TEST_DIR)/pkg_test.c node.o $(DEP_OBJS)
	$(FU_CC) -o $(TEST_DIR)/$@.bin $^
	$(MEM_CHECK_CMD) $(TEST_DIR)/$@.bin

fuc_test: fuc
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit raw-tokens --out-dir lexer -O literal.tokens.raw.tmp lexer/literal.fu
	cmp -s $(TEST_DIR)/lexer/literal.tokens.raw  $(TEST_DIR)/lexer/literal.tokens.raw.tmp
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit greed-tokens --out-dir lexer -O literal.tokens.greed.tmp  lexer/literal.fu
	cmp -s $(TEST_DIR)/lexer/literal.tokens.greed  $(TEST_DIR)/lexer/literal.tokens.greed.tmp

	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit raw-tokens --out-dir lexer -O ops_keyword.tokens.raw.tmp lexer/ops_keyword.fu
	cmp -s $(TEST_DIR)/lexer/ops_keyword.tokens.raw $(TEST_DIR)/lexer/ops_keyword.tokens.raw.tmp
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit greed-tokens --out-dir lexer -O ops_keyword.tokens.greed.tmp lexer/ops_keyword.fu
	cmp -s $(TEST_DIR)/lexer/ops_keyword.tokens.greed  $(TEST_DIR)/lexer/ops_keyword.tokens.greed.tmp

	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit raw-tokens --out-dir parser -O point.tokens.raw.tmp parser/point.fu
	cmp -s $(TEST_DIR)/parser/point.tokens.raw  $(TEST_DIR)/parser/point.tokens.raw.tmp
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit parser-tokens --out-dir parser -O point.tokens.parser.tmp parser/point.fu
	cmp -s $(TEST_DIR)/parser/point.tokens.parser $(TEST_DIR)/parser/point.tokens.parser.tmp
	cd $(TEST_DIR); $(MEM_CHECK_CMD) $(FUC) --emit raw-ast --out-dir parser -O point.ast.raw.tmp parser/point.fu
	cmp -s $(TEST_DIR)/parser/point.ast.raw  $(TEST_DIR)/parser/point.ast.raw.tmp

clean:
	rm -rf $(TEST_DIR)/*.bin
	rm -rf $(TEST_DIR)/lexer/*.tmp
	rm -rf $(TEST_DIR)/parser/*.tmp
	rm -rf $(ROOT_DIR)/fuc
	rm -rf *.o
	rm -rf *.h.gch
	rm -rf vgcore.*
