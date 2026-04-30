CXX      := g++
CXXFLAGS := \
	-std=c++23 \
	-Wall -Wextra -Wpedantic -Werror \
	-Wshadow -Wconversion -Wsign-conversion \
	-Wnull-dereference -Wdouble-promotion -Wformat=2 \
	-Wimplicit-fallthrough -Wold-style-cast \
	-Woverloaded-virtual -Wnon-virtual-dtor -Wcast-align \
	-Wduplicated-cond -Wduplicated-branches -Wlogical-op \
	-Wuseless-cast \
	-fno-omit-frame-pointer -g3 \
	-I includes

SANITIZERS := \
	-fsanitize=address \
	-fsanitize=undefined \
	-fsanitize=leak

LDFLAGS := $(SANITIZERS)

SRC      := src/main.cpp src/interpreter.cpp src/resolver.cpp src/object.cpp includes/utils.cpp
OUT      := build/cpplox

BUILD    := build/obj
OBJS     := $(SRC:%.cpp=$(BUILD)/%.o)
DEPS     := $(OBJS:.o=.d)

TEST_SRC := src/interpreter.cpp src/resolver.cpp src/object.cpp includes/utils.cpp
TEST_OBJ := $(TEST_SRC:%.cpp=$(BUILD)/%.o)

TEST_DIR    := tests/cpp
TEST_FILES  := $(wildcard $(TEST_DIR)/test_*.cpp)
TEST_TARGET := build/test_runner

.PHONY: build run clean release test

build: $(OUT)

$(OUT): $(OBJS)
	mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

$(BUILD)/%.o: %.cpp
	mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -MMD -MP -c $< -o $@

run: build
	./$(OUT)

release: CXXFLAGS := -std=c++23 -O3 -march=native -flto -DNDEBUG -I includes
release: LDFLAGS  :=
release: $(OUT)

test: $(TEST_TARGET)
	./$(TEST_TARGET)

$(TEST_TARGET): $(TEST_OBJ) $(TEST_FILES)
	mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) $(TEST_FILES) $(TEST_OBJ) -o $@ -lgtest -lgtest_main -lpthread

clean:
	rm -rf build

-include $(DEPS)
