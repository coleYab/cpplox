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

SRC      := main.cpp includes/utils.cpp
OUT      := build/cpplox

BUILD    := build/obj
OBJS     := $(SRC:%.cpp=$(BUILD)/%.o)
DEPS     := $(OBJS:.o=.d)

.PHONY: build run clean release

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

clean:
	rm -rf build

-include $(DEPS)
