// Expression parser based on the Shunting Yard algorithm.
//
// reference:
// https://en.wikipedia.org/wiki/Shunting_yard_algorithm
//
// cmd: clear && zig build-exe -freference-trace shuntingyard.zig && ./shuntingyard "1+2"
// test: clear && zig test -freference-trace shuntingyard.zig

const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

// Statically define a map from KeyT to ValueT:
// const map = StaticMap(u8, u8).init(&.{
//   .{ 1, 2 },
//   .{ 3, 4 },
//   .{ 5, 6 },
// });
// If you need KeyT to be a string, use StaticStringMap
fn StaticMap(comptime KeyT: type, comptime ValueT: type) type {
  return struct {
    pub inline fn init(comptime map: []const struct { KeyT, ValueT }) fn(key: KeyT) ?ValueT {
      const _inner = struct {
        pub fn get(key: KeyT) ?ValueT {
          comptime var i: comptime_int = 0;
          inline while (true) : (i += 1) {
            if (i >= map.len) {
              return null;
            }
            if (key == map[i][0]) {
              return map[i][1];
            }
          }
        }
      };

      return _inner.get;
    }
  };
}

test "StaticMap" {
  const testing = std.testing;

  const map = StaticMap(u8, u8).init(&.{
    .{ 1, 2 },
    .{ 3, 4 },
    .{ 5, 6 },
  });
  try testing.expectEqual(map(1).?, 2);
  try testing.expectEqual(map(5).?, 6);
  try testing.expectEqual(map(7), null);
}

// A list on which you can only add, Returns the index on addition.
// Could be used as a storage for a tree.
// ex:
// ```zig
// var storage: [5]u8 = undefined;
// var list = AddOnlyList(u8).init(&storage);
// ```
fn AddOnlyList(comptime T: type) type {
  return struct {
    const Self = @This();

    list: []T,
    head: usize,

    pub fn init(list: []T) Self {
      return Self{
        .list = list,
        .head = 0,
      };
    }

    pub fn add(self: *Self, t: T) !usize {
      if (self.head < self.list.len) {
        self.list[self.head] = t;
        self.head += 1;
        return self.head - 1;
      } else {
        return error.ListFull;
      }
    }

    pub fn len(self: Self) usize {
      return self.head;
    }
  };
}

test "AddOnlyList" {
  const testing = std.testing;

  var storage: [5]u8 = undefined;
  var list = AddOnlyList(u8).init(&storage);

  try testing.expectEqual(try list.add(1), 0);
  try testing.expectEqual(try list.add(7), 1);
  try testing.expectEqual(list.len(), 2);
}

// A stack backed by external storage.
// ex:
// ```zig
// var storage: [5]u8 = undefined;
// var stack = Stack(u8).init(&storage);
// try stack.push(34);
// stack.pop();
// ```
fn Stack(comptime T: type) type {
  return struct {
    const Self = @This();

    list: []T,
    head: usize = 0,

    pub fn init(list: []T) Self {
      return Self{
        .list = list,
        .head = 0,
      };
    }

    pub fn push(self: *Self, t: T) !void {
      if (self.head >= self.list.len) {
        return error.StackOverflow;
      }
      self.list[self.head] = t;
      self.head += 1;
      // return self.head - 1;
    }

    pub fn peek(self: Self) ?T {
      if (self.head <= 0) {
        return null;
      }
      return self.list[self.head - 1];
    }

    pub fn pop(self: *Self) ?T {
      if (self.head <= 0) {
        return null;
      }
      self.head -= 1;
      return self.list[self.head];
    }

    pub fn len(self: Self) usize {
      return self.head;
    }

    pub fn toSlice(self: Self) []T {
      return self.list[0..self.head];
    }
  };
}

test "Stack" {
  const testing = std.testing;

  var storage: [5]u8 = undefined;
  var stack = Stack(u8).init(&storage);

  try stack.push(1);
  try stack.push(7);
  try testing.expectEqual(stack.len(), 2);
  try testing.expectEqual(stack.toSlice()[1], 7);
  try testing.expectEqual(stack.pop().?, 7);
  try testing.expectEqual(stack.pop().?, 1);
  try testing.expectEqual(stack.len(), 0);
  try testing.expectEqual(stack.pop(), null);
}

pub const Tokenizer = struct {
  const Self = @This();

  buffer: [:0]const u8,
  index: usize,

  pub const Token = struct {
    tag: Tag,
    start: usize,
    end: usize,

    fn getValue(self: @This(), buffer: []const u8) []const u8 {
      return buffer[self.start..self.end + 1];
    }

    pub const Tag = enum {
      eof,
      l_paren,
      r_paren,
      add_op,
      sub_op,
      mul_op,
      div_op,
      pow_op,
      add_unop,
      sub_unop,
      float_literal,
    };
  };

  pub fn init(buffer: [:0]const u8) Self {
    return Tokenizer{
      .buffer = buffer,
      .index = 0,
    };
  }

  pub fn next(self: *Self) !Token {
    const token = try self.peek();
    self.index = token.end + 1;
    return token;
  }

  pub fn peek(self: *Self) !Token {
    const State = enum { start, float_literal };

    var state = State.start;
    var result = Token {
      .tag = .eof,
      .start = self.index,
      .end = self.index,
    };

    if (self.index > self.buffer.len) {
      result.start -= 1;
      result.end -= 1;
      return result;
    }

    var index: usize = self.index;
    while (index < self.buffer.len) : (index += 1) {
      const c = self.buffer[index];
      switch (state) {
        .start => switch (c) {
          ' ', '\t', '\n', '\r' => {}, // ignore whitespace
          '(' => {
            result.tag = .l_paren;
            result.start = index;
            result.end = index;
            return result;
          },
          ')' => {
            result.tag = .r_paren;
            result.start = index;
            result.end = index;
            return result;
          },
          '+' => {
            result.tag = .add_op;
            result.start = index;
            result.end = index;
            return result;
          },
          '-' => {
            result.tag = .sub_op;
            result.start = index;
            result.end = index;
            return result;
          },
          '*' => {
            result.tag = .mul_op;
            result.start = index;
            result.end = index;
            return result;
          },
          '/' => {
            result.tag = .div_op;
            result.start = index;
            result.end = index;
            return result;
          },
          '^' => {
            result.tag = .pow_op;
            result.start = index;
            result.end = index;
            return result;
          },
          '0'...'9' => {
            state = State.float_literal;
            result.tag = .float_literal;
            result.start = index;
            result.end = index;
          },
          else => {
            try stderr.print("error: unexpected character {} ({c}) at position {}\n", .{ c, c, index });
            return error.UnexpectedCharacter;
          },
        },
        .float_literal => switch (c) {
          '0'...'9', '.' => {
            result.end = index;
          },
          else => {
            return result;
          }
        },
      }
    }
    return result;
  }
};

const operator_precedence = StaticMap(Tokenizer.Token.Tag, u8).init(&.{
  .{ Tokenizer.Token.Tag.add_op, 1 },
  .{ Tokenizer.Token.Tag.sub_op, 1 },
  .{ Tokenizer.Token.Tag.mul_op, 2 },
  .{ Tokenizer.Token.Tag.div_op, 2 },
  .{ Tokenizer.Token.Tag.pow_op, 3 },
  .{ Tokenizer.Token.Tag.add_unop, 3 },
  .{ Tokenizer.Token.Tag.sub_unop, 3 },
});

const Associativity = enum(u1) {
  LEFT = 0,
  RIGHT = 1,
};

const operator_associativity = StaticMap(Tokenizer.Token.Tag, Associativity).init(&.{
  .{ Tokenizer.Token.Tag.add_op, Associativity.LEFT  },
  .{ Tokenizer.Token.Tag.sub_op, Associativity.LEFT  },
  .{ Tokenizer.Token.Tag.mul_op, Associativity.LEFT  },
  .{ Tokenizer.Token.Tag.div_op, Associativity.LEFT  },
  .{ Tokenizer.Token.Tag.pow_op, Associativity.RIGHT },
  .{ Tokenizer.Token.Tag.add_unop, Associativity.RIGHT },
  .{ Tokenizer.Token.Tag.sub_unop, Associativity.RIGHT },
});

fn shuntingyard(expr: [:0]const u8, input_stack: *Stack(Tokenizer.Token),
  output_stack: *Stack(Tokenizer.Token)) !void {
  var tokenizer = Tokenizer.init(expr);
  var prev: ?Tokenizer.Token = null;
  while (true) {
    var token = try tokenizer.next();
    // Handle unray operator here
    // + or - are unary operator if
    // The token is at the very start of the input
    // The token is after a ( token
    // The token is after a binary operator token such as +
    // The token is after after another - token
    if (token.tag == Tokenizer.Token.Tag.add_op or token.tag == Tokenizer.Token.Tag.sub_op) {
      var is_unop = prev == null;
      if (!is_unop) {
        switch (prev.?.tag) {
          Tokenizer.Token.Tag.add_op,
          Tokenizer.Token.Tag.sub_op,
          Tokenizer.Token.Tag.mul_op,
          Tokenizer.Token.Tag.div_op,
          Tokenizer.Token.Tag.pow_op,
          Tokenizer.Token.Tag.add_unop,
          Tokenizer.Token.Tag.sub_unop => is_unop = true,
          else => {},
        }
      }
      if (is_unop) {
        if (token.tag == Tokenizer.Token.Tag.add_op)
          token.tag = Tokenizer.Token.Tag.add_unop
        else token.tag = Tokenizer.Token.Tag.sub_unop;
      }
    }

    switch (token.tag) {
      Tokenizer.Token.Tag.add_op,
      Tokenizer.Token.Tag.sub_op,
      Tokenizer.Token.Tag.mul_op,
      Tokenizer.Token.Tag.div_op,
      Tokenizer.Token.Tag.pow_op,
      Tokenizer.Token.Tag.add_unop,
      Tokenizer.Token.Tag.sub_unop => {
        const precedence = operator_precedence(token.tag).?;
        if (input_stack.len() > 0) {
          const previous_operator = input_stack.peek().?;
          switch (previous_operator.tag) {
            Tokenizer.Token.Tag.add_op,
            Tokenizer.Token.Tag.sub_op,
            Tokenizer.Token.Tag.mul_op,
            Tokenizer.Token.Tag.div_op,
            Tokenizer.Token.Tag.pow_op,
            Tokenizer.Token.Tag.add_unop,
            Tokenizer.Token.Tag.sub_unop => {
              // If the operator's precedence is lower than that of the operators at the
              // top of the stack or the precedences are equal and the operator is left
              // associative, then that operator is popped off the stack and added to
              // the output
              const previous_operator_precedence = operator_precedence(previous_operator.tag).?;
              if (precedence < previous_operator_precedence or
                (precedence == previous_operator_precedence and
                  operator_associativity(token.tag) == Associativity.LEFT)) {
                try output_stack.push(input_stack.pop().?);
                try input_stack.push(token);
              } else {
                try input_stack.push(token);
              }
            },
            else => try input_stack.push(token),
          }
        } else {
          try input_stack.push(token);
        }
      },
      Tokenizer.Token.Tag.float_literal => {
        // It's not an operator, push it on the stack
        try output_stack.push(token);
      },
      Tokenizer.Token.Tag.l_paren => {
        try input_stack.push(token);
      },
      Tokenizer.Token.Tag.r_paren => {
        var popToken = input_stack.pop();
        while (true) {
          if (popToken == null) {
            return error.UnmatchedParenthesis;
          } else if (popToken.?.tag != Tokenizer.Token.Tag.l_paren) {
            try output_stack.push(popToken.?);
            popToken = input_stack.pop();
          } else {
            break;
          }
        }
      },
      Tokenizer.Token.Tag.eof => break,
    }
    prev = token;
  }
  var token = input_stack.pop();
  while (token != null) {
    try output_stack.push(token.?);
    token = input_stack.pop();
  }
}

const NodeType = enum {
  operand,
  binary_operator,
  unary_operator,
};

const OperandNode = struct {
  value: []const u8,
};

const BinaryOperatorNode = struct {
  operator: Tokenizer.Token.Tag,
  lhs: *Node,
  rhs: *Node,
};

const UnaryOperatorNode = struct {
  operator: Tokenizer.Token.Tag,
  rhs: *Node,
};

const Node = union(NodeType) {
  operand: OperandNode,
  binary_operator: BinaryOperatorNode,
  unary_operator: UnaryOperatorNode,
};

fn makeTree(expression: []const u8, rpn_stack: *Stack(Tokenizer.Token), list: *AddOnlyList(Node)) !*Node {
  while (rpn_stack.pop()) |head| {
    switch (head.tag) {
      Tokenizer.Token.Tag.add_op,
      Tokenizer.Token.Tag.sub_op,
      Tokenizer.Token.Tag.mul_op,
      Tokenizer.Token.Tag.div_op,
      Tokenizer.Token.Tag.pow_op => {
        const index = try list.add(Node{ .binary_operator = BinaryOperatorNode{
          .operator = head.tag,
          .rhs = try makeTree(expression, rpn_stack, list),
          .lhs = try makeTree(expression, rpn_stack, list),
        }});
        return &list.list[index];
      },
      Tokenizer.Token.Tag.add_unop,
      Tokenizer.Token.Tag.sub_unop => {
        const index = try list.add(Node{ .unary_operator = UnaryOperatorNode{
          .operator = head.tag,
          .rhs = try makeTree(expression, rpn_stack, list),
        }});
        return &list.list[index];
      },
      Tokenizer.Token.Tag.float_literal => {
        const index = try list.add(Node{ .operand = OperandNode{
          .value = head.getValue(expression),
        }});
        return &list.list[index];
      },
      else => return error.UnexpectedToken,
    }
  }
  return error.UnexpectedEmptyStack;
}

fn prettyPrint(node: Node, writer: anytype) !usize {
  var written: usize = 0;
  switch (node) {
    .binary_operator => |binop| {
      try writer.writeAll("(");
      written += 1;
      written += try prettyPrint(binop.lhs.*, writer);
      switch (binop.operator) {
        .add_op => try writer.writeAll("+"),
        .sub_op => try writer.writeAll("-"),
        .mul_op => try writer.writeAll("*"),
        .div_op => try writer.writeAll("/"),
        .pow_op => try writer.writeAll("^"),
        else => unreachable,
      }
      written += 1;
      written += try prettyPrint(binop.rhs.*, writer);
      try writer.writeAll(")");
      written += 1;
    },
    .operand => |op| {
      try writer.writeAll(op.value);
      written += op.value.len;
    },
    .unary_operator => |unop| {
      try writer.writeAll("(");
      written += 1;
      switch (unop.operator) {
        .add_unop => try writer.writeAll("+"),
        .sub_unop => try writer.writeAll("-"),
        else => unreachable,
      }
      written += 1;
      written += try prettyPrint(unop.rhs.*, writer);
      try writer.writeAll(")");
      written += 1;
    },
  }
  return written;
}

pub fn parse(comptime N: comptime_int, nodelist: []Node, expression: [:0]const u8) !*Node {
  var root: *Node = undefined;
  {
    var output: [N]Tokenizer.Token = undefined;
    var output_stack = Stack(Tokenizer.Token).init(&output);
    {
      var input: [N]Tokenizer.Token = undefined;
      var input_stack = Stack(Tokenizer.Token).init(&input);
      try shuntingyard(expression, &input_stack, &output_stack);
    }
    var list = AddOnlyList(Node).init(nodelist);
    root = try makeTree(expression, &output_stack, &list);
  }
  return root;
}

pub fn evaluate(comptime T: type, node: Node) !T {
  switch (node) {
    .binary_operator => |binop| {
      const lhs = try evaluate(T, binop.lhs.*);
      const rhs = try evaluate(T, binop.rhs.*);
      return switch (binop.operator) {
        .add_op => lhs + rhs,
        .sub_op => lhs - rhs,
        .mul_op => lhs * rhs,
        .div_op => lhs / rhs,
        .pow_op => std.math.pow(T, lhs, rhs),
        else => unreachable,
      };
    },
    .operand => |op| {
      return std.fmt.parseFloat(T, op.value);
    },
    .unary_operator => |unop| {
      switch (unop.operator) {
        .add_unop => return try evaluate(T, unop.rhs.*),
        .sub_unop => return -(try evaluate(T, unop.rhs.*)),
        else => unreachable,
      }
    },
  }
}

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 2 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a expression parser\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <expression> - parse given expression\n", .{ args[0] });
    return;
  }

  var nodelist: [1024]Node = undefined;
  const tree = try parse(1024, &nodelist, args[1]);
  const value = try evaluate(f64, tree.*);
  try stdout.print("{d}\n", .{ value });
}

fn testTokenize(source: [:0]const u8, expected_token_tags: []const Tokenizer.Token.Tag) !void {
  var tokenizer = Tokenizer.init(source);
  for (expected_token_tags) |expected_token_tag| {
      const token = try tokenizer.next();
      try std.testing.expectEqual(expected_token_tag, token.tag);
  }
  const last_token = try tokenizer.next();
  try std.testing.expectEqual(Tokenizer.Token.Tag.eof, last_token.tag);
  try std.testing.expectEqual(source.len, last_token.start);
  try std.testing.expectEqual(source.len, last_token.end);
}

test "getNextToken" {
  try testTokenize(" 12 ", &.{
    Tokenizer.Token.Tag.float_literal,
    Tokenizer.Token.Tag.eof,
  });
  try testTokenize("3.14+32", &.{
    Tokenizer.Token.Tag.float_literal,
    Tokenizer.Token.Tag.add_op,
    Tokenizer.Token.Tag.float_literal,
    Tokenizer.Token.Tag.eof,
  });
  try testTokenize("+32", &.{
    Tokenizer.Token.Tag.add_op,
    Tokenizer.Token.Tag.float_literal,
    Tokenizer.Token.Tag.eof,
  });
  try testTokenize("", &.{ Tokenizer.Token.Tag.eof });
}

fn checkOutputStack(expression: [:0]const u8, expected_tokens: []const []const u8) !void {
  var input: [256]Tokenizer.Token = undefined;
  var input_stack = Stack(Tokenizer.Token).init(&input);

  var output: [256]Tokenizer.Token = undefined;
  var output_stack = Stack(Tokenizer.Token).init(&output);

  try shuntingyard(expression, &input_stack, &output_stack);

  const output_slice = output_stack.toSlice();
  for (expected_tokens, 0..) |expected_token, i| {
    try std.testing.expect(std.mem.eql(u8, expected_token, output_slice[i].getValue(expression)));
  }
  if (expected_tokens.len < output_slice.len) {
    return error.RemainingItemInStack;
  }
}

test "shuntingyard" {
  try checkOutputStack("1+2", &.{ "1", "2", "+"});
  try checkOutputStack("        1 +2         *    3     ", &.{ "1", "2", "3", "*", "+" });
  try checkOutputStack("1+2+3", &.{ "1", "2", "+", "3", "+" });
  try checkOutputStack("1+(2+3)", &.{ "1", "2", "3", "+", "+" });
  try checkOutputStack("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3", &.{
    "3", "4", "2", "*", "1", "5", "-", "2", "3", "^", "^", "/", "+",
  });
}

fn testParse(expression: [:0]const u8, expected: []const u8) !void {
  const expect = std.testing.expect;
  const eql = std.mem.eql;

  var nodelist: [256]Node = undefined;
  const tree = try parse(256, &nodelist, expression);

  var output: [512]u8 = undefined;
  var fbs = std.io.fixedBufferStream(&output);
  const writer = fbs.writer();
  const written = try prettyPrint(tree.*, writer);
  try expect(eql(u8, output[0..written], expected));
}

test "parse" {
  try testParse("1+2", "(1+2)");
  try testParse("        1 +2         *    3     ", "(1+(2*3))");
  try testParse("1+2+3", "((1+2)+3)");
  try testParse("1+(2+3)", "(1+(2+3))");
  try testParse("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3", "(3+((4*2)/((1-5)^(2^3))))");
  try testParse("(1+2)^-(2+5*-(2+4))", "((1+2)^(-(2+(5*(-(2+4))))))");
}

fn testEvaluate(comptime T: type, expression: [:0]const u8, expected: T) !void {
  const expectEqual = std.testing.expectEqual;

  var nodelist: [256]Node = undefined;
  const tree = try parse(256, &nodelist, expression);
  try expectEqual(try evaluate(T, tree.*), expected);
}

test "evaluate" {
  try testEvaluate(f32, "1+2", 3);
  try testEvaluate(f32, "        1 +2         *    3     ", 7);
  try testEvaluate(f32, "1+2+3", 6);
  try testEvaluate(f32, "1+(2+3)", 6);
  try testEvaluate(f64, "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3", 3.0001220703125);
  try testEvaluate(f64, "(1+2)^-(2+5*-(2+4))", 22876792454961);
}
