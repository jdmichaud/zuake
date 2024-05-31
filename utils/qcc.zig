// Compile QuakeC bytecode.
//
// reference:
//
// cmd: clear && zig build-exe -freference-trace qvm.zig && ./qvm ../data/pak/progs.dat
// test: zig test qcc.zig
const std = @import("std");
const dat = @import("dat");

pub const Token = struct {
  tag: Tag,
  start: usize,
  end: usize,

  pub const Tag = enum {
    eof,
    new_line,

    dot,
    comma,
    semicolon,
    l_paren,
    r_paren,
    l_bracket,
    r_bracket,
    equal,
    double_equal,
    not_equal,
    not,
    plus,
    minus,
    elipsis,
    comment,
    quote,
    double_quote,
    slash,
    and_,
    or_,
    ampersand,
    pipe,
    true,
    false,

    identifier,
    vector_literal,
    string_literal,
    float_literal,
    builtin_literal,
  };
};

pub const Location = struct {
  column: u32,
  row: u32,
};

pub const GenericError = struct {
  message: [255:0]u8 = [_:0]u8{ 0 } ** 255,
  location: Location = .{ .column = 0, .row = 0 },
};

pub fn getLocation(buffer: []const u8, index: usize) Location {
  var i: usize = 0;
  var location = Location{ .column = 0, .row = 0 };
  while (i < index) : (i += 1) {
    location.column += 1;
    if (buffer[i] == '\n') {
      location.column = 0;
      location.row += 1;
    }
  }
  return location;
}

pub const Tokenizer = struct {
  const Self = @This();

  buffer: [:0]const u8,
  index: usize,

  const State = enum {
    start,
    identifier,
    builtin_literal,
    float_literal,
    string_literal,
    vector_literal,
    comment,
  };

  pub fn init(buffer: [:0]const u8) Self {
    return Self {
      .buffer = buffer,
      .index = 0,
    };
  }

  pub fn next(self: *Self, err: *GenericError) !Token {
    var state = State.start;
    var result = Token {
      .tag = .eof,
      .start = self.index,
      .end = undefined,
    };

    while (self.index < self.buffer.len) {
      // std.log.err("character {c} state {}", .{ self.buffer[self.index], state });
      switch (state) {
        .start => switch (self.buffer[self.index]) {
          ' ', '\t', '\n', '\r' => self.index += 1, // ignore whitespace
          '(' => {
            result.tag = .l_paren;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          ')' => {
            result.tag = .r_paren;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          '{' => {
            result.tag = .l_bracket;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          '}' => {
            result.tag = .r_bracket;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          '.' => {
            result.start = self.index;
            if (std.mem.eql(u8, self.buffer[self.index..self.index + 3], "...")) {
              result.tag = .elipsis;
              self.index += 3;
              result.end = self.index;
              return result;
            } else if (self.index + 1 < self.buffer.len and
              std.ascii.isDigit(self.buffer[self.index + 1])) {
              state = .float_literal;
            } else {
              result.tag = .dot;
              self.index += 1;
              result.end = self.index;
              return result;
            }
          },
          '+' => {
            result.tag = .plus;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          '-' => {
            result.tag = .minus;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          ',' => {
            result.tag = .comma;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          ';' => {
            result.tag = .semicolon;
            result.start = self.index;
            self.index += 1;
            result.end = self.index;
            return result;
          },
          '=' => {
            result.start = self.index;
            if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == '=') {
              result.tag = .double_equal;
              self.index += 2;
            } else {
              result.tag = .equal;
              self.index += 1;
            }
            result.end = self.index;
            return result;
          },
          '&' => {
            result.start = self.index;
            if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == '&') {
              result.tag = .and_;
              self.index += 2;
            } else {
              result.tag = .ampersand;
              self.index += 1;
            }
            result.end = self.index;
            return result;
          },
          '|' => {
            result.start = self.index;
            if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == '|') {
              result.tag = .or_;
              self.index += 2;
            } else {
              result.tag = .pipe;
              self.index += 1;
            }
            result.end = self.index;
            return result;
          },
          '!' => {
            result.start = self.index;
            if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == '=') {
              result.tag = .not_equal;
              self.index += 2;
            } else {
              result.tag = .not;
              self.index += 1;
            }
            result.end = self.index;
            return result;
          },
          '"' => {
            state = .string_literal;
            result.start = self.index;
            self.index += 1;
          },
          '\'' => {
            state = .vector_literal;
            result.start = self.index;
            self.index += 1;
          },
          '/' => {
            if (self.index + 1 < self.buffer.len and self.buffer[self.index + 1] == '/') {
              state = .comment;
              result.start = self.index;
              self.index += 1;
            } else {
              result.tag = .slash;
              result.start = self.index;
              self.index += 1;
              result.end = self.index;
              return result;
            }
          },
          '#' => {
            state = .builtin_literal;
            result.start = self.index;
            self.index += 1;
          },
          'a'...'z', 'A'...'Z', '_' => {
            const i = self.index;
            if (self.buffer[i] == 'f' and i + "false".len < self.buffer.len and
              std.mem.eql(u8, self.buffer[i..i + "false".len], "false")) {
              result.tag = .false;
              result.start = self.index;
              self.index += "false".len;
              result.end = self.index;
              return result;
            } else if (self.buffer[i] == 't' and i + "true".len < self.buffer.len and
              std.mem.eql(u8, self.buffer[i..i + "true".len], "true")) {
              result.tag = .true;
              result.start = self.index;
              self.index += "true".len;
              result.end = self.index;
              return result;
            } else {
              state = .identifier;
              result.start = self.index;
            }
          },
          '0'...'9' => {
            state = .float_literal;
            result.start = self.index;
          },
          else => |c| {
            _ = try std.fmt.bufPrintZ(&err.message, "unexpected character: {c}", .{ c });
            err.location = getLocation(self.buffer, self.index);
            return error.UnexpectedCharacter;
          },
        },
        .identifier => switch (self.buffer[self.index]) {
          'a'...'z', 'A'...'Z', '_' => {
            result.tag = .identifier;
            result.end = self.index;
            self.index += 1;
          },
          else => return result,
        },
        .builtin_literal => switch (self.buffer[self.index]) {
          '0'...'9' => {
            result.tag = .builtin_literal;
            result.end = self.index;
            self.index += 1;
          },
          else => return result,
        },
        .float_literal => switch (self.buffer[self.index]) {
          '0'...'9', '.' => {
            result.tag = .float_literal;
            result.end = self.index;
            self.index += 1;
          },
          else => return result,
        },
        .string_literal => switch (self.buffer[self.index]) {
          '"' => {
            result.tag = .string_literal;
            result.end = self.index;
            self.index += 1;
            return result;
          },
          else => {
            result.tag = .string_literal;
            result.end = self.index;
            self.index += 1;
          }
        },
        .vector_literal => switch (self.buffer[self.index]) {
          '\'' => {
            result.tag = .vector_literal;
            result.end = self.index;
            self.index += 1;
            return result;
          },
          '0'...'9', '+', '-', '.', ' ', '\t', '\n', '\r' => {
            result.tag = .vector_literal;
            result.end = self.index;
            self.index += 1;
          },
          else => |c| {
            _ = try std.fmt.bufPrintZ(&err.message, "unexpected character in vector literal: {c}", .{ c });
            err.location = getLocation(self.buffer, self.index);
            return error.UnexpectedCharacter;
          },
        },
        .comment => switch (self.buffer[self.index]) {
          '\n' => {
            result.tag = .comment;
            result.end = self.index;
            self.index += 1;
            return result;
          },
          else => self.index += 1,
        },
      }
    }

    return result;
  }

};

const Ast = struct {
  const NodeType = enum {
    var_decl,
    fn_decl,
    field_decl,
    builtin_decl,
    scope,
  };

  const QType = enum {
    Void,
    Float,
    Vector,
    String,

    const Self = @This();
    pub fn prettyPrint(self: Self, string: []u8) !usize {
      switch (self) {
        .Void => return (try std.fmt.bufPrint(string, "void", .{})).len,
        .Float => return (try std.fmt.bufPrint(string, "float", .{})).len,
        .Vector => return (try std.fmt.bufPrint(string, "vector", .{})).len,
        .String => return (try std.fmt.bufPrint(string, "string", .{})).len,
      }
    }
  };

  const QValue = union(QType) {
    Void: void,
    Float: f32,
    Vector: @Vector(3, f32),
    String: []const u8,

    const Self = @This();
    pub fn prettyPrint(self: Self, string: []u8) !usize {
      switch (self) {
        .Void => return (try std.fmt.bufPrint(string, "void", .{})).len,
        .Float => |f| return (try std.fmt.bufPrint(string, "{d}", .{ f })).len,
        .Vector => |v| {
          return (try std.fmt.bufPrint(string, "'{d} {d} {d}'", .{ v[0], v[1], v[2] })).len;
        },
        .String => |s| return (try std.fmt.bufPrint(string, "{s}", .{ s })).len,
      }
    }
  };

  const VarDecl = struct {
    type: QType,
    name: []const u8,
    value: ?QValue,
  };

  const ParamDecl = struct {
    type: QType,
    name: []const u8,

    const Self = @This();
    pub fn prettyPrint(self: Self, string: []u8) !usize {
      const written = try self.type.prettyPrint(string);
      return written + (try std.fmt.bufPrint(string, " {s}", .{ self.name })).len;
    }
  };

  const FnDecl = struct {
    return_type: QType,
    name: []const u8,
    parameter_list: []const ParamDecl,
    body: *Node,
  };

  const FieldDecl = struct {
    type: QType,
    name: []const u8,
  };

  const BuiltinDecl = struct {
    name: []const u8,
    return_type: QType,
    parameter_list: []const ParamDecl,
    index: usize,
  };

  const Scope = struct {
    instructions: []Node,
  };

  const Node = union(NodeType) {
    var_decl: VarDecl,
    fn_decl: FnDecl,
    field_decl: FieldDecl,
    builtin_decl: BuiltinDecl,
    scope: Scope,

    pub fn prettyPrint(self: @This(), string: []u8) !usize {
      var written: usize = 0;
      switch (self) {
        .var_decl => |d| {
          written += try d.type.prettyPrint(string);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          if (d.value) |value| {
            written += (try std.fmt.bufPrint(string[written..], " = ", .{})).len;
            written += try value.prettyPrint(string[written..]);
          }
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .fn_decl => |d| {
          written += try d.return_type.prettyPrint(string);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          for (d.parameter_list, 0..) |param, i| {
            written += try param.prettyPrint(string[written..]);
            if (i < d.parameter_list.len - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ") {s} = ", .{ d.name })).len;
          written += try d.body.prettyPrint(string[written..]);
        },
        .field_decl => |d| {
          written += try d.type.prettyPrint(string);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          written += (try std.fmt.bufPrint(string[written..], ";\n", .{})).len;
        },
        .builtin_decl => |d| {
          written += try d.return_type.prettyPrint(string);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          written += (try std.fmt.bufPrint(string[written..], ") {s} = #{};", .{ d.name, d.index })).len;
        },
        .scope => |s| {
          written += (try std.fmt.bufPrint(string[written..], "{{\n", .{})).len;
          for (s.instructions) |param| {
            // TODO: indentation here
            written += try param.prettyPrint(string[written..]);
            written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
          }
          written += (try std.fmt.bufPrint(string[written..], "}}\n", .{})).len;
        },
      }
      return written;
    }
  };
};

const ParseError = error {
  EmptySource,
};

fn makeError(errcode: ParseError, err: *GenericError, message: []const u8) ParseError {
  _ = try std.fmt.bufPrint(err.message, message, .{});
  return errcode;
}

const Parser = struct {
  const Self = @This();

  tokenizer: Tokenizer,

  pub fn init(buffer: [:0]const u8) Self {
    return Self {
      .tokenizer = Tokenizer.init(buffer),
    };
  }

  pub fn parse(self: *Self, err: *GenericError) !Ast.Node {
    switch (try self.tokenizer.next(err)) {
      Token.Tag.eof => return makeError(ParseError.EmptySource, err, "Empty source"),
      Token.Tag.new_line,
      Token.Tag.comment => // ignore
      Token.Tag.identifier => // var/fn decl
      Token.Tag.dot => // field decl
    }
  }

  // fn parseRoot(self: *Self, err: *GenericError) !Ast.Node {
  //   return Ast.Node{ .var_decl = Ast.VarDecl{
  //     .type = Ast.QType.String,
  //     .name = "foo",
  //     .value = Ast.QValue{ .String = "something" },
  //   }};
  // }
};

fn testTokenize(source: [:0]const u8, expected_token_tags: []const Token.Tag, err: *GenericError) !void {
  var tokenizer = Tokenizer.init(source);
  for (expected_token_tags) |expected_token_tag| {
      const token = try tokenizer.next(err);
      try std.testing.expectEqual(expected_token_tag, token.tag);
  }
  const last_token = try tokenizer.next(err);
  try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
  try std.testing.expectEqual(source.len, last_token.start);
  try std.testing.expectEqual(source.len, last_token.end);
}

test "tokenizer test" {
  var err = GenericError{};
  try testTokenize("(this is an_identifier)", &.{
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.eof,
  }, &err);
  try testTokenize(".vector field;", &.{
    Token.Tag.dot,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("float f = 3.14;", &.{
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.float_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("string s = \"pi\";", &.{
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.string_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("vector v = '+0.5 -1 -.2';", &.{
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.vector_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("void (float f, vector v) foo = {}", &.{
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.comma,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.l_bracket,
    Token.Tag.r_bracket,
    Token.Tag.eof,
  }, &err);
  try testTokenize("void\t(string str, ...)\tprint = #99;", &.{
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.comma,
    Token.Tag.elipsis,
    Token.Tag.r_paren,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.builtin_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);

  const comments =
    \\ // Let's describe this function
    \\ void () main = {
    \\   string foo = "foo";
    \\   printf(foo); // prints foo
    \\ }
  ;
  try testTokenize(comments, &.{
    Token.Tag.comment,
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.r_paren,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.l_bracket,
    Token.Tag.identifier,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.string_literal,
    Token.Tag.semicolon,
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.semicolon,
    Token.Tag.comment,
    Token.Tag.r_bracket,
    Token.Tag.eof,
  }, &err);

  const conditions =
    \\if ("foo" == "foo" && 1 != 2 || false && true) {
    \\  printf(foo);
    \\}
  ;
  try testTokenize(conditions, &.{
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.string_literal,
    Token.Tag.double_equal,
    Token.Tag.string_literal,
    Token.Tag.and_,
    Token.Tag.float_literal,
    Token.Tag.not_equal,
    Token.Tag.float_literal,
    Token.Tag.or_,
    Token.Tag.false,
    Token.Tag.and_,
    Token.Tag.true,
    Token.Tag.r_paren,
    Token.Tag.l_bracket,
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.semicolon,
    Token.Tag.r_bracket,
    Token.Tag.eof,
  }, &err);
}

fn expectEqualString(lhs: []const u8, rhs: []const u8) !void {
  if (std.mem.eql(u8, lhs, rhs)) {
    return;
  } else {
    std.log.err("\n\"{s}\"\n not equal to\n\"{s}\"", .{ lhs, rhs });
    return error.Fail;
  }
}

fn testParse(source: [:0]const u8, err: *GenericError) !void {
  var output: [4096]u8 = .{ 0 } ** 4096;
  var parser = Parser.init(source);
  const node = try parser.parse(err);
  _ = try node.prettyPrint(&output);
  try expectEqualString(source, &output);
}

test "parser test" {
  var err = GenericError{};
  try testParse("float f = 3.14;", &err);
}