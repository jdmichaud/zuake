// Compile QuakeC bytecode.
//
// reference:
// https://github.com/vkazanov/tree-sitter-quakec/blob/main/grammar.js
//
// cmd: clear && zig build-exe -freference-trace qvm.zig && ./qvm ../data/pak/progs.dat
// test: clear && zig test qcc.zig
const std = @import("std");
const dat = @import("dat");

pub const Token = struct {
  tag: Tag,
  start: usize,
  end: usize,

  pub const Tag = enum {
    eof,
    // new_line,

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
    type,
    vector_literal,
    string_literal,
    float_literal,
    builtin_literal,

    kw_if,
    kw_else,
    kw_while,
    kw_do,
    kw_local,
    kw_return,
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
    const token = try self.peek(err);
    self.index = token.end + 1;
    std.log.warn("next {}", .{ token });
    return token;
  }

  pub fn peek(self: *Self, err: *GenericError) !Token {
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

    var index = self.index;
    while (index < self.buffer.len) {
      // std.log.err("character {c} state {}", .{ self.buffer[index], state });
      switch (state) {
        .start => switch (self.buffer[index]) {
          ' ', '\t', '\n', '\r' => index += 1, // ignore whitespace
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
          '{' => {
            result.tag = .l_bracket;
            result.start = index;
            result.end = index;
            return result;
          },
          '}' => {
            result.tag = .r_bracket;
            result.start = index;
            result.end = index;
            return result;
          },
          '.' => {
            result.start = index;
            if (std.mem.eql(u8, self.buffer[index..index + 3], "...")) {
              result.tag = .elipsis;
              result.end = index + 2;
              return result;
            } else if (index + 1 < self.buffer.len and
              std.ascii.isDigit(self.buffer[index + 1])) {
              state = .float_literal;
            } else {
              result.tag = .dot;
              result.end = index;
              return result;
            }
          },
          '+' => {
            result.tag = .plus;
            result.start = index;
            result.end = index;
            return result;
          },
          '-' => {
            result.tag = .minus;
            result.start = index;
            result.end = index;
            return result;
          },
          ',' => {
            result.tag = .comma;
            result.start = index;
            result.end = index;
            return result;
          },
          ';' => {
            result.tag = .semicolon;
            result.start = index;
            result.end = index;
            return result;
          },
          '=' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .double_equal;
              result.end = index + 1;
            } else {
              result.tag = .equal;
              result.end = index;
            }
            return result;
          },
          '&' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '&') {
              result.tag = .and_;
              result.end = index + 1;
            } else {
              result.tag = .ampersand;
              result.end = index;
            }
            return result;
          },
          '|' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '|') {
              result.tag = .or_;
              result.end = index + 1;
            } else {
              result.tag = .pipe;
              result.end = index;
            }
            return result;
          },
          '!' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .not_equal;
              result.end = index + 1;
            } else {
              result.tag = .not;
              result.end = index;
            }
            return result;
          },
          '"' => {
            state = .string_literal;
            result.start = index;
            index += 1;
          },
          '\'' => {
            state = .vector_literal;
            result.start = index;
            index += 1;
          },
          '/' => {
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '/') {
              state = .comment;
              result.start = index;
              index += 1;
            } else {
              result.tag = .slash;
              result.start = index;
              result.end = index;
              return result;
            }
          },
          '#' => {
            state = .builtin_literal;
            result.start = index;
            index += 1;
          },
          'a'...'z', 'A'...'Z', '_' => {
            const i = index;
            if (self.buffer[i] == 'f' and i + "false".len < self.buffer.len and
              std.mem.eql(u8, self.buffer[i..i + "false".len], "false")) {
              result.tag = .false;
              result.start = index;
              result.end = index + "false".len - 1;
              return result;
            } else if (self.buffer[i] == 't' and i + "true".len < self.buffer.len and
              std.mem.eql(u8, self.buffer[i..i + "true".len], "true")) {
              result.tag = .true;
              result.start = index;
              result.end = index + "true".len - 1;
              return result;
            } else {
              state = .identifier;
              result.start = index;
            }
          },
          '0'...'9' => {
            state = .float_literal;
            result.start = index;
          },
          else => |c| {
            _ = try std.fmt.bufPrintZ(&err.message, "unexpected character: {c}", .{ c });
            err.location = getLocation(self.buffer, index);
            return error.UnexpectedCharacter;
          },
        },
        .identifier => switch (self.buffer[index]) {
          'a'...'z', 'A'...'Z', '_' => {
            result.tag = .identifier;
            result.end = index;
            index += 1;
          },
          else => {
            const KeywordEnum = enum {
              @"void", @"float", @"string", @"vector", @"if", @"else", @"while", @"do", @"local", @"return", @"0unknown",
            };
            switch (std.meta.stringToEnum(KeywordEnum, self.buffer[result.start..result.end + 1]) orelse .@"0unknown") {
              .@"void", .@"float", .@"string", .@"vector" => result.tag = .type,
              .@"if" => result.tag = .kw_if,
              .@"else" => result.tag = .kw_else,
              .@"while" => result.tag = .kw_while,
              .@"do" => result.tag = .kw_do,
              .@"local" => result.tag = .kw_local,
              .@"return" => result.tag = .kw_return,
              .@"0unknown" => result.tag = .identifier,
            }
            return result;
          }
        },
        .builtin_literal => switch (self.buffer[index]) {
          '0'...'9' => {
            result.tag = .builtin_literal;
            result.end = index;
            index += 1;
          },
          else => return result,
        },
        .float_literal => switch (self.buffer[index]) {
          '0'...'9', '.' => {
            result.tag = .float_literal;
            result.end = index;
            index += 1;
          },
          else => return result,
        },
        .string_literal => switch (self.buffer[index]) {
          '"' => {
            result.tag = .string_literal;
            result.end = index;
            return result;
          },
          else => {
            result.tag = .string_literal;
            result.end = index;
            index += 1;
          }
        },
        .vector_literal => switch (self.buffer[index]) {
          '\'' => {
            result.tag = .vector_literal;
            result.end = index;
            return result;
          },
          '0'...'9', '+', '-', '.', ' ', '\t', '\n', '\r' => {
            result.tag = .vector_literal;
            result.end = index;
            index += 1;
          },
          else => |c| {
            _ = try std.fmt.bufPrintZ(&err.message, "unexpected character in vector literal: {c}", .{ c });
            err.location = getLocation(self.buffer, index);
            return error.UnexpectedCharacter;
          },
        },
        .comment => switch (self.buffer[index]) {
          '\n' => {
            result.tag = .comment;
            result.end = index;
            return result;
          },
          else => index += 1,
        },
      }
    }

    return result;
  }

};

const Ast = struct {
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

    pub fn fromName(name: []const u8) Self {
      if (std.mem.eql(u8, name, "void")) {
        return QType.Void;
      }
      if (std.mem.eql(u8, name, "float")) {
        return QType.Float;
      }
      if (std.mem.eql(u8, name, "vector")) {
        return QType.Vector;
      }
      if (std.mem.eql(u8, name, "string")) {
        return QType.String;
      }
      @panic("unknown name");
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

  const Program = struct {
    declarations: []const Node,
  };

  const VarDecl = struct {
    type: QType,
    name: []const u8,
    value: ?Expression,
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

  const FloatLiteral = struct {
    value: f32,
  };

  const ExpressionType = enum {
    float_literal,
  };

  const Expression = union(ExpressionType) {
    float_literal: FloatLiteral,

    pub fn prettyPrint(self: @This(), string: []u8) !usize {
      var written: usize = 0;
      switch (self) {
        .float_literal => |f| {
          written += (try std.fmt.bufPrint(string[written..], "{d}", .{ f.value })).len;
        }
      }
      return written;
    }
  };

  const NodeType = enum {
    program,
    var_decl,
    fn_decl,
    field_decl,
    builtin_decl,
    expression,
  };

  const Node = union(NodeType) {
    program: Program,
    var_decl: VarDecl,
    fn_decl: FnDecl,
    field_decl: FieldDecl,
    builtin_decl: BuiltinDecl,
    expression: Expression,

    pub fn prettyPrint(self: @This(), string: []u8) !usize {
      var written: usize = 0;
      switch (self) {
        .var_decl => |d| {
          written += try d.type.prettyPrint(string);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          if (d.value) |value| {
            written += (try std.fmt.bufPrint(string[written..], " = ", .{})).len;
            std.log.warn("value {}", .{ value });
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
        .expression => |expression| {
          written += try expression.prettyPrint(string);
        },
        .program => |p| {
          for (p.declarations) |declaration| {
            written += try declaration.prettyPrint(string);
          }
        },
        // .scope => |s| {
        //   written += (try std.fmt.bufPrint(string[written..], "{{\n", .{})).len;
        //   for (s.instructions) |param| {
        //     // TODO: indentation here
        //     written += try param.prettyPrint(string[written..]);
        //     written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
        //   }
        //   written += (try std.fmt.bufPrint(string[written..], "}}\n", .{})).len;
        // },
      }
      return written;
    }
  };
};

const ParseError = error {
  EmptySource,
  UnexpectedInput,
  NoSpaceLeft,
};

fn makeError(errcode: ParseError, location: ?Location, err: *GenericError, comptime format: []const u8,
  args: anytype) ParseError {
  if (location) |loc| {
    _ = try std.fmt.bufPrint(&err.message, "error:{}:{}: " ++ format,
      .{ loc.row, loc.column } ++ args);
  } else {
    _ = try std.fmt.bufPrint(&err.message, "error: " ++ format, args);
  }
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
    const StatementLimit = 512;
    var program: [StatementLimit]Ast.Node = undefined;
    var declIndex: u16 = 0;

    while (true) {
      const token = try self.tokenizer.peek(err);
      std.log.warn("parse token {}", .{ token });
      switch (token.tag) {
        Token.Tag.comment => {}, // ignore
        Token.Tag.type => {
          program[declIndex] = try self.parseDeclaration(err);
          declIndex += 1;
        },
        // Token.Tag.dot => { // field decl
        // },
        Token.Tag.eof => {
          if (declIndex == 0) {
            // Do not allow empty source file
            return makeError(ParseError.EmptySource, null, err, "Empty source file", .{});
          }
          return Ast.Node { .program = Ast.Program{
            .declarations = program[0..declIndex],
          } };
        },
        else => |t| return makeError(ParseError.EmptySource, null, err, "Unexpected token {}", .{ t }),
      }
      if (declIndex >= StatementLimit) {
        return makeError(ParseError.EmptySource, null, err, "Too many statement. Limit is {}", .{ StatementLimit });
      }
    }
  }

  fn parseDeclaration(self: *Self, err: *GenericError) !Ast.Node {
    const typeToken = try self.tokenizer.next(err);
    const identifierToken = try self.tokenizer.peek(err);
    switch (identifierToken.tag) {
      Token.Tag.identifier => {
        return try self.parseVariableDefinition(typeToken, err);
      },
      else => |t| return makeError(ParseError.EmptySource,
        getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "Unexpected token {}", .{ t }),
    }
  }

  fn parseVariableDefinition(self: *Self, typeToken: Token, err: *GenericError) !Ast.Node {
    const identifierToken = try self.tokenizer.next(err);
    if (identifierToken.tag != Token.Tag.identifier) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting identifier found {}", .{ identifierToken });
    }
    const eqlToken = try self.tokenizer.next(err);
    switch (eqlToken.tag) {
      Token.Tag.equal => {
        const expression = try self.parseExpression(err);
        const scToken = try self.tokenizer.next(err);
        switch (scToken.tag) {
          Token.Tag.semicolon => {
            // std.log.warn("create declaration with value {}", .{ expression });
            return Ast.Node{ .var_decl = Ast.VarDecl{
              .type = Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
              .name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1],
              .value = expression,
            }};
          },
          else => |t| return makeError(ParseError.EmptySource,
            getLocation(self.tokenizer.buffer, identifierToken.start), err,
            "Unexpected token {}", .{ t }),
        }
      },
      Token.Tag.semicolon => {
        return Ast.Node{ .var_decl = Ast.VarDecl{
          .type = Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
          .name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1],
          .value = null,
        }};
      },
      else => return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting '=' or ';' found {}", .{ eqlToken }),
    }
  }

  fn parseExpression(self: *Self, err: *GenericError) !Ast.Expression {
    const token = try self.tokenizer.next(err);
    switch (token.tag) {
      Token.Tag.float_literal => {
        const value = try std.fmt.parseFloat(f32, self.tokenizer.buffer[token.start..token.end + 1]);
        std.log.warn("parseExpression float literal value {}", .{ value });
        return Ast.Expression{ .float_literal = Ast.FloatLiteral{ .value = value } };
      },
      else => return makeError(ParseError.EmptySource, getLocation(self.tokenizer.buffer, token.start), err,
       "Unexpected token {}", .{ token }),
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

// test "tokenizer test" {
//   var err = GenericError{};
//   try testTokenize("(this is an_identifier)", &.{
//     Token.Tag.l_paren,
//     Token.Tag.identifier,
//     Token.Tag.identifier,
//     Token.Tag.identifier,
//     Token.Tag.r_paren,
//     Token.Tag.eof,
//   }, &err);
//   try testTokenize(".vector field;", &.{
//     Token.Tag.dot,
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.semicolon,
//     Token.Tag.eof,
//   }, &err);
//   try testTokenize("float f = 3.14;", &.{
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.float_literal,
//     Token.Tag.semicolon,
//     Token.Tag.eof,
//   }, &err);
//   try testTokenize("string s = \"pi\";", &.{
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.string_literal,
//     Token.Tag.semicolon,
//     Token.Tag.eof,
//   }, &err);
//   try testTokenize("vector v = '+0.5 -1 -.2';", &.{
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.vector_literal,
//     Token.Tag.semicolon,
//     Token.Tag.eof,
//   }, &err);
//   try testTokenize("void (float f, vector v) foo = {}", &.{
//     Token.Tag.type,
//     Token.Tag.l_paren,
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.comma,
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.r_paren,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.l_bracket,
//     Token.Tag.r_bracket,
//     Token.Tag.eof,
//   }, &err);
//   try testTokenize("void\t(string str, ...)\tprint = #99;", &.{
//     Token.Tag.type,
//     Token.Tag.l_paren,
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.comma,
//     Token.Tag.elipsis,
//     Token.Tag.r_paren,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.builtin_literal,
//     Token.Tag.semicolon,
//     Token.Tag.eof,
//   }, &err);

//   const comments =
//     \\ // Let's describe this function
//     \\ void () main = {
//     \\   string foo = "foo";
//     \\   printf(foo); // prints foo
//     \\ }
//   ;
//   try testTokenize(comments, &.{
//     Token.Tag.comment,
//     Token.Tag.type,
//     Token.Tag.l_paren,
//     Token.Tag.r_paren,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.l_bracket,
//     Token.Tag.type,
//     Token.Tag.identifier,
//     Token.Tag.equal,
//     Token.Tag.string_literal,
//     Token.Tag.semicolon,
//     Token.Tag.identifier,
//     Token.Tag.l_paren,
//     Token.Tag.identifier,
//     Token.Tag.r_paren,
//     Token.Tag.semicolon,
//     Token.Tag.comment,
//     Token.Tag.r_bracket,
//     Token.Tag.eof,
//   }, &err);

//   const conditions =
//     \\if ("foo" == "foo" && 1 != 2 || false && true) {
//     \\  printf(foo);
//     \\}
//   ;
//   try testTokenize(conditions, &.{
//     Token.Tag.kw_if,
//     Token.Tag.l_paren,
//     Token.Tag.string_literal,
//     Token.Tag.double_equal,
//     Token.Tag.string_literal,
//     Token.Tag.and_,
//     Token.Tag.float_literal,
//     Token.Tag.not_equal,
//     Token.Tag.float_literal,
//     Token.Tag.or_,
//     Token.Tag.false,
//     Token.Tag.and_,
//     Token.Tag.true,
//     Token.Tag.r_paren,
//     Token.Tag.l_bracket,
//     Token.Tag.identifier,
//     Token.Tag.l_paren,
//     Token.Tag.identifier,
//     Token.Tag.r_paren,
//     Token.Tag.semicolon,
//     Token.Tag.r_bracket,
//     Token.Tag.eof,
//   }, &err);
// }

fn expectEqualString(lhs: []const u8, rhs: []const u8) !void {
  if (std.mem.eql(u8, lhs, rhs)) {
    return;
  } else {
    std.log.err("\n\"{s}\" ({}) \n not equal to\n\"{s}\" ({})", .{ lhs, lhs.len, rhs, rhs.len });
    return error.Fail;
  }
}

fn testParse(source: [:0]const u8, err: *GenericError) !void {
  var output: [4096:0]u8 = .{ 0 } ** 4096;
  var parser = Parser.init(source);
  const node = try parser.parse(err);
  _ = try node.prettyPrint(&output);
  try expectEqualString(source, std.mem.span(@as([*:0]const u8, @ptrCast(&output))));
}

test "parser test" {
  var err = GenericError{};
  try testParse("float f = 3.14;", &err);
}
