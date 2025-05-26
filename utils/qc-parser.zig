// Parse QuakeC bytecode.
//
// reference:
// https://github.com/vkazanov/tree-sitter-quakec/blob/main/grammar.js
// https://icculus.org/~marco/quakec/fteqcc_manual.txt
//
// test: clear && zig test -freference-trace qc-parser.zig
const std = @import("std");
const dat = @import("dat");

const ParseError = error {
  EmptySource,
  UnexpectedInput,
  NoSpaceLeft,
  NotAVectorMissingLeadingQuote,
  NotAVectorMissingClosingQuote,
  NotAVector,
  NotAType,
  NodeListOutOfBound,
  UnexpectedCharacter,
  UnknownOperator,
  NodeOutOfCapacity,
  InvalidCharacter,
  MissingMatchingParenthesis,
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

fn indentPrint(buf: []u8, indent: usize, comptime fmt: []const u8, args: anytype) !usize {
  var written: usize = 0;
  for (0..indent) |_| {
    _ = try std.fmt.bufPrint(buf[written..], " ", .{});
    written += 1;
  }
  written += (try std.fmt.bufPrint(buf[written..], fmt, args)).len;
  return written;
}

pub const Token = struct {
  tag: Tag,
  start: usize,
  end: usize,

  pub const Tag = enum {
    eof,
    // new_line,

    dot,
    comma,
    colon,
    semicolon,
    l_paren,
    r_paren,
    l_bracket,
    r_bracket,
    l_brace,
    r_brace,
    equal,
    double_equal,
    not_equal,
    plus_equal,
    minus_equal,
    mul_equal,
    slash_equal,
    or_equal,
    and_equal,
    and_tilde_equal,
    modulo_equal,
    caret_equal,
    not,
    kw_not,
    plus,
    minus,
    mul,
    modulo,
    tilde,
    caret,
    less_than,
    greater_than,
    less_or_equal,
    greater_or_equal,
    spaceship, // <=>
    power, // **
    elipsis,
    comment,
    quote,
    double_quote,
    slash,
    and_,
    or_,
    ampersand,
    pipe,
    question,
    true,
    false,

    identifier,
    type,
    vector_literal,
    string_literal,
    float_literal,
    builtin_literal,
    frame_identifier,

    kw_if,
    kw_else,
    kw_while,
    kw_do,
    kw_for,
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
  location: Location = .{ .column = 1, .row = 1 },

  pub fn prettyPrint(self: @This(), buf: []u8) !usize {
    return (try std.fmt.bufPrint(buf, "{}:{}: {s}", .{
      self.location.row, self.location.column, self.message,
    })).len;
  }
};

pub fn getLocation(buffer: []const u8, index: usize) Location {
  var i: usize = 0;
  var location = Location{ .column = 1, .row = 1 };
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

  buffer: []const u8,
  index: usize,
  // Used to cache peek result
  next_token: ?Token = null,

  const State = enum {
    start,
    identifier,
    builtin_literal,
    frame_identifier,
    float_literal,
    string_literal,
    vector_literal,
    comment,
    multiline_comment,
  };

  pub fn init(buffer: []const u8) Self {
    return Self {
      .buffer = buffer,
      .index = 0,
    };
  }

  pub fn next(self: *Self, err: *GenericError) !Token {
    if (self.next_token) |token| {
      self.next_token = null;
      self.index = token.end + 1;
      // std.log.debug("next {}", .{ token });
      return token;
    } else {
      const token = try self.get_next_token(err);
      self.index = token.end + 1;
      // std.log.debug("next {}", .{ token });
      return token;
    }
  }

  pub fn peek(self: *Self, err: *GenericError) ParseError!Token {
    if (self.next_token) |token| {
      return token;
    } else {
      self.next_token = try self.get_next_token(err);
      return self.next_token.?;
    }
  }

  // Peek several token in advance
  // used for update expressions (i++, j--)
  // FIXME: This function is possible because we can rewind the index which is
  // only possible because we work on an array and not on a file.
  // Ideally we would need a ring buffer of token with an index on which the
  // next token is.
  pub fn peekAt(self: *Self, err: *GenericError, comptime tokenIndex: usize) ParseError!Token {
    if (tokenIndex == 0) @compileError("peekAt index start at 1");

    if (tokenIndex == 1) {
      return self.peek(err);
    }

    var tokenIndex2 = tokenIndex;
    const backup = self.index;
    var token: Token = undefined;
    while (tokenIndex2 > 0) : ({ tokenIndex2 -= 1; }) {
      token = try self.get_next_token(err);
      self.index = token.end + 1;
    }

    // rewind
    self.index = backup;
    return token;
  }

  // Get the next token but does not advance the index position
  fn get_next_token(self: *Self, err: *GenericError) ParseError!Token {
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
      switch (state) {
        .start => switch (self.buffer[index]) {
          ' ', '\t', '\n', '\r' => index += 1, // ignore whitespace
          ':' => {
            result.tag = .colon;
            result.start = index;
            result.end = index;
            return result;
          },
          '?' => {
            result.tag = .question;
            result.start = index;
            result.end = index;
            return result;
          },
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
          '[' => {
            result.tag = .l_bracket;
            result.start = index;
            result.end = index;

            return result;
          },
          ']' => {
            result.tag = .r_bracket;
            result.start = index;
            result.end = index;
            return result;
          },
          '{' => {
            result.tag = .l_brace;
            result.start = index;
            result.end = index;
            return result;
          },
          '}' => {
            result.tag = .r_brace;
            result.start = index;
            result.end = index;
            return result;
          },
          '.' => {
            result.start = index;
            if (self.buffer.len > index + 3 and std.mem.eql(u8, self.buffer[index..index + 3], "...")) {
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
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .plus_equal;
              result.end = index + 1;
            } else {
              result.tag = .plus;
              result.end = index;
            }
            return result;
          },
          '-' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .minus_equal;
              result.end = index + 1;
            } else {
              result.tag = .minus;
              result.end = index;
            }
            return result;
          },
          '*' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .mul_equal;
              result.end = index + 1;
            } else if (index + 1 < self.buffer.len and self.buffer[index + 1] == '*') {
              result.tag = .power;
              result.end = index + 1;
            } else {
              result.tag = .mul;
              result.end = index;
            }
            return result;
          },
          '%' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .modulo_equal;
              result.end = index + 1;
            } else {
              result.tag = .modulo;
              result.end = index;
            }
            return result;
          },
          '~' => {
            result.start = index;
            // ~= not part of the gramma?
            // if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
            //   result.tag = .tilde_equal;
            //   result.end = index + 1;
            // } else {
              result.tag = .tilde;
              result.end = index;
            // }
            return result;
          },
          '^' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .caret_equal;
              result.end = index + 1;
            } else {
              result.tag = .caret;
              result.end = index;
            }
            return result;
          },
          '<' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '='
              and index + 2 < self.buffer.len and self.buffer[index + 2] == '>') {
              result.tag = .spaceship;
              result.end = index + 2;
            } else if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .less_or_equal;
              result.end = index + 1;
            } else {
              result.tag = .less_than;
              result.end = index;
            }
            return result;
          },
          '>' => {
            result.start = index;
            if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .greater_or_equal;
              result.end = index + 1;
            } else {
              result.tag = .greater_than;
              result.end = index;
            }
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
            } else if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .and_equal;
              result.end = index + 1;
            } else if ((index + 1 < self.buffer.len and self.buffer[index + 1] == '~')
              and (index + 2 < self.buffer.len and self.buffer[index + 2] == '=')) {
              result.tag = .and_tilde_equal;
              result.end = index + 2;
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
            } else if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .or_equal;
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
            } else if (index + 1 < self.buffer.len and self.buffer[index + 1] == '*') {
              state = .multiline_comment;
              result.start = index;
              index += 1;
            } else if (index + 1 < self.buffer.len and self.buffer[index + 1] == '=') {
              result.tag = .slash_equal;
              result.start = index;
              result.end = index + 1;
              return result;
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
          '$' => {
            state = .frame_identifier;
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
            std.log.warn("expected {c}", .{ c });
            return error.UnexpectedCharacter;
          },
        },
        .identifier => switch (self.buffer[index]) {
          'a'...'z', 'A'...'Z', '0'...'9', '_' => {
            result.tag = .identifier;
            result.end = index;
            index += 1;
          },
          else => {
            const KeywordEnum = enum {
              @"void", @"float", @"string", @"vector", @"entity", @"if", @"else", @"while", @"do",
              @"for", @"local", @"return", @"not", @"0unknown",
            };
            switch (std.meta.stringToEnum(KeywordEnum, self.buffer[result.start..result.end + 1]) orelse .@"0unknown") {
              .@"void", .@"float", .@"string", .@"vector", .@"entity" => result.tag = .type,
              .@"if" => result.tag = .kw_if,
              .@"else" => result.tag = .kw_else,
              .@"while" => result.tag = .kw_while,
              .@"do" => result.tag = .kw_do,
              .@"for" => result.tag = .kw_for,
              .@"local" => result.tag = .kw_local,
              .@"return" => result.tag = .kw_return,
              .@"not" => result.tag = .kw_not,
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
        .frame_identifier => switch (self.buffer[index]) {
          'a'...'z', 'A'...'Z', '0'...'9' => {
            result.tag = .frame_identifier;
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
        .multiline_comment => {
          if (self.buffer[index] == '*' and
            index + 1 < self.buffer.len and self.buffer[index + 1] == '/') {
            result.tag = .comment;
            result.end = index + 1;
            return result;
          } else {
            index += 1;
          }
        },
      }
    }

    return result;
  }

};

pub const Ast = struct {
  const PrintError = error {
    NoSpaceLeft,
    EmptySource,
    UnexpectedInput,
    NotAVectorMissingLeadingQuote,
    NotAVectorMissingClosingQuote,
    NotAVector,
    NotAType,
    NodeListOutOfBound,
  };

  const QType = enum {
    Void,
    Float,
    Vector,
    String,
    Entity,

    const Self = @This();
    pub fn prettyPrint(self: Self, string: []u8) !usize {
      switch (self) {
        .Void => return (try std.fmt.bufPrint(string, "void", .{})).len,
        .Float => return (try std.fmt.bufPrint(string, "float", .{})).len,
        .Vector => return (try std.fmt.bufPrint(string, "vector", .{})).len,
        .String => return (try std.fmt.bufPrint(string, "string", .{})).len,
        .Entity => return (try std.fmt.bufPrint(string, "entity", .{})).len,
      }
    }

    pub fn fromName(name: []const u8) !Self {
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
      if (std.mem.eql(u8, name, "entity")) {
        return QType.Entity;
      }
      return error.NotAType;
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
    declarations: NodeList,
  };

  const VarDecl = struct {
    type: QType,
    name: []const u8,
    value: Node.Index,
    local: bool,
  };

  const ParamType = enum {
    elipsis,
    declaration,
  };

  const ParamDecl = struct {
    param: union(ParamType) {
      elipsis: void,
      declaration: struct {
        type: QType,
        name: []const u8,
      },
    },

    const Self = @This();
    pub fn prettyPrint(self: Self, string: []u8) !usize {
      switch (self.param) {
        .elipsis => {
          return (try std.fmt.bufPrint(string, "...", .{})).len;
        },
        .declaration => |param| {
          const written = try param.type.prettyPrint(string);
          return written + (try std.fmt.bufPrint(string[written..], " {s}", .{ param.name })).len;
        },
      }
    }
  };

  const IfStatement = struct {
    negated: bool,
    condition: Node.Index,
    statement: Node.Index,
    else_statement: ?Node.Index,
  };

  const WhileStatement = struct {
    condition: Node.Index,
    statement: Node.Index,
  };

  const DoWhileStatement = WhileStatement;

  const ForStatement = struct {
    initializer: ?Node.Index,
    condition: Node.Index,
    loopexpr: ?Node.Index,
    statement: Node.Index,
  };

  const StatementType = enum {
    var_decl,
    return_statement,
    if_statement,
    while_statement,
    do_while_statement,
    for_statement,
    expression,
  };

  const Statement = union(StatementType) {
    var_decl: Node.Index,
    return_statement: Node.Index,
    if_statement: IfStatement,
    while_statement: WhileStatement,
    do_while_statement: DoWhileStatement,
    for_statement: ForStatement,
    expression: Node.Index,

    const Self = @This();
    pub fn prettyPrint(self: Self, string: []u8) !usize {
      _ = self;
      _ = string;
      return 0;
    }
  };

  const BodyType = enum {
    statement_list,
    builtin_immediate,
  };

  const Body = union(BodyType) {
    statement_list: NodeList,
    builtin_immediate: u16,
  };

  const FrameSpecifier = struct {
    frame_identifier: Identifier,
    next_function: Identifier,
  };

  const FnDecl = struct {
    return_type: QType,
    name: []const u8,
    frame_specifier: ?FrameSpecifier,
    parameter_list: NodeList,
    body: Node.Index,
  };

  const FieldDecl = struct {
    type: QType,
    name: []const u8,
  };

  const MethodDecl = struct {
    return_type: QType,
    name: []const u8,
    frame_specifier: ?FrameSpecifier,
    parameter_list: NodeList,
  };

  const BuiltinDecl = struct {
    name: []const u8,
    return_type: QType,
    parameter_list: NodeList,
    index: usize,
  };

  const FloatLiteral = struct {
    value: f32,
  };

  const StringLiteral = struct {
    value: []const u8,
  };

  const VectorLiteral = struct {
    value: @Vector(3, f32),

    pub fn fromString(str: []const u8) !VectorLiteral {
      if (str[0] != '\'') return error.NotAVectorMissingLeadingQuote;
      if (str[str.len - 1] != '\'') return error.NotAVectorMissingClosingQuote;
      var it = std.mem.splitScalar(u8, str[1..str.len - 1], ' ');
      var part = it.next();
      const x = if (part) |p|
        std.fmt.parseFloat(f32, p) catch return error.NotAVector else return error.NotAVector;
      part = it.next();
      const y = if (part) |p|
        std.fmt.parseFloat(f32, p) catch return error.NotAVector else return error.NotAVector;
      part = it.next();
      const z = if (part) |p|
        std.fmt.parseFloat(f32, p) catch return error.NotAVector else return error.NotAVector;
      return VectorLiteral{
        .value = @Vector(3, f32){ x, y, z },
      };
    }
  };

  const Identifier = struct {
    name: []const u8,
  };

  const Operator = enum {
    plus,
    minus,
    mul,
    div,
    modulo,
    not,
    complement,
    equal,
    double_equal,
    not_equal,
    plus_equal,
    minus_equal,
    mul_equal,
    slash_equal,
    or_equal,
    and_equal,
    and_tilde_equal,
    modulo_equal,
    caret_equal,
    less_than,
    greater_than,
    less_or_equal,
    greater_or_equal,
    logical_and,
    logical_or,
    spaceship, // <=>
    power, // **

    fn fromToken(t: Token) !Operator {
      return switch (t.tag) {
        .plus => .plus,
        .minus => .minus,
        .mul => .mul,
        .slash => .div,
        .modulo => .modulo,
        .not => .not,
        .tilde => .complement,
        .equal => .equal,
        .double_equal => .double_equal,
        .not_equal => .not_equal,
        .plus_equal => .plus_equal,
        .minus_equal => .minus_equal,
        .mul_equal => .mul_equal,
        .slash_equal => .slash_equal,
        .or_equal => .or_equal,
        .and_equal => .and_equal,
        .and_tilde_equal => .and_tilde_equal,
        .modulo_equal => .modulo_equal,
        .caret_equal => .caret_equal,
        .less_than => .less_than,
        .greater_than => .greater_than,
        .less_or_equal => .less_or_equal,
        .greater_or_equal => .greater_or_equal,
        .and_ => .logical_and,
        .or_ => .logical_or,
        .spaceship => .spaceship,
        .power => .power,
        else => error.UnknownOperator,
      };
    }

    pub fn toString(o: Operator) []const u8 {
      return switch (o) {
        .plus => "+",
        .minus => "-",
        .mul => "*",
        .div => "/",
        .modulo => "%",
        .not => "!",
        .complement => "~",
        .equal => "=",
        .double_equal => "==",
        .not_equal => "!=",
        .plus_equal => "+=",
        .minus_equal => "-=",
        .mul_equal => "*=",
        .slash_equal => "/=",
        .or_equal => "|=",
        .and_equal => "&=",
        .and_tilde_equal => "&~=",
        .modulo_equal => "%=",
        .caret_equal => "~",
        .less_than => "<",
        .greater_than => ">",
        .less_or_equal => "<=",
        .greater_or_equal => ">=",
        .logical_and => "&&",
        .logical_or => "||",
        .spaceship => "<=>",
        .power => "**",
      };
    }
  };

  const BinaryOp = struct {
    operator: Operator,
    lhs: Node.Index,
    rhs: Node.Index,
  };

  const UnaryOp = struct {
    operator: Operator,
    operand: Node.Index,
  };

  const FnCall = struct {
    callee: Node.Index,
    argument_list: NodeList,
  };

  const Subscript = struct {
    name: []const u8,
    index_expression: Node.Index,
  };

  const FieldExpression = struct {
    object: Node.Index,
    field: []const u8,
  };

  const UpdateExpression = struct {
    expression: Node.Index,
    operator: Operator,
  };

  const Assignment = struct {
    identifier: Node.Index,
    operator: Operator,
    value: Node.Index,
  };

  const Ternary = struct {
    condition: Node.Index,
    true_clause: Node.Index,
    false_clause: Node.Index,
  };

  const ExpressionType = enum {
    binary_op,
    unary_op,
    fn_call,
    float_literal,
    string_literal,
    vector_literal,
    identifier,
    parenthesized_expression,
    subscript_expression,
    update_expression,
    field_expression,
    frame_identifier,
    assignment,
    ternary,
  };

  const Expression = union(ExpressionType) {
    binary_op: BinaryOp,
    unary_op: UnaryOp,
    fn_call: FnCall,
    float_literal: FloatLiteral,
    string_literal: StringLiteral,
    vector_literal: VectorLiteral,
    identifier: Identifier,
    parenthesized_expression: Node.Index, // *?Expression
    subscript_expression: Subscript,
    update_expression: UpdateExpression,
    field_expression: FieldExpression,
    frame_identifier: Identifier,
    assignment: Assignment,
    ternary: Ternary,

    pub fn prettyPrint(self: @This(), nodes: []Node, string: []u8) PrintError!usize {
      var written: usize = 0;
      switch (self) {
        .unary_op => |u| {
          written += (try std.fmt.bufPrint(string[written..], "{s}", .{ Operator.toString(u.operator) })).len;
          written += try nodes[u.operand].prettyPrint(nodes, string[written..]);
        },
        .binary_op => |b| {
          written += try nodes[b.lhs].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " {s} ", .{ Operator.toString(b.operator) })).len;
          written += try nodes[b.rhs].prettyPrint(nodes, string[written..]);
        },
        .fn_call => |fc| {
          written += try nodes[fc.callee].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], "(", .{})).len;
          var paramIt = fc.argument_list.iter();
          var i: usize = 0;
          while (paramIt.next()) |param| : ({ i += 1; }) {
            written += try param.prettyPrint(nodes, string[written..]);
            if (i < fc.argument_list.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ")", .{})).len;
        },
        .float_literal => |f| {
          written += (try std.fmt.bufPrint(string[written..], "{d}", .{ f.value })).len;
        },
        .string_literal => |s| {
          written += (try std.fmt.bufPrint(string[written..], "{s}", .{ s.value })).len;
        },
        .vector_literal => |v| {
          written += (try std.fmt.bufPrint(string[written..], "'{d} {d} {d}'", .{
            v.value[0], v.value[1], v.value[2],
          })).len;
        },
        .identifier => |i| {
          written += (try std.fmt.bufPrint(string[written..], "{s}", .{ i.name })).len;
        },
        .parenthesized_expression => |e| {
          written += (try std.fmt.bufPrint(string[written..], "(", .{})).len;
          written += try nodes[e].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], ")", .{})).len;
        },
        .subscript_expression => |e| {
          written += (try std.fmt.bufPrint(string[written..], "{s}", .{ e.name })).len;
          written += (try std.fmt.bufPrint(string[written..], "[", .{})).len;
          written += try nodes[e.index_expression].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], "]", .{})).len;
        },
        .update_expression => |e| {
          written += try nodes[e.expression].prettyPrint(nodes, string[written..]);
          written += switch (e.operator) {
            Ast.Operator.plus => (try std.fmt.bufPrint(string[written..], "++", .{})).len,
            Ast.Operator.minus => (try std.fmt.bufPrint(string[written..], "--", .{})).len,
            else => @panic("internal error: update expression with an unknown operator"),
          };
        },
        .field_expression => |f| {
          written += try nodes[f.object].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], ".{s}", .{ f.field })).len;
        },
        .frame_identifier => |f| {
          written += (try std.fmt.bufPrint(string[written..], "{s}", .{ f.name })).len;
        },
        .assignment => |a| {
          written += try nodes[a.identifier].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " {s} ", .{ Ast.Operator.toString(a.operator) })).len;
          written += try nodes[a.value].prettyPrint(nodes, string[written..]);
        },
        .ternary => |t| {
          written += try nodes[t.condition].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " ? ", .{})).len;
          written += try nodes[t.true_clause].prettyPrint(nodes, string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " : ", .{})).len;
          written += try nodes[t.false_clause].prettyPrint(nodes, string[written..]);
        },
      }
      return written;
    }
  };

  const PayloadType = enum {
    program,
    var_decl,
    fn_decl,
    field_decl,
    method_decl,
    builtin_decl,
    param_decl,
    statement,
    expression,
    body,
  };

  const Payload = union(PayloadType) {
    program: Program,
    var_decl: VarDecl,
    fn_decl: FnDecl,
    field_decl: FieldDecl,
    method_decl: MethodDecl,
    builtin_decl: BuiltinDecl,
    param_decl: ParamDecl,
    statement: Statement,
    expression: Expression,
    body: Body,
  };

  pub const Node = struct {
    const Self = @This();
    const Index = u32;

    payload: Payload,
    next: Node.Index,

    pub fn init(payload: Payload) Node {
      return Self{
        .payload = payload,
        .next = 0,
      };
    }

    fn getNode(nodes: []Node, index: Node.Index) ?Node {
      if (index == 0) return null;
      return nodes[index];
    }

    pub fn debugPrint(self: @This(), nodes: []Node, string: []u8, indent: usize) PrintError!usize {
      var written: usize = 0;
      switch (self.payload) {
        .var_decl => |d| {
          written += try indentPrint(string[written..], indent, "VAR_DECL {s}\n", .{ d.name });
          var typestr: [255]u8 = undefined;
          const n = try d.type.prettyPrint(&typestr);
          written += try indentPrint(string[written..], indent, "  type {s}\n", .{ typestr[0..n] });
          written += try indentPrint(string[written..], indent, "  local {}\n", .{ d.local });
          written += try indentPrint(string[written..], indent, "  value {}\n", .{ d.value });
        },
        .fn_decl => |f| {
          written += try indentPrint(string[written..], indent, "FN_DECL {s}\n", .{ f.name });
          written += try indentPrint(string[written..], indent, "  return type {}\n", .{ f.return_type });
          written += try indentPrint(string[written..], indent, "  parameters:\n", .{});
          var paramIt = f.parameter_list.iter();
          while (paramIt.next()) |param| {
            written += try param.debugPrint(nodes, string[written..], indent + 4);
          }
          if (f.frame_specifier) |fs| {
            written += try indentPrint(string[written..], indent, "  frame specifiers:\n", .{});
            written += try indentPrint(string[written..], indent, "    frame: {s}\n", .{ fs.frame_identifier.name });
            written += try indentPrint(string[written..], indent, "    next fn: {s}\n", .{ fs.next_function.name });
          }
          written += try indentPrint(string[written..], indent, "  frame_specifier:\n", .{});

          written += try indentPrint(string[written..], indent, "  body ", .{});
          if (getNode(nodes, f.body)) |body| {
            written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
            written += try body.debugPrint(nodes, string[written..], indent + 4);
            written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
          } else {
            written += (try std.fmt.bufPrint(string[written..], " {{}}\n", .{})).len;
          }
        },
        .field_decl => |d| {
          written += try indentPrint(string[written..], indent, "FIELD_DECL {s}\n", .{ d.name });
          var typestr: [255]u8 = undefined;
          const n = try d.type.prettyPrint(&typestr);
          written += try indentPrint(string[written..], indent, "  type {s}\n", .{ typestr[0..n] });
        },
        .method_decl => |f| {
          written += try indentPrint(string[written..], indent, "METHOD_DECL {s}\n", .{ f.name });
          written += try indentPrint(string[written..], indent, "  return type {}\n", .{ f.return_type });
          written += try indentPrint(string[written..], indent, "  parameters:\n", .{});
          var paramIt = f.parameter_list.iter();
          while (paramIt.next()) |param| {
            written += try param.debugPrint(nodes, string[written..], indent + 4);
          }
          if (f.frame_specifier) |fs| {
            written += try indentPrint(string[written..], indent, "  frame specifiers:\n", .{});
            written += try indentPrint(string[written..], indent, "    frame: {s}\n", .{ fs.frame_identifier.name });
            written += try indentPrint(string[written..], indent, "    next fn: {s}\n", .{ fs.next_function.name });
          }
          written += try indentPrint(string[written..], indent, "  frame_specifier:\n", .{});
        },
        .builtin_decl => |d| {
          written += try indentPrint(string[written..], indent, "BUILTIN_DECL {s}\n", .{ d.name });
        },
        .expression => |e| {
          switch (e) {
            .update_expression => |ue| {
              written += try indentPrint(string[written..], indent, "UPDATE_EXPRESSION\n", .{});
              written += try indentPrint(string[written..], indent, "  operator: {s}\n", .{ Ast.Operator.toString(ue.operator) });
              written += try indentPrint(string[written..], indent, "  subexpression:\n", .{});
              written += try nodes[ue.expression].debugPrint(nodes, string[written..], indent + 4);
            },
            .subscript_expression => |se| {
              written += try indentPrint(string[written..], indent, "SUBSCRIPT_EXPRESSION\n", .{});
              written += try indentPrint(string[written..], indent, "  name {s}\n", .{ se.name });
              written += try indentPrint(string[written..], indent, "  index:\n", .{});
              written += try nodes[se.index_expression].debugPrint(nodes, string[written..], indent + 4);
            },
            else => written += try indentPrint(string[written..], indent, "EXPRESSION\n", .{}),
          }
        },
        .program => |p| {
          var it = p.declarations.iter();
          while (it.next()) |declaration| {
            written += try declaration.debugPrint(nodes, string[written..], indent);
          }
        },
        .param_decl => |p| {
          var paramstr: [255]u8 = undefined;
          const n = try p.prettyPrint(&paramstr);
          written += try indentPrint(string[written..], indent, "PARAM_DECL {s}\n", .{ paramstr[0..n] });
        },
        .statement => |s| {
          written += try indentPrint(string[written..], indent, "STATEMENT", .{});
          written += switch (s) {
            .var_decl => (try std.fmt.bufPrint(string[written..], " VAR_DECL\n", .{})).len,
            .return_statement => (try std.fmt.bufPrint(string[written..], " RETURN_STATEMENT\n", .{})).len,
            .if_statement => (try std.fmt.bufPrint(string[written..], " IF_STATEMENT\n", .{})).len,
            .while_statement => (try std.fmt.bufPrint(string[written..], " WHILE_STATEMENT\n", .{})).len,
            .do_while_statement => (try std.fmt.bufPrint(string[written..], " DO_WHILE_STATEMENT\n", .{})).len,
            .for_statement => (try std.fmt.bufPrint(string[written..], " FOR_STATEMENT\n", .{})).len,
            .expression => (try std.fmt.bufPrint(string[written..], " EXPRESSION\n", .{})).len,
          };
        },
        .body => |b| {
          switch (b) {
            .statement_list => |list| {
              written += try indentPrint(string[written..], indent, "STATEMENTS:\n", .{});
              var statementIt = list.iter();
              while (statementIt.next()) |statement| {
                written += try statement.debugPrint(nodes, string[written..], indent + 2);
              }
            },
            .builtin_immediate => |_| {
              written += try indentPrint(string[written..], indent, "BUILTIN_IMMEDIATE\n", .{});
            },
          }
          return written;
        },
      }
      return written;
    }

    pub fn prettyPrint(self: @This(), nodes: []Node, string: []u8) PrintError!usize {
      return try self.prettyPrintIndent(nodes, string, 0);
    }

    pub fn prettyPrintIndent(self: @This(), nodes: []Node, string: []u8, indent: usize) PrintError!usize {
      var written: usize = 0;
      switch (self.payload) {
        .var_decl => |d| {
          if (d.local) {
            written += try indentPrint(string[written..], indent, "local ", .{});
          } else {
            written += try indentPrint(string[written..], indent, "", .{});
          }
          written += try d.type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          if (getNode(nodes, d.value)) |value| {
            written += (try std.fmt.bufPrint(string[written..], " = ", .{})).len;
            written += try value.prettyPrint(nodes, string[written..]);
          }
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .fn_decl => |f| {
          written += try indentPrint(string[written..], indent, "", .{});
          written += try f.return_type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          var paramIt = f.parameter_list.iter();
          var i: usize = 0;
          while (paramIt.next()) |param| : (i += 1) {
            written += try param.prettyPrintIndent(nodes, string[written..], indent);
            if (i < f.parameter_list.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ") {s}", .{ f.name })).len;

          const fbody = getNode(nodes, f.body);
          if (f.frame_specifier != null or fbody != null) {
            written += (try std.fmt.bufPrint(string[written..], " = ", .{})).len;
          }

          if (f.frame_specifier) |fs| {
            written += (try std.fmt.bufPrint(string[written..], "[{s}, {s}] ", .{
              fs.frame_identifier.name,
              fs.next_function.name,
            })).len;
          }

          if (fbody) |body| {
            written += try body.prettyPrintIndent(nodes, string[written..], indent);
          }
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .field_decl => |d| {
          written += try indentPrint(string[written..], indent, ".", .{});
          written += try d.type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .method_decl => |f| {
          written += try indentPrint(string[written..], indent, ".", .{});
          written += try f.return_type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          var paramIt = f.parameter_list.iter();
          var i: usize = 0;
          while (paramIt.next()) |param| : (i += 1) {
            written += try param.prettyPrintIndent(nodes, string[written..], indent);
            if (i < f.parameter_list.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ") {s}", .{ f.name })).len;
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .builtin_decl => |d| {
          written += try indentPrint(string[written..], indent, "", .{});
          written += try d.return_type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          var paramIt = d.parameter_list.iter();
          var i: usize = 0;
          while (paramIt.next()) |param| : (i += 1) {
            written += try param.prettyPrintIndent(nodes, string[written..], indent);
            if (i < d.parameter_list.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ") {s} = #{}", .{ d.name, d.index })).len;
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .expression => |expression| {
          written += try indentPrint(string[written..], indent, "", .{});
          written += try expression.prettyPrint(nodes, string[written..]);
        },
        .program => |p| {
          var it = p.declarations.iter();
          var i: usize = 0;
          while (it.next()) |declaration| : ({ i += 1; }) {
            written += try declaration.prettyPrintIndent(nodes, string[written..], indent);
            // written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
            if (i < p.declarations.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
            }
          }
        },
        .param_decl => |p| {
          written += try p.prettyPrint(string[written..]);
        },
        .statement => |s| {
          written += try indentPrint(string[written..], indent, "", .{});
          switch (s) {
            .var_decl => |v| {
              written += try nodes[v].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
            },
            .return_statement => |expr| {
              written += (try std.fmt.bufPrint(string[written..], "return ", .{})).len;
              written += try nodes[expr].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
            },
            .if_statement => |ifs| {
              written += (try std.fmt.bufPrint(string[written..], "if ", .{})).len;
              if (ifs.negated) {
                written += (try std.fmt.bufPrint(string[written..], "not (", .{})).len;
              } else {
                written += (try std.fmt.bufPrint(string[written..], "(", .{})).len;
              }
              written += try nodes[ifs.condition].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], ") ", .{})).len;
              written += try nodes[ifs.statement].prettyPrintIndent(nodes, string[written..], indent);
              if (ifs.else_statement) |e| {
                written += (try std.fmt.bufPrint(string[written..], " else ", .{})).len;
                written += try nodes[e].prettyPrintIndent(nodes, string[written..], indent);
              }
            },
            .while_statement => |w| {
              written += (try std.fmt.bufPrint(string[written..], "while (", .{})).len;
              written += try nodes[w.condition].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], ") ", .{})).len;
              written += try nodes[w.statement].prettyPrintIndent(nodes, string[written..], indent);
            },
            .do_while_statement => |w| {
              written += (try std.fmt.bufPrint(string[written..], "do ", .{})).len;
              written += try nodes[w.statement].prettyPrintIndent(nodes, string[written..], indent);
              written += (try std.fmt.bufPrint(string[written..], " while (", .{})).len;
              written += try nodes[w.condition].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], ")", .{})).len;
            },
            .for_statement => |w| {
              written += (try std.fmt.bufPrint(string[written..], "for (", .{})).len;
              if (w.initializer) |initializer| {
                written += try nodes[initializer].prettyPrint(nodes, string[written..]);
              }
              written += (try std.fmt.bufPrint(string[written..], "; ", .{})).len;
              written += try nodes[w.condition].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], "; ", .{})).len;
              if (w.loopexpr) |loopexpr| {
                written += try nodes[loopexpr].prettyPrint(nodes, string[written..]);
              }
              written += (try std.fmt.bufPrint(string[written..], ") ", .{})).len;
              written += try nodes[w.statement].prettyPrintIndent(nodes, string[written..], indent);
            },
            .expression => |e| {
              written += try nodes[e].prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
            },
          }
        },
        .body => |b| {
          switch (b) {
            .statement_list => |list| {
              // if (list.len() == 1) {
              //   const l = try list.get(0);
              //   written += try l.prettyPrint(nodes, string);
              //   return written;
              // } else {
                written += (try std.fmt.bufPrint(string[written..], "{{", .{})).len;
                if (list.len() > 0) {
                  written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
                }
                var statementIt = list.iter();
                while (statementIt.next()) |statement| {
                  written += try statement.prettyPrintIndent(nodes, string[written..], indent + 2);
                  written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
                }
                written += try indentPrint(string[written..], indent, "}}", .{});
                return written;
              // }
            },
            .builtin_immediate => |index| {
              return (try std.fmt.bufPrint(string, "#{}", .{ index })).len;
            },
          }
        },
      }
      return written;
    }
  };

  // Represent a virtual list in the Node array.
  // Simplify appending and get an element from an Index.
  const NodeList = struct {
    const Self = @This();
    head: Node.Index,
    nodes: []Node,

    // Initializing the list with a 0 index creates a new list.
    // The next element to be appended becomes the head of the list.
    fn init(head: Node.Index, nodes: []Node) Self {
      return Self{
        .head = head,
        .nodes = nodes,
      };
    }

    fn len(self: Self) usize {
      if (self.head == 0) return 0;
      var count: usize = 1;
      var node: Node = self.nodes[self.head];
      while (node.next != 0) {
        count += 1;
        node = self.nodes[node.next];
      }
      return count;
    }

    fn get(self: Self, index: Node.Index) !Node {
      if (self.head == 0) return error.NodeListOutOfBound;
      var node: Node = self.nodes[self.head];
      if (index == 0) return node;
      var count: usize = 0;
      while (node.next != 0) {
        count += 1;
        node = self.nodes[node.next];
        if (index == count) {
          return node;
        }
      }
      return error.NodeListOutOfBound;
    }

    fn appendNode(self: *Self, index: Node.Index) void {
      if (self.head == 0) {
        self.head = index;
      } else {
        var node = &self.nodes[self.head];
        while (node.next != 0) {
          node = &self.nodes[node.next];
        }
        node.next = index;
      }
    }

    const NodeListIterator = struct {
      nextIndex: Node.Index,
      nodes: []Node,

      pub fn next(self: *@This()) ?Node {
        if (self.nextIndex == 0) {
          return null;
        }
        const node = self.nodes[self.nextIndex];
        self.nextIndex = node.next;
        return node;
      }
    };

    fn iter(self: Self) NodeListIterator {
      return NodeListIterator{
        .nextIndex = self.head,
        .nodes = self.nodes,
      };
    }
  };
};

pub const Parser = struct {
  const Self = @This();

  tokenizer: Tokenizer,
  nodes: []Ast.Node,
  // The root node is 0 and will be set manually.
  nb_nodes: Ast.Node.Index = 1,

  pub fn init(buffer: []const u8, nodes: []Ast.Node) Self {
    return Self {
      .tokenizer = Tokenizer.init(buffer),
      .nodes = nodes,
    };
  }

  fn checkToken(self: *Self, token: Token, tag: Token.Tag, err: *GenericError) !void {
    if (token.tag != tag) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, token.start),
        err, "expecting {s} got {s}", .{ @tagName(tag), @tagName(token.tag) });
    }
  }

  // Append an already created node to the node list and return its index
  fn insertNode(self: *Self, payload: Ast.Payload) !Ast.Node.Index {
    // std.log.debug("insertNode {}", .{ payload });
    const node = Ast.Node{ .payload = payload, .next = 0 };
    if (self.nb_nodes >= self.nodes.len) {
      return error.NodeOutOfCapacity;
    }
    self.nodes[self.nb_nodes] = node;
    self.nb_nodes += 1;
    return self.nb_nodes - 1;
  }

  pub fn parse(self: *Self, err: *GenericError) !void {
    var declarations = Ast.NodeList.init(0, self.nodes);

    while (true) {
      const token = try self.tokenizer.peek(err);
      switch (token.tag) {
        Token.Tag.comment => _ = try self.tokenizer.next(err), // ignore
        Token.Tag.type => {
          try self.parseDeclaration(&declarations, err);
        },
        Token.Tag.dot => { // field decl
          declarations.appendNode(try self.parseFieldDefinition(err));
        },
        Token.Tag.eof => {
          if (declarations.len() == 0) {
            // Do not allow empty source file
            return makeError(ParseError.EmptySource, getLocation(self.tokenizer.buffer, token.start),
              err, "Empty source file", .{});
          }
          self.nodes[0] = Ast.Node.init(Ast.Payload{ .program = Ast.Program{
            .declarations = declarations,
          } });
          return;
        },
        else => |t| return makeError(ParseError.UnexpectedInput,
          getLocation(self.tokenizer.buffer, token.start), err, "Unexpected token {}", .{ t }),
      }
    }
  }

  const VariableType = enum {
    local,
    nonlocal,
  };

  fn parseDeclaration(self: *Self, declarations: *Ast.NodeList, err: *GenericError) !void {
    const typeToken = try self.tokenizer.next(err);
    const identifierToken = try self.tokenizer.peek(err);
    switch (identifierToken.tag) {
      Token.Tag.identifier => {
        const is_c_style_function = (try self.tokenizer.peekAt(err, 2)).tag == Token.Tag.l_paren;
        if (is_c_style_function) {
          declarations.appendNode(try self.parseFunctionDefinition(typeToken, err));
          return;
        }
        try self.parseVariableDefinition(declarations, typeToken, .nonlocal, err);
        const scToken = try self.tokenizer.next(err);
        try self.checkToken(scToken, Token.Tag.semicolon, err);
      },
      Token.Tag.l_paren => { // function declaration/definition
        const fn_def = try self.parseFunctionDefinition(typeToken, err);
        // semicolon is optional
        const sc_token = try self.tokenizer.peek(err);
        if (sc_token.tag == Token.Tag.semicolon) {
          _ = try self.tokenizer.next(err);
        }
        declarations.appendNode(fn_def);
      },
      else => |t| return makeError(ParseError.EmptySource,
        getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "Unexpected token {}", .{ t }),
    }
  }

  fn parseVariableDefinition(self: *Self, declarations: *Ast.NodeList, typeToken: Token,
    vtype: VariableType, err: *GenericError) !void {
    var identifier_token = try self.tokenizer.next(err);
    if (identifier_token.tag != Token.Tag.identifier) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifier_token.start), err,
        "expecting identifier found {}", .{ identifier_token.tag });
    }
    const eql_token = try self.tokenizer.peek(err);
    var value: Ast.Node.Index = 0;
    _ = b: switch (eql_token.tag) {
      Token.Tag.comma => {
        declarations.appendNode(try self.insertNode(Ast.Payload{ .var_decl = Ast.VarDecl{
          .type = try Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
          .name = self.tokenizer.buffer[identifier_token.start..identifier_token.end + 1],
          .value = value,
          .local = vtype == .local,
        }}));
        value = 0;
        // discard the comma
        _ = try self.tokenizer.next(err);
        identifier_token = try self.tokenizer.next(err);
        continue :b (try self.tokenizer.peek(err)).tag;
      },
      Token.Tag.equal => {
        _ = try self.tokenizer.next(err);
        value = try self.parseExpression(err);
        continue :b (try self.tokenizer.peek(err)).tag;
      },
      Token.Tag.semicolon => {
        declarations.appendNode(try self.insertNode(Ast.Payload{ .var_decl = Ast.VarDecl{
          .type = try Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
          .name = self.tokenizer.buffer[identifier_token.start..identifier_token.end + 1],
          .value = value,
          .local = vtype == .local,
        }}));
        value = 0;
        return;
      },
      else => return makeError(ParseError.UnexpectedInput,
        getLocation(self.tokenizer.buffer, identifier_token.start), err,
        "expecting '=' or ';' found {}", .{ eql_token }),
    };
  }

  fn parseFieldDefinition(self: *Self, err: *GenericError) !Ast.Node.Index {
    // discard dot
    _ = try self.tokenizer.next(err);
    const type_token = try self.tokenizer.next(err);
    const next_token = try self.tokenizer.peek(err);
    switch (next_token.tag) {
      Token.Tag.identifier => {
        _ = try self.tokenizer.next(err);
        const sc_token = try self.tokenizer.next(err);
        try self.checkToken(sc_token, Token.Tag.semicolon, err);
        return self.insertNode(Ast.Payload{ .field_decl = Ast.FieldDecl{
          .type = try Ast.QType.fromName(self.tokenizer.buffer[type_token.start..type_token.end + 1]),
          .name = self.tokenizer.buffer[next_token.start..next_token.end + 1],
        }});
      },
      Token.Tag.l_paren => {
        var method_decl = Ast.MethodDecl{
          .return_type = try Ast.QType.fromName(self.tokenizer.buffer[type_token.start..type_token.end + 1]),
          .name = "",
          .frame_specifier = null,
          .parameter_list = Ast.NodeList.init(0, self.nodes),
        };
        try self.parseFunctionParameters(&method_decl.parameter_list, err);

        const identifier_token = try self.tokenizer.next(err);
        try self.checkToken(identifier_token, Token.Tag.identifier, err);
        method_decl.name = self.tokenizer.buffer[identifier_token.start..identifier_token.end + 1];

        const sc_token = try self.tokenizer.next(err);
        try self.checkToken(sc_token, Token.Tag.semicolon, err);

        return  self.insertNode(Ast.Payload{ .method_decl = method_decl });
      },
      else => return makeError(ParseError.UnexpectedInput,
        getLocation(self.tokenizer.buffer, next_token.start), err,
        "expecting identifier or '(', found {}", .{ next_token }),
    }
  }

  fn parsePrimary(self: *Self, err: *GenericError) ParseError!Ast.Node.Index {
    const token = try self.tokenizer.next(err);
    switch (token.tag) {
      Token.Tag.float_literal => {
        const value = try std.fmt.parseFloat(f32, self.tokenizer.buffer[token.start..token.end + 1]);
        // Check if there is a type suffix i.e. 10.0f
        const type_suffix = try self.tokenizer.peek(err);
        if (type_suffix.tag == Token.Tag.identifier and self.tokenizer.buffer[type_suffix.start] == 'f') {
          // just ignore it
          _ = try self.tokenizer.next(err);
        }
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .float_literal = Ast.FloatLiteral{ .value = value } },
        });
      },
      Token.Tag.string_literal => {
        const value = self.tokenizer.buffer[token.start..token.end + 1];
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .string_literal = Ast.StringLiteral{ .value = value } },
        });
      },
      Token.Tag.vector_literal => {
        const vectorLiteralStr = self.tokenizer.buffer[token.start..token.end + 1];
        const value = Ast.VectorLiteral.fromString(vectorLiteralStr) catch |e| {
          return makeError(e, getLocation(self.tokenizer.buffer, token.start), err,
            "incorrect vector literal", .{});
        };
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .vector_literal = value,
        } });
      },
      Token.Tag.identifier => {
        const name = self.tokenizer.buffer[token.start..token.end + 1];
        // Variables
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .identifier = Ast.Identifier{ .name = name } },
        });
      },
      // Parenthesized expression
      Token.Tag.l_paren => {
        const expression = try self.parseExpression(err);
        // Check that the parenthesized expression ends with a matching parenthesis
        if ((try self.tokenizer.peek(err)).tag != Token.Tag.r_paren) {
          return makeError(ParseError.EmptySource, getLocation(self.tokenizer.buffer, token.start), err,
            "unexpected token {}", .{ token });
        }
        _ = try self.tokenizer.next(err);

        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .parenthesized_expression = expression,
        }});
      },
      // Frame identifier
      Token.Tag.frame_identifier => {
        _ = try self.tokenizer.next(err);
        const name = self.tokenizer.buffer[token.start..token.end + 1];
        // Variables
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .frame_identifier = Ast.Identifier{ .name = name } },
        });
      },
      else => return makeError(ParseError.MissingMatchingParenthesis,
        getLocation(self.tokenizer.buffer, token.start), err, "unexpected token {}", .{ token.tag }),
    }
  }

  fn getIndentifierName(self: Self, err: *GenericError, node: Ast.Node, token: Token) ![]const u8 {
    return switch (node.payload) {
      .expression => |e| switch (e) {
        .identifier => |i| i.name,
        else => makeError(ParseError.MissingMatchingParenthesis,
          getLocation(self.tokenizer.buffer, token.start), err, "expecting an identifier", .{}),
      },
      else => makeError(ParseError.MissingMatchingParenthesis,
        getLocation(self.tokenizer.buffer, token.start), err, "expecting an identifier", .{}),
    };
  }

  fn parsePostfix(self: *Self, err: *GenericError) ParseError!Ast.Node.Index {
    var primary = try self.parsePrimary(err);

    while (true) {
      const next = try self.tokenizer.peek(err);
      switch (next.tag) {
        // Function call
        Token.Tag.l_paren => {
          // pop l_paren
          _ = try self.tokenizer.next(err);
          // Check we got an identifier back
          var fn_call = Ast.FnCall{
            // For now we suppose that we cannot call an expression, only an identifier
            .callee = primary,
            .argument_list = Ast.NodeList.init(0, self.nodes),
          };
          var r_paren = try self.tokenizer.peek(err);
          // Parse the parameter declaration until we reach a r_paren
          while (r_paren.tag != Token.Tag.r_paren) {
            fn_call.argument_list.appendNode(try self.parseExpression(err));
            r_paren = try self.tokenizer.peek(err);
            // Ignore the comma but expect it
            if (r_paren.tag == Token.Tag.comma) _ = try self.tokenizer.next(err);
          }
          // Discard r_paren
          _ = try self.tokenizer.next(err);
          primary = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{
            .fn_call = fn_call,
          }});
        },
        // Subscript expression (array[i])
        Token.Tag.l_bracket => {
          // pop l_bracket
          _ = try self.tokenizer.next(err);
          const subscriptExpr = Ast.Subscript{
            // For now we suppose that we cannot subscript an expression, only an identifier
            .name = try self.getIndentifierName(err, self.nodes[primary], next),
            .index_expression = try self.parseExpression(err),
          };
          const r_bracket = try self.tokenizer.next(err);
          try self.checkToken(r_bracket, Token.Tag.r_bracket, err);
          primary = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{
            .subscript_expression = subscriptExpr,
          }});
        },
        // Field access (foo.bar)
        Token.Tag.dot => {
          _ = try self.tokenizer.next(err);
          const field = try self.parsePrimary(err);
          primary = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{
            .field_expression = Ast.FieldExpression{
              .object = primary,
              .field = try self.getIndentifierName(err, self.nodes[field], next),
            },
          }});
        },
        else => break,
      }
    }

    return primary;
  }

  fn parsePower(self: *Self, err: *GenericError) ParseError!Ast.Node.Index {
    const primary = try parsePostfix(self, err);

    const power_token = try self.tokenizer.peek(err);
    if (power_token.tag == Token.Tag.power) {
      _ = try self.tokenizer.next(err);
      return try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .binary_op = Ast.BinaryOp {
        .operator = Ast.Operator.power,
        .lhs = primary,
        .rhs = try self.parsePower(err),
      }}});
    }

    return primary;
  }

  fn parseFactor(self: *Self, err: *GenericError) ParseError!Ast.Node.Index {
    const next = try self.tokenizer.peek(err);
    // Unary
    if (next.tag == Token.Tag.minus or next.tag == Token.Tag.plus or next.tag == Token.Tag.tilde
      or next.tag == Token.Tag.not) {
      _ = try self.tokenizer.next(err);

      const operand = try self.parseExpression(err);
      return try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .unary_op = Ast.UnaryOp {
        .operator = try Ast.Operator.fromToken(next),
        .operand = operand,
      }}});
    }

    const primary = try parsePower(self, err);

    // Update expression (i++, i--)
    const postnext = try self.tokenizer.peek(err);
    if (postnext.tag == Token.Tag.plus or postnext.tag == Token.Tag.minus) {
      const postnextnext = try self.tokenizer.peekAt(err, 2);
      if (postnext.tag == postnextnext.tag) {
        _ = try self.tokenizer.next(err);
        _ = try self.tokenizer.next(err);
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .update_expression = Ast.UpdateExpression{
            .expression = primary,
            .operator = try Ast.Operator.fromToken(postnext),
          },
        }});
      }
    }

    return primary;
  }

  fn parseTerm(self: *Self, err: *GenericError) !Ast.Node.Index {
    var lhs = try self.parseFactor(err);

    var peek_token = try self.tokenizer.peek(err);
    while (peek_token.tag == Token.Tag.mul or peek_token.tag == Token.Tag.slash
      or peek_token.tag == Token.Tag.modulo) {
      _ = try self.tokenizer.next(err);

      const rhs = try self.parseFactor(err);
      lhs = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .binary_op = Ast.BinaryOp {
        .operator = try Ast.Operator.fromToken(peek_token),
        .lhs = lhs,
        .rhs = rhs,
      }}});
      peek_token = try self.tokenizer.peek(err);
    }

    return lhs;
  }

  fn parseQuantity(self: *Self, err: *GenericError) !Ast.Node.Index {
    var lhs = try self.parseTerm(err);

    var peek_token = try self.tokenizer.peek(err);
    while (peek_token.tag == Token.Tag.plus or peek_token.tag == Token.Tag.minus) {
      _ = try self.tokenizer.next(err);

      const rhs = try self.parseTerm(err);
      lhs = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .binary_op = Ast.BinaryOp {
        .operator = try Ast.Operator.fromToken(peek_token),
        .lhs = lhs,
        .rhs = rhs,
      }}});
      peek_token = try self.tokenizer.peek(err);
    }

    return lhs;
  }

  fn parsePredicate(self: *Self, err: *GenericError) !Ast.Node.Index {
    var lhs = try self.parseQuantity(err);

    var peek_token = try self.tokenizer.peek(err);
    while (peek_token.tag == Token.Tag.less_than or peek_token.tag == Token.Tag.less_or_equal or
      peek_token.tag == Token.Tag.greater_than or peek_token.tag == Token.Tag.greater_or_equal or
      peek_token.tag == Token.Tag.double_equal or peek_token.tag == Token.Tag.not_equal or
      peek_token.tag == Token.Tag.spaceship) {
      _ = try self.tokenizer.next(err);

      const rhs = try self.parseQuantity(err);
      lhs = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .binary_op = Ast.BinaryOp {
        .operator = try Ast.Operator.fromToken(peek_token),
        .lhs = lhs,
        .rhs = rhs,
      }}});
      peek_token = try self.tokenizer.peek(err);
    }

    return lhs;
  }

  fn parseValue(self: *Self, err: *GenericError) !Ast.Node.Index {
    var lhs = try self.parsePredicate(err);

    var peek_token = try self.tokenizer.peek(err);
    while (peek_token.tag == Token.Tag.and_ or peek_token.tag == Token.Tag.or_) {
      _ = try self.tokenizer.next(err);

      const rhs = try self.parsePredicate(err);
      lhs = try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .binary_op = Ast.BinaryOp {
        .operator = try Ast.Operator.fromToken(peek_token),
        .lhs = lhs,
        .rhs = rhs,
      }}});
      peek_token = try self.tokenizer.peek(err);
    }

    return lhs;
  }

  fn parseTernary(self: *Self, err: *GenericError) ParseError!Ast.Node.Index {
    const value = try self.parseValue(err);

    const operator_token = try self.tokenizer.peek(err);
    if (operator_token.tag == Token.Tag.question) {
      _ = try self.tokenizer.next(err);
      const true_clause = try self.parseExpression(err);
      const colon_token = try self.tokenizer.next(err);
      try self.checkToken(colon_token, Token.Tag.colon, err);
      return try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .ternary = Ast.Ternary {
        .condition = value,
        .true_clause = true_clause,
        .false_clause = try self.parseExpression(err),
      }}});
    }

    return value;
  }

  // Forward Declarations for Parsing Functions (Grammar Hierarchy)
  // expression ::= ternary ( ('=' | '+=' | '-=' | '*=' | '/=' | '|=' | '&=' | '&~=' | '%=' | '^=') expression)*
  // ternary    ::= value ( '?' expression ':' ternary )? // Can't have an assignment in the false clause.
  // value      ::= predicate ( ( '||' | '&&' ) predicate )*
  // predicate  ::= quantity ( ( '<' | '<=' | '>' | '>=' | '==' | '!=' ) quantity )*
  // quantity   ::= term ( ( '+' | '-' ) term )*
  // term       ::= factor ( ( '*' | '/' | '%' ) factor )*
  // factor     ::= '-' factor | power      // Unary minus applied here
  // power      ::= primary ( '^' power )?   // Exponentiation (right-assoc) applied here
  // postfix    ::= power ( '.' expression | '[' expression ']' | '(' argument_list ')' )*
  // primary    ::= NUMBER | IDENTIFIER | '(' expression ')'
  fn parseExpression(self: *Self, err: *GenericError) !Ast.Node.Index {
    const value = try self.parseTernary(err);

    const operator_token = try self.tokenizer.peek(err);
    if (
      operator_token.tag == Token.Tag.equal or
      operator_token.tag == Token.Tag.plus_equal or
      operator_token.tag == Token.Tag.minus_equal or
      operator_token.tag == Token.Tag.mul_equal or
      operator_token.tag == Token.Tag.slash_equal or
      operator_token.tag == Token.Tag.or_equal or
      operator_token.tag == Token.Tag.and_equal or
      operator_token.tag == Token.Tag.and_tilde_equal or
      operator_token.tag == Token.Tag.modulo_equal or
      operator_token.tag == Token.Tag.caret_equal
    ) {
      _ = try self.tokenizer.next(err);
      return try self.insertNode(Ast.Payload{ .expression = Ast.Expression{ .assignment = Ast.Assignment {
        .identifier = value,
        .operator = try Ast.Operator.fromToken(operator_token),
        .value = try self.parseExpression(err),
      }}});
    }
    return value;
  }

  // Parsing of function parameters used at two different places:
  // C-style function definition.
  // QuakeC-style function definition.
  fn parseFunctionParameters(self: *Self, parameter_list: *Ast.NodeList, err: *GenericError) !void {
    // pop l_paren
    _ = try self.tokenizer.next(err);

    var r_paren = try self.tokenizer.peek(err);
    // Parse the parameter declaration until we reach a r_paren
    while (r_paren.tag != Token.Tag.r_paren) {
      parameter_list.appendNode(try self.parseParamDeclaration(err));
      r_paren = try self.tokenizer.peek(err);
      // Ignore the comma but expect it
      if (r_paren.tag == Token.Tag.comma) _ = try self.tokenizer.next(err);
    }
    // Pop r_paren and expect the name of the function
    _ = try self.tokenizer.next(err);
  }

  fn parseFunctionDefinition(self: *Self, typeToken: Token, err: *GenericError) !Ast.Node.Index {
    // Retrieve the return type
    const atype = Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]) catch |e| {
      return makeError(e, getLocation(self.tokenizer.buffer, typeToken.start), err,
        "Expecting a type got {s}", .{ self.tokenizer.buffer[typeToken.start..typeToken.end + 1] });
    };
    // Create a temporary
    var fn_decl = Ast.FnDecl{
      .return_type = atype,
      .name = "",
      .frame_specifier = null,
      .parameter_list = Ast.NodeList.init(0, self.nodes),
      .body = 0,
    };
    // Peek at the next token
    var l_paren = try self.tokenizer.peek(err);
    if (l_paren.tag == Token.Tag.l_paren) {
      // This is a QuakeC-style funcion definition with the parameters before the
      // function identifier.
      try self.parseFunctionParameters(&fn_decl.parameter_list, err);
    }
    const identifier_token = try self.tokenizer.next(err);
    if (identifier_token.tag != Token.Tag.identifier) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifier_token.start), err,
        "expecting function name found {}", .{ identifier_token });
    }
    fn_decl.name = self.tokenizer.buffer[identifier_token.start..identifier_token.end + 1];
    // Deal with weirderies of having equal or not or having parameters after the identifier
    var body_token = try self.tokenizer.peek(err);
    switch_start: switch (body_token.tag) {
      Token.Tag.semicolon => {
        // We are dealing with a function declaration
        return self.insertNode(Ast.Payload{ .fn_decl = fn_decl });
      },
      Token.Tag.equal => {
        // To support C-style function definition, check if we have equal and pop it
        _ = try self.tokenizer.next(err);
        continue :switch_start (try self.tokenizer.peek(err)).tag;
      },
      Token.Tag.l_paren => {
        // This is a C-style funcion definition with the parameters before the
        // function identifier.
        l_paren = try self.tokenizer.peek(err);
        try self.checkToken(l_paren, Token.Tag.l_paren, err);
        try self.parseFunctionParameters(&fn_decl.parameter_list, err);
      },
      else => {},
    }
    // Parse the potential frame specifier
    const functionContentToken = try self.tokenizer.peek(err);
    if (functionContentToken.tag == Token.Tag.l_bracket) {
      // Frame specifier
      _ = try self.tokenizer.next(err);
      const frame_identifier = b: {
        const frame_identifier_token = try self.tokenizer.next(err);
        try self.checkToken(frame_identifier_token, Token.Tag.frame_identifier, err);
        const name = self.tokenizer.buffer[frame_identifier_token.start..frame_identifier_token.end + 1];
        break :b Ast.Identifier{ .name = name };
      };
      {
        const comma_token = try self.tokenizer.next(err);
        try self.checkToken(comma_token, Token.Tag.comma, err);
      }
      const next_function = b: {
        const next_function_token = try self.tokenizer.next(err);
        try self.checkToken(next_function_token, Token.Tag.identifier, err);
        const name = self.tokenizer.buffer[next_function_token.start..next_function_token.end + 1];
        break :b Ast.Identifier{ .name = name };
      };
      const r_bracket_token = try self.tokenizer.next(err);
      try self.checkToken(r_bracket_token, Token.Tag.r_bracket, err);
      fn_decl.frame_specifier = Ast.FrameSpecifier{
        .frame_identifier = frame_identifier,
        .next_function = next_function,
      };
    }
    // Now parse the body
    body_token = try self.tokenizer.peek(err);
    switch (body_token.tag) {
      Token.Tag.builtin_literal => {
        // This is a builtin declaration
        const builtinImmediateToken = try self.tokenizer.next(err);
        const bl = self.tokenizer.buffer[builtinImmediateToken.start + 1..builtinImmediateToken.end + 1];
        const bDecl = Ast.BuiltinDecl{
          .return_type = atype,
          .name = fn_decl.name,
          .parameter_list = fn_decl.parameter_list,
          .index = try std.fmt.parseInt(u16, bl, 10),
        };
        return self.insertNode(Ast.Payload{ .builtin_decl = bDecl });
      },
      Token.Tag.l_brace => {
        // This is a statement list
        // We don't pop l_brace here to let parseStatements knows if its a multi or mono
        // statement situation
        fn_decl.body = try self.insertNode(Ast.Payload{ .body = Ast.Body{
          .statement_list = try self.parseStatements(err),
        } });
        return self.insertNode(Ast.Payload{ .fn_decl = fn_decl });
      },
      else => {
        // This is an expression
        const expression = try self.parseExpression(err);
        fn_decl.body = try self.insertNode(Ast.Payload{ .body = Ast.Body{
          .statement_list = Ast.NodeList.init(expression, self.nodes)
        } });
        return self.insertNode(Ast.Payload{ .fn_decl = fn_decl });
      }
    }

    return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, body_token.start), err,
      "expecting '=' or ';', got {}", .{ body_token.tag });
  }

  fn parseStatements(self: *Self, err: *GenericError) !Ast.NodeList {
    var node_list = Ast.NodeList.init(0, self.nodes);
    const l_brace = try self.tokenizer.peek(err);
    if (l_brace.tag != Token.Tag.l_brace) {
      // There is only one statment (no braces)
      try self.parseStatement(&node_list, err);
      return node_list;
    }
    // pop l_brace
    _ = try self.tokenizer.next(err);
    var r_brace = try self.tokenizer.peek(err);
    while (r_brace.tag != Token.Tag.r_brace) {
      try self.parseStatement(&node_list, err);
      r_brace = try self.tokenizer.peek(err);
    }
    // pop r_brace
    _ = try self.tokenizer.next(err);
    return node_list;
  }

  fn parseStatement(self: *Self, statement_list: *Ast.NodeList, err: *GenericError) ParseError!void {
    const first_token = try self.tokenizer.peek(err);

    switch (first_token.tag) {
      .kw_local => {
        _ = try self.tokenizer.next(err);
        try self.parseVariableDefinition(statement_list, try self.tokenizer.next(err), .local, err);
        const scToken = try self.tokenizer.next(err);
        try self.checkToken(scToken, Token.Tag.semicolon, err);
      },
      .kw_return => {
        _ = try self.tokenizer.next(err);
        const expression = try self.parseExpression(err);
        statement_list.appendNode(try self.insertNode(Ast.Payload{ .statement = Ast.Statement{
          .return_statement = expression,
        }}));
        const scToken = try self.tokenizer.next(err);
        try self.checkToken(scToken, Token.Tag.semicolon, err);
      },
      .kw_if => {
        _ = try self.tokenizer.next(err);
        const not_token = try self.tokenizer.peek(err);
        const negated = not_token.tag == Token.Tag.kw_not;
        if (negated) {
          _ = try self.tokenizer.next(err);
        }
        const l_paren_token = try self.tokenizer.next(err);
        try self.checkToken(l_paren_token, Token.Tag.l_paren, err);
        const condition = try self.parseExpression(err);
        const r_paren_token = try self.tokenizer.next(err);
        try self.checkToken(r_paren_token, Token.Tag.r_paren, err);

        var if_statement = Ast.IfStatement{
          .negated = negated,
          .condition = condition,
          .statement = try self.insertNode(Ast.Payload{ .body = Ast.Body{
            .statement_list = try self.parseStatements(err),
          } }),
          .else_statement = null,
        };
        const else_token = try self.tokenizer.peek(err);
        if (else_token.tag == Token.Tag.kw_else) {
          _ = try self.tokenizer.next(err);
          if_statement.else_statement = try self.insertNode(Ast.Payload{ .body = Ast.Body{
            .statement_list = try self.parseStatements(err),
          } });
        }
        statement_list.appendNode(try self.insertNode(Ast.Payload{ .statement = Ast.Statement{
          .if_statement = if_statement,
        }}));
      },
      .kw_while => {
        _ = try self.tokenizer.next(err);
        const l_paren_token = try self.tokenizer.next(err);
        try self.checkToken(l_paren_token, Token.Tag.l_paren, err);
        const condition = try self.parseExpression(err);
        const r_paren_token = try self.tokenizer.next(err);
        try self.checkToken(r_paren_token, Token.Tag.r_paren, err);

        const while_statement = Ast.WhileStatement{
          .condition = condition,
          .statement = try self.insertNode(Ast.Payload{ .body = Ast.Body{
            .statement_list = try self.parseStatements(err),
          } }),
        };

        statement_list.appendNode(try self.insertNode(Ast.Payload{ .statement = Ast.Statement{
          .while_statement = while_statement,
        }}));
      },
      .kw_do => {
        _ = try self.tokenizer.next(err);

        const statement = try self.insertNode(Ast.Payload{ .body = Ast.Body{
          .statement_list = try self.parseStatements(err),
        } });

        const while_token = try self.tokenizer.next(err);
        try self.checkToken(while_token, Token.Tag.kw_while, err);
        const l_paren_token = try self.tokenizer.next(err);
        try self.checkToken(l_paren_token, Token.Tag.l_paren, err);
        const condition = try self.parseExpression(err);
        const r_paren_token = try self.tokenizer.next(err);
        try self.checkToken(r_paren_token, Token.Tag.r_paren, err);

        const do_while_statement = Ast.WhileStatement{
          .condition = condition,
          .statement = statement,
        };
        statement_list.appendNode(try self.insertNode(Ast.Payload{ .statement = Ast.Statement{
          .do_while_statement = do_while_statement,
        }}));
      },
      .kw_for => {
        // Not part of the original grammar but seems to be supported by other compiler
        _ = try self.tokenizer.next(err);
        const l_paren_token = try self.tokenizer.next(err);
        try self.checkToken(l_paren_token, Token.Tag.l_paren, err);

        const initializer: ?Ast.Node.Index = b: {
          var sc_token = try self.tokenizer.peek(err);
          if (sc_token.tag != Token.Tag.semicolon) {
            const expression = try self.parseExpression(err);
            sc_token = try self.tokenizer.next(err);
            try self.checkToken(sc_token, Token.Tag.semicolon, err);
            break :b expression;
          } else {
            _ = try self.tokenizer.next(err);
            break :b null;
          }
        };

        const condition: Ast.Node.Index = try self.parseExpression(err);
        {
          const sc_token = try self.tokenizer.next(err);
          try self.checkToken(sc_token, Token.Tag.semicolon, err);
        }

        const loopexpr: ?Ast.Node.Index = b: {
          var r_paren_token = try self.tokenizer.peek(err);
          if (r_paren_token.tag != Token.Tag.r_paren) {
            const expression = try self.parseExpression(err);
            r_paren_token = try self.tokenizer.next(err);
            try self.checkToken(r_paren_token, Token.Tag.r_paren, err);
            break :b expression;
          } else {
            _ = try self.tokenizer.next(err);
            break :b null;
          }
        };

        const statement = try self.insertNode(Ast.Payload{ .body = Ast.Body{
          .statement_list = try self.parseStatements(err),
        }});

        const for_statement = Ast.ForStatement{
          .initializer = initializer,
          .condition = condition,
          .loopexpr = loopexpr,
          .statement = statement,
        };
        statement_list.appendNode(try self.insertNode(Ast.Payload{ .statement = Ast.Statement{
          .for_statement = for_statement,
        }}));
      },
      .comment => _ = try self.tokenizer.next(err), // ignore comments
      else => {
        // If the first token is a type, it is a global variable declaration
        const isError = Ast.QType.fromName(self.tokenizer.buffer[first_token.start..first_token.end + 1]);
        if (isError == error.NotAType) {
          // Otherwise it is an expression
          const expression = try self.parseExpression(err);
          statement_list.appendNode(try self.insertNode(Ast.Payload{ .statement = Ast.Statement{
            .expression = expression,
          }}));
        } else {
          _ = try self.tokenizer.next(err);
          try self.parseVariableDefinition(statement_list, first_token, .nonlocal, err);
        }
        const scToken = try self.tokenizer.next(err);
        try self.checkToken(scToken, Token.Tag.semicolon, err);
      }
    }
  }

  fn parseParamDeclaration(self: *Self, err: *GenericError) !Ast.Node.Index {
    const typeToken = try self.tokenizer.next(err);
    if (typeToken.tag == Token.Tag.elipsis) {
      return self.insertNode(Ast.Payload{ .param_decl = Ast.ParamDecl{
        .param = .elipsis,
      } });
    }
    const atype = Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]) catch |e| {
      return makeError(e, getLocation(self.tokenizer.buffer, typeToken.start), err,
        "Expecting a type got {s}", .{ self.tokenizer.buffer[typeToken.start..typeToken.end + 1] });
    };
    const identifierToken = try self.tokenizer.next(err);
    if (identifierToken.tag != Token.Tag.identifier) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting identifier found {}", .{ identifierToken });
    }
    const name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1];
    return self.insertNode(Ast.Payload{ .param_decl = Ast.ParamDecl{
      .param = .{
        .declaration = .{ .type = atype, .name = name },
      }
    } });
  }
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
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("float f = 3.14;", &.{
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.float_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("f += 2;", &.{
    Token.Tag.identifier,
    Token.Tag.plus_equal,
    Token.Tag.float_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("f /= 2;", &.{
    Token.Tag.identifier,
    Token.Tag.slash_equal,
    Token.Tag.float_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("f &~= 2;", &.{
    Token.Tag.identifier,
    Token.Tag.and_tilde_equal,
    Token.Tag.float_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("string s = \"pi\";", &.{
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.string_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("vector v = '+0.5 -1 -.2';", &.{
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.vector_literal,
    Token.Tag.semicolon,
    Token.Tag.eof,
  }, &err);
  try testTokenize("void (float f, vector v) foo = {}", &.{
    Token.Tag.type,
    Token.Tag.l_paren,
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.comma,
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.l_brace,
    Token.Tag.r_brace,
    Token.Tag.eof,
  }, &err);
  try testTokenize("void\t(string str, ...)\tprint = #99;", &.{
    Token.Tag.type,
    Token.Tag.l_paren,
    Token.Tag.type,
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
    Token.Tag.type,
    Token.Tag.l_paren,
    Token.Tag.r_paren,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.l_brace,
    Token.Tag.type,
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
    Token.Tag.r_brace,
    Token.Tag.eof,
  }, &err);

  const multiline_comments =
    \\ void () main = {
    \\   /* Some
    \\      comment */
    \\   string foo = "foo";
    \\   printf(foo);
    \\ }
  ;
  try testTokenize(multiline_comments, &.{
    Token.Tag.type,
    Token.Tag.l_paren,
    Token.Tag.r_paren,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.l_brace,
    Token.Tag.comment,
    Token.Tag.type,
    Token.Tag.identifier,
    Token.Tag.equal,
    Token.Tag.string_literal,
    Token.Tag.semicolon,
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.semicolon,
    Token.Tag.r_brace,
    Token.Tag.eof,
  }, &err);

  const conditions =
    \\if ("foo" == "foo" && 1 != 2 || false && true) {
    \\  printf(foo);
    \\}
  ;
  try testTokenize(conditions, &.{
    Token.Tag.kw_if,
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
    Token.Tag.l_brace,
    Token.Tag.identifier,
    Token.Tag.l_paren,
    Token.Tag.identifier,
    Token.Tag.r_paren,
    Token.Tag.semicolon,
    Token.Tag.r_brace,
    Token.Tag.eof,
  }, &err);
}

fn expectEqualString(lhs: []const u8, rhs: []const u8) !void {
  if (std.mem.eql(u8, lhs, rhs)) {
    return;
  } else {
    std.log.err("\n\"{s}\" ({}) \n not equal to\n\"{s}\" ({})", .{ lhs, lhs.len, rhs, rhs.len });
    return error.Fail;
  }
}

fn testParseWithOutput(source: [:0]const u8, expected_outcome: []const u8, err: *GenericError) !void {
  var ast: [256]Ast.Node = undefined;
  var output: [4096:0]u8 = .{ 0 } ** 4096;
  var parser = Parser.init(source, ast[0..]);
  parser.parse(err) catch |e| {
    const n = try err.prettyPrint(&output);
    std.log.debug("{s}", .{ output[0..n] });
    return e;
  };
  _ = try ast[0].prettyPrint(&ast, &output);
  if (std.testing.log_level == .debug) {
    var debug_str: [4096:0]u8 = .{ 0 } ** 4096;
    const debug_written = try ast[0].debugPrint(&ast, &debug_str, 0);
    std.log.debug("ast:\n{s}", .{ debug_str[0..debug_written] });
  }
  try expectEqualString(expected_outcome, std.mem.span(@as([*:0]const u8, @ptrCast(&output))));
}

fn testParse(comptime source: [:0]const u8, err: *GenericError) !void {
  return testParseWithOutput(source, source, err);
}

fn testExpression(source: [:0]const u8, err: *GenericError) !void {
  var ast: [256]Ast.Node = undefined;
  var output: [4096:0]u8 = .{ 0 } ** 4096;
  var parser = Parser.init(source, ast[0..]);
  const expr = parser.parseExpression(err) catch |e| {
    const n = try err.prettyPrint(&output);
    std.log.debug("{s}", .{ output[0..n] });
    return e;
  };
  _ = try ast[expr].prettyPrint(&ast, &output);
  if (std.testing.log_level == .debug) {
    var debug_str: [4096:0]u8 = .{ 0 } ** 4096;
    const debug_written = try ast[expr].debugPrint(&ast, &debug_str, 0);
    std.log.debug("expr:\n{s}", .{ debug_str[0..debug_written] });
  }
  try expectEqualString(source, std.mem.span(@as([*:0]const u8, @ptrCast(&output))));
}

test "parser test" {
  // std.testing.log_level = .debug;

  var err = GenericError{};
  try testParse("float f = 3.14;", &err);
  try testParseWithOutput("float f = 3.14f;", "float f = 3.14;", &err);
  try testParse("float f;", &err);
  // Grouped variable declarations
  try testParseWithOutput("float a, b;",
    \\float a;
    \\float b;
    , &err);
  try testParseWithOutput("float a = 2, b;",
    \\float a = 2;
    \\float b;
    , &err);
  try testParseWithOutput("float a, b = 2;",
    \\float a;
    \\float b = 2;
    , &err);
  try testParse("string s = \"foo\";", &err);
  try testParse("vector v = '1 2 3';", &err);
  try testParse(".vector vectorField;", &err);
  try testParse("void () foo;", &err);
  try testParse("void () foo = #42;", &err);
  try testParse(".void () foo;", &err);
  try testParse("void (string s) foo;", &err);
  try testParse("void (string s, ...) foo;", &err);
  try testParse("void () main = {};", &err);
  try testParse("void (float f, vector v) main = {};", &err);
  try testParse("void (float f, vector v) main = {};", &err);
  try testParse("void (string str, ...) print = #99;", &err);
  try testParse("void () enf_die1 = [$death1, enf_die2] {};", &err);
  try testParseWithOutput(
    \\float () foo = {
    \\  // Some comment
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\};
    ,
    \\float () foo = {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\};
    , &err);
  // comment as the last statement in a function body
  try testParseWithOutput(
    \\void () main = {
    \\  print(ftos(__builtin_exp(hundy)), "\n"); // prints: 22026.465
    \\};
    ,
    \\void () main = {
    \\  print(ftos(__builtin_exp(hundy)), "\n");
    \\};
    , &err);
  // extended comment
  try testParseWithOutput(
    \\float () foo = {
    \\  /* Ignore this
    \\     comment */
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\};
    ,
    \\float () foo = {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\};
    , &err);

  try testParse(
    \\float () foo = {
    \\  local float a = 3.14;
    \\  a *= a;
    \\  return a;
    \\};
    , &err);

  // Support missing equal sign
  try testParseWithOutput(
    \\float () foo {
    \\  local float a = 3.14;
    \\  a *= a;
    \\  return a;
    \\};
    ,
    \\float () foo = {
    \\  local float a = 3.14;
    \\  a *= a;
    \\  return a;
    \\};
    , &err);

  // Support for C like function definition
  try testParseWithOutput(
    \\float foo() {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\}
    ,
    \\float () foo = {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\};
    , &err);

  // Even with QuakeC-style function definition the semicolon is optional
  try testParseWithOutput(
    \\float () foo = {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\}
    ,
    \\float () foo = {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\};
    , &err);

  try testParse(
    \\float (float numerator, float denominator) div = {
    \\  if (denominator != 0) {
    \\    return numerator / denominator;
    \\  } else {
    \\    error("denominator is zero");
    \\  }
    \\};
    , &err);

  // Hexen's not operator
  try testParse(
    \\float (float numerator, float denominator) div = {
    \\  if not (denominator != 0) {
    \\    return numerator / denominator;
    \\  } else {
    \\    error("denominator is zero");
    \\  }
    \\};
    , &err);

  try testParseWithOutput(
    \\float (float n) main = {
    \\  float pi = 0;
    \\  float i = 0;
    \\  while (i < n) {
    \\    if (i % 2 == 0)
    \\      pi += 1.0 / (2 * i + 1);
    \\    else
    \\      pi -= 1.0 / (2 * i + 1);
    \\    i++;
    \\  }
    \\
    \\  pi *= 4;
    \\
    \\  print("Approximation of pi: %.15f\n", pi);
    \\  return pi;
    \\};
    ,
    \\float (float n) main = {
    \\  float pi = 0;
    \\  float i = 0;
    \\  while (i < n) {
    \\    if (i % 2 == 0) {
    \\      pi += 1 / (2 * i + 1);
    \\    } else {
    \\      pi -= 1 / (2 * i + 1);
    \\    }
    \\    i++;
    \\  }
    \\  pi *= 4;
    \\  print("Approximation of pi: %.15f\n", pi);
    \\  return pi;
    \\};
    , &err);

  try testParseWithOutput(
    \\float (float n) main = {
    \\  float pi = 0;
    \\  float i = 0;
    \\  do {
    \\    if (i % 2 == 0)
    \\      pi += 1.0 / (2 * i + 1);
    \\    else
    \\      pi -= 1.0 / (2 * i + 1);
    \\    i++;
    \\  } while (i < n)
    \\
    \\  pi *= 4;
    \\
    \\  print("Approximation of pi: %.15f\n", pi);
    \\  return pi;
    \\};
    ,
    \\float (float n) main = {
    \\  float pi = 0;
    \\  float i = 0;
    \\  do {
    \\    if (i % 2 == 0) {
    \\      pi += 1 / (2 * i + 1);
    \\    } else {
    \\      pi -= 1 / (2 * i + 1);
    \\    }
    \\    i++;
    \\  } while (i < n)
    \\  pi *= 4;
    \\  print("Approximation of pi: %.15f\n", pi);
    \\  return pi;
    \\};
    , &err);

    try testParse(
      \\float () bar = {
      \\  for (i = 0; i < 10; i += 1) {
      \\    print("%i\n", i);
      \\  }
      \\};
      , &err);

    try testParse(
      \\float () bar = {
      \\  for (; i < 10; i += 1) {
      \\    print("%i\n", i);
      \\  }
      \\};
      , &err);

    try testParse(
      \\float () bar = {
      \\  for (i = 0; i < 10; ) {
      \\    print("%i\n", i);
      \\  }
      \\};
      , &err);

    try testParse(
      \\float () bar = {
      \\  for (; i < 10; ) {
      \\    print("%i\n", i);
      \\  }
      \\};
      , &err);
}

test "expression parser test" {
  // std.testing.log_level = .debug;

  var err = GenericError{};
  try testExpression("12", &err);
  try testExpression("12 + 7", &err);
  try testExpression("12 - 7", &err);
  try testExpression("12 + 7 - 3", &err);
  try testExpression("12 * 3", &err);
  try testExpression("12 / 3", &err);
  try testExpression("12 % 3", &err);
  try testExpression("12 % 3 * 4", &err);
  try testExpression("12 * 3 + 4", &err);
  try testExpression("12 + 3 * 4", &err);
  try testExpression("-12", &err);
  try testExpression("-12 % -4", &err);
  try testExpression("foo()", &err);
  try testExpression("foo(1)", &err);
  try testExpression("foo(1, 2)", &err);
  try testExpression("1 + foo(1, 2)", &err);
  try testExpression("foo(1, 2) + 1", &err);
  try testExpression("foo(bar(9 % 4), 2 * 3) + 1", &err);
  try testExpression("(3 + 2) * 2", &err);
  try testExpression("3.14 * (radius * radius) - max(x, -y, 10)", &err);
  try testExpression("array[1]", &err);
  try testExpression("array[1 + 2]", &err);
  try testExpression("array[3.14 * -(!radius * radius) - ~max(x, -y, 10)]", &err);
  try testExpression("print(\"hello\")", &err);
  try testExpression("i++", &err);
  try testExpression("i--", &err);
  try testExpression("12 + i++", &err);
  try testExpression("i-- + 12", &err);
  try testExpression("i++ * (12 - 3)", &err);
  try testExpression("i++ * (12 - 3)", &err);
  try testExpression("-i++", &err);
  try testExpression("array[x]++", &err);
  try testExpression("object.field", &err);
  try testExpression("$frame12", &err);
  try testExpression("a < b", &err);
  try testExpression("a > b", &err);
  try testExpression("a >= b", &err);
  try testExpression("a <= b", &err);
  try testExpression("a <=> b", &err);
  try testExpression("a == b", &err);
  try testExpression("a != b", &err);
  try testExpression("a && b", &err);
  try testExpression("a || b", &err);
  try testExpression("!a", &err);
  try testExpression("b || !a", &err);
  try testExpression("1 < 2 && 3 >= 2 || 0", &err);
  try testExpression("a = 23", &err);
  try testExpression("a /= 23", &err);
  try testExpression("a &~= 23", &err);
  try testExpression("a ** b", &err);
  try testExpression("a ** b ** 2", &err);
  try testExpression("a = b += 23", &err);
  try testExpression("a = b = 23 * (3 ** 4 && 9)", &err);
  try testExpression("a.b", &err);
  try testExpression("a.b.c", &err);
  try testExpression("a.b.c()", &err);
  try testExpression("a ? 1 : 2", &err);
  try testExpression("2 = 0 ? b : 2 ** 3", &err);
  try testExpression("1 ? a = 0 ? 2 : 3 : 4", &err);
}

test "peekAt" {
  // std.testing.log_level = .debug;

  var err = GenericError{};
  {
    var tokenizer = Tokenizer.init(&.{ '+', '-', '%', 0 });
    try std.testing.expectEqual(Token.Tag.minus, (try tokenizer.peekAt(&err, 2)).tag);
    try std.testing.expectEqual(Token.Tag.modulo, (try tokenizer.peekAt(&err, 3)).tag);
    try std.testing.expectEqual(Token.Tag.minus, (try tokenizer.peekAt(&err, 2)).tag);
  }
  {
    var tokenizer = Tokenizer.init(&.{ '+', '-', '%', 0 });
    _ = try tokenizer.peek(&err);
    try std.testing.expectEqual(Token.Tag.minus, (try tokenizer.peekAt(&err, 2)).tag);
    try std.testing.expectEqual(Token.Tag.modulo, (try tokenizer.peekAt(&err, 3)).tag);
  }
}
