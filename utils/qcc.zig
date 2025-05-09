// Compile QuakeC bytecode.
//
// reference:
// https://github.com/vkazanov/tree-sitter-quakec/blob/main/grammar.js
//
// test: clear && zig test -freference-trace qcc.zig
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
    l_brace,
    r_brace,
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
            std.log.warn("expected {c}", .{ c });
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
  const Precedence = enum(i8) {
    ASSIGNMENT  = -2,
    CONDITIONAL = -1,
    DEFAULT     = 0,
    LOGICAL_OR  = 1,
    LOGICAL_AND = 2,
    RELATIONAL  = 3,
    ADD         = 4,                     // same as substraction
    BITWISE_OR  = 5,
    BITWISE_AND = 6,
    MULTIPLY    = 7,                 // same as division
    UNARY       = 8,                    // positive/negative
    CALL        = 9,
    FIELD       = 10,
    SUBSCRIPT   = 11,
  };

  const operator_precedence = std.StaticStringMap(Precedence).init(.{
    .{ "+", Precedence.ADD },
    .{ "-", Precedence.ADD },
    .{ "*", Precedence.MULTIPLY },
    .{ "/", Precedence.MULTIPLY },
    .{ "%", Precedence.MULTIPLY },
    .{ "||", Precedence.LOGICAL_OR },
    .{ "&&", Precedence.LOGICAL_AND },
    .{ "|", Precedence.BITWISE_OR },
    .{ "&", Precedence.BITWISE_AND },
    .{ "==", Precedence.RELATIONAL },
    .{ "!=", Precedence.RELATIONAL },
    .{ ">", Precedence.RELATIONAL },
    .{ ">=", Precedence.RELATIONAL },
    .{ "<=", Precedence.RELATIONAL },
    .{ "<", Precedence.RELATIONAL },
  });

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
    condition: Node.Index,
    statement: NodeList,
    else_statement: NodeList,
  };

  const WhileStatement = struct {
    condition: Node.Index,
    statement: []const Node.Index,
  };

  const StatementType = enum {
    var_decl,
    return_statement,
    if_statement,
    while_statement,
    do_while_statement,
  };

  const Statement = union {
    var_decl: VarDecl,
    return_statement: Expression,
    if_statement: IfStatement,
    while_statement: WhileStatement,
    do_while_statement: WhileStatement,
    expression: Expression,

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

    const Self = @This();
    pub fn prettyPrint(self: Self, nodes: []Node, string: []u8) ParseError!usize {
      switch (self) {
        .statement_list => |list| {
          if (list.len() == 1) {
            return (try list.get(0)).prettyPrint(nodes, string);
          } else {
            var written: usize = 0;
            written += (try std.fmt.bufPrint(string[written..], "{{", .{})).len;
            if (list.len() > 0) {
              written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
            }
            var statementIt = list.iter();
            while (statementIt.next()) |statement| {
              written += try statement.prettyPrint(nodes, string[written..]);
              written += (try std.fmt.bufPrint(string[written..], "\n", .{})).len;
            }
            written += (try std.fmt.bufPrint(string[written..], "}}", .{})).len;
            return written;
          }
        },
        .builtin_immediate => |index| {
          return (try std.fmt.bufPrint(string, "#{}", .{ index })).len;
        },
      }
    }
  };

  const FnDecl = struct {
    return_type: QType,
    name: []const u8,
    parameter_list: NodeList,
    body: Node.Index,
  };

  const FieldDecl = struct {
    type: QType,
    name: []const u8,
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

  const ExpressionType = enum {
    float_literal,
    string_literal,
    vector_literal,
    identifier,
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

  const Expression = union(ExpressionType) {
    float_literal: FloatLiteral,
    string_literal: StringLiteral,
    vector_literal: VectorLiteral,
    identifier: Identifier,

    pub fn prettyPrint(self: @This(), string: []u8) !usize {
      var written: usize = 0;
      switch (self) {
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
      }
      return written;
    }
  };

  const PayloadType = enum {
    program,
    var_decl,
    fn_decl,
    field_decl,
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
    builtin_decl: BuiltinDecl,
    param_decl: ParamDecl,
    statement: Statement,
    expression: Expression,
    body: Body,
  };

  const Node = struct {
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

    pub fn prettyPrint(self: @This(), nodes: []Node, string: []u8) !usize {
      var written: usize = 0;
      switch (self.payload) {
        .var_decl => |d| {
          written += try d.type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          if (getNode(nodes, d.value)) |value| {
            written += (try std.fmt.bufPrint(string[written..], " = ", .{})).len;
            written += try value.prettyPrint(nodes, string[written..]);
          }
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .fn_decl => |d| {
          written += try d.return_type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          var paramIt = d.parameter_list.iter();
          var i: usize = 0;
          while (paramIt.next()) |param| : (i += 1) {
            written += try param.prettyPrint(nodes, string[written..]);
            if (i < d.parameter_list.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ") {s}", .{ d.name })).len;
          if (getNode(nodes, d.body)) |body| {
            written += (try std.fmt.bufPrint(string[written..], " = ", .{})).len;
            written += try body.prettyPrint(nodes, string[written..]);
          } else {
            written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
          }
        },
        .field_decl => |d| {
          written += (try std.fmt.bufPrint(string[written..], ".", .{})).len;
          written += try d.type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " {s}", .{ d.name })).len;
          written += (try std.fmt.bufPrint(string[written..], ";", .{})).len;
        },
        .builtin_decl => |d| {
          written += try d.return_type.prettyPrint(string[written..]);
          written += (try std.fmt.bufPrint(string[written..], " (", .{})).len;
          var paramIt = d.parameter_list.iter();
          var i: usize = 0;
          while (paramIt.next()) |param| : (i += 1) {
            written += try param.prettyPrint(nodes, string[written..]);
            if (i < d.parameter_list.len() - 1) {
              written += (try std.fmt.bufPrint(string[written..], ", ", .{})).len;
            }
          }
          written += (try std.fmt.bufPrint(string[written..], ") {s} = #{};", .{ d.name, d.index })).len;
        },
        .expression => |expression| {
          written += try expression.prettyPrint(string[written..]);
        },
        .program => |p| {
          var it = p.declarations.iter();
          while (it.next()) |declaration| {
            written += try declaration.prettyPrint(nodes, string[written..]);
          }
        },
        .param_decl => |p| {
          written += try p.prettyPrint(string[written..]);
        },
        .statement => unreachable,
        .body => |b| {
          written += try b.prettyPrint(nodes, string[written..]);
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

  const NodeList = struct {
    const Self = @This();
    head: Node.Index,
    nodes: []Node,

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

const Parser = struct {
  const Self = @This();

  tokenizer: Tokenizer,
  nodes: []Ast.Node,
  // The root node is 0 and will be set manually.
  nb_nodes: Ast.Node.Index = 1,

  pub fn init(buffer: [:0]const u8, nodes: []Ast.Node) Self {
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
  // fn insertNode(self: *Self, node: Ast.Node) !Ast.Node.Index {
  //   if (self.nb_nodes >= self.nodes.len) {
  //     return error.NodeOutOfCapacity;
  //   }
  //   self.nodes[self.nb_nodes] = node;
  //   self.nb_nodes += 1;
  //   return self.nb_nodes - 1;
  // }
  fn insertNode(self: *Self, payload: Ast.Payload) !Ast.Node.Index {
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
        Token.Tag.comment => {}, // ignore
        Token.Tag.type => {
          declarations.appendNode(try self.parseDeclaration(err));
        },
        Token.Tag.dot => { // field decl
          declarations.appendNode(try self.parseFieldDefinition(err));
        },
        Token.Tag.eof => {
          if (declarations.len() == 0) {
            // Do not allow empty source file
            return makeError(ParseError.EmptySource, null, err, "Empty source file", .{});
          }
          self.nodes[0] = Ast.Node.init(Ast.Payload{ .program = Ast.Program{
            .declarations = declarations,
          } });
          return;
        },
        else => |t| return makeError(ParseError.UnexpectedInput, null, err, "Unexpected token {}", .{ t }),
      }
    }
  }

  fn parseDeclaration(self: *Self, err: *GenericError) !Ast.Node.Index {
    const typeToken = try self.tokenizer.next(err);
    const identifierToken = try self.tokenizer.peek(err);
    switch (identifierToken.tag) {
      Token.Tag.identifier => {
        return try self.parseVariableDefinition(typeToken, false, err);
      },
      Token.Tag.l_paren => { // function declaration/definition
        return try self.parseFunctionDefinition(typeToken, err);
      },
      else => |t| return makeError(ParseError.EmptySource,
        getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "Unexpected token {}", .{ t }),
    }
  }

  fn parseVariableDefinition(self: *Self, typeToken: Token, local: bool, err: *GenericError) !Ast.Node.Index {
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
            return self.insertNode(Ast.Payload{ .var_decl = Ast.VarDecl{
              .type = try Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
              .name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1],
              .value = expression,
              .local = local,
            }});
          },
          else => |t| return makeError(ParseError.EmptySource,
            getLocation(self.tokenizer.buffer, identifierToken.start), err,
            "Unexpected token {}", .{ t }),
        }
      },
      Token.Tag.semicolon => {
        return self.insertNode(Ast.Payload{ .var_decl = Ast.VarDecl{
          .type = try Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
          .name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1],
          .value = 0,
          .local = local,
        }});
      },
      else => return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting '=' or ';' found {}", .{ eqlToken }),
    }
  }

  fn parseFieldDefinition(self: *Self, err: *GenericError) !Ast.Node.Index {
    // discard dot
    _ = try self.tokenizer.next(err);
    const typeToken = try self.tokenizer.next(err);
    const identifierToken = try self.tokenizer.next(err);
    if (identifierToken.tag != Token.Tag.identifier) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting identifier found {}", .{ identifierToken });
    }
    const scToken = try self.tokenizer.next(err);
    if (scToken.tag != Token.Tag.semicolon) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting semicolon found {}", .{ identifierToken });
    }
    return self.insertNode(Ast.Payload{ .field_decl = Ast.FieldDecl{
      .type = try Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]),
      .name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1],
    }});
  }

  fn parseExpression(self: *Self, err: *GenericError) !Ast.Node.Index {
    const token = try self.tokenizer.next(err);
    switch (token.tag) {
      Token.Tag.float_literal => {
        const value = try std.fmt.parseFloat(f32, self.tokenizer.buffer[token.start..token.end + 1]);
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
        return self.insertNode(Ast.Payload{ .expression = Ast.Expression{
          .identifier = Ast.Identifier{ .name = name } },
        });
      },
      else => return makeError(ParseError.EmptySource, getLocation(self.tokenizer.buffer, token.start), err,
       "unexpected token {}", .{ token }),
    }
  }

  fn parseFunctionDefinition(self: *Self, typeToken: Token, err: *GenericError) !Ast.Node.Index {
    // Retrieve the return type
    const atype = Ast.QType.fromName(self.tokenizer.buffer[typeToken.start..typeToken.end + 1]) catch |e| {
      return makeError(e, getLocation(self.tokenizer.buffer, typeToken.start), err,
        "Expecting a type got {s}", .{ self.tokenizer.buffer[typeToken.start..typeToken.end + 1] });
    };
    // pop l_paren
    _ = try self.tokenizer.next(err);
    // Create a temporary
    var fnDecl = Ast.FnDecl{
      .return_type = atype,
      .name = "",
      .parameter_list = Ast.NodeList.init(0, self.nodes),
      .body = 0,
    };
    // Peek at the next token
    var r_paren = try self.tokenizer.peek(err);
    var i: u16 = 0;
    // Parse the parameter declaration until we reach a r_paren
    while (r_paren.tag != Token.Tag.r_paren) : (i += 1) {
      fnDecl.parameter_list.appendNode(try self.parseParamDeclaration(err));
      r_paren = try self.tokenizer.peek(err);
      // Ignore the comma but expect it
      if (r_paren.tag == Token.Tag.comma) _ = try self.tokenizer.next(err);
    }
    // Pop r_paren and expect the name of the function
    _ = try self.tokenizer.next(err);
    const identifierToken = try self.tokenizer.next(err);
    if (identifierToken.tag != Token.Tag.identifier) {
      return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, identifierToken.start), err,
        "expecting function name found {}", .{ identifierToken });
    }
    fnDecl.name = self.tokenizer.buffer[identifierToken.start..identifierToken.end + 1];
    // Now parse the body
    const bodyToken = try self.tokenizer.next(err);
    if (bodyToken.tag == Token.Tag.l_bracket) {
      // This is a frame specifier
      // TODO: Support frame specifier
      unreachable;
    }
    switch (bodyToken.tag) {
      Token.Tag.semicolon => {
        // We are dealing with a function declaration
        return self.insertNode(Ast.Payload{ .fn_decl = fnDecl });
      },
      Token.Tag.equal => {
        // We have a body
        const functionContentToken = try self.tokenizer.peek(err);
        switch (functionContentToken.tag) {
          Token.Tag.builtin_literal => {
            // This is a builtin declaration
            const builtinImmediateToken = try self.tokenizer.next(err);
            const scToken = try self.tokenizer.next(err);
            if (scToken.tag != Token.Tag.semicolon) {
              return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, bodyToken.start), err,
                "expecting semicolon got {}", .{ bodyToken });
            }
            const bl = self.tokenizer.buffer[builtinImmediateToken.start + 1..builtinImmediateToken.end + 1];
            const bDecl = Ast.BuiltinDecl{
              .return_type = atype,
              .name = fnDecl.name,
              .parameter_list = fnDecl.parameter_list,
              .index = try std.fmt.parseInt(u16, bl, 10),
            };
            return self.insertNode(Ast.Payload{ .builtin_decl = bDecl });
          },
          Token.Tag.l_brace => {
            // This is a statement list
            fnDecl.body = try self.insertNode(Ast.Payload{ .body = Ast.Body{
              .statement_list = try self.parseStatements(err),
            } });
            return self.insertNode(Ast.Payload{ .fn_decl = fnDecl });
          },
          else => {
            // This is an expression
            const expression = try self.parseExpression(err);
            fnDecl.body = try self.insertNode(Ast.Payload{ .body = Ast.Body{
              .statement_list = Ast.NodeList.init(expression, self.nodes)
              } });
            return self.insertNode(Ast.Payload{ .fn_decl = fnDecl });
          }
        }
      },
      else => {},
    }

    return makeError(ParseError.UnexpectedInput, getLocation(self.tokenizer.buffer, bodyToken.start), err,
      "expecting function body {}", .{ bodyToken });
  }

  fn parseStatements(self: *Self, err: *GenericError) !Ast.NodeList {
    var nodeList = Ast.NodeList.init(0, self.nodes);
    const l_brace = try self.tokenizer.peek(err);
    if (l_brace.tag != Token.Tag.l_brace) {
      // There is only one statment (no braces)
      nodeList.appendNode(try self.parseStatement(err));
      return nodeList;
    }
    // pop l_brace
    _ = try self.tokenizer.next(err);
    var r_brace = try self.tokenizer.peek(err);
    while (r_brace.tag != Token.Tag.r_brace) {
      nodeList.appendNode(try self.parseStatement(err));
      r_brace = try self.tokenizer.peek(err);
    }
    // pop r_brace
    _ = try self.tokenizer.next(err);
    return nodeList;
  }

  fn parseStatement(self: *Self, err: *GenericError) !Ast.Node.Index {
    const firstToken = try self.tokenizer.next(err);
    var index: Ast.Node.Index = 0;
    switch (firstToken.tag) {
      .kw_local => {
        index = try self.parseVariableDefinition(try self.tokenizer.next(err), true, err);
      },
      .kw_return => {
        index = try self.parseExpression(err);
        const scToken = try self.tokenizer.next(err);
        try self.checkToken(scToken, Token.Tag.semicolon, err);
      },
      .kw_if => {

      },
      .kw_while => {

      },
      .kw_do => {

      },
      else => {
        std.log.warn("switch else", .{});
        // If the first token is a type, it is a global variable declaration
        const isError = Ast.QType.fromName(self.tokenizer.buffer[firstToken.start..firstToken.end + 1]);
        if (isError == error.NotAType) {
          // Otherwise it is an expression
          index = try self.parseExpression(err);
          const scToken = try self.tokenizer.next(err);
          try self.checkToken(scToken, Token.Tag.semicolon, err);
        } else {
          index = try self.parseVariableDefinition(firstToken, false, err);
        }
      }
    }
    return index;
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

fn testParse(source: [:0]const u8, err: *GenericError) !void {
  var ast: [256]Ast.Node = undefined;
  var parser = Parser.init(source, ast[0..]);
  try parser.parse(err);
  var output: [4096:0]u8 = .{ 0 } ** 4096;
  _ = try ast[0].prettyPrint(&ast, &output);
  try expectEqualString(source, std.mem.span(@as([*:0]const u8, @ptrCast(&output))));
}

test "parser test" {
  std.testing.log_level = .debug;

  var err = GenericError{};
  try testParse("float f = 3.14;", &err);
  try testParse("float f;", &err);
  try testParse("string s = \"foo\";", &err);
  try testParse("vector v = '1 2 3';", &err);
  try testParse(".vector vectorField;", &err);
  try testParse("void () foo;", &err);
  try testParse("void () foo = #42;", &err);
  try testParse("void (string s) foo;", &err);
  try testParse("void (string s, ...) foo;", &err);
  try testParse("void () main = {}", &err);
  try testParse("void (float f, vector v) main = {}", &err);
  try testParse(
    \\void () foo = {
    \\  local float a = 3.14;
    \\  return 2 + a;
    \\}
    , &err);
}
