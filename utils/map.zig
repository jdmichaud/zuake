// Load a Quake map file (the original map, not the compiled bsp file)
//
// reference:
//  https://www.gamers.org/dEngine/quake/spec/quake-spec34/qkspec_2.htm#CMFM0
//
// cmd: clear && zig build-exe -freference-trace map.zig && ./map ../../quake_map_source/start.map

const std = @import("std");
const misc = @import("misc.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdOut().writer();

pub const Plane = struct {
  points: [3]@Vector(3, i16),
  texture: []const u8,
  xOff: f32,
  yOff: f32,
  rotation: f32,
  xScale: f32,
  yScale: f32,
};

pub const Brush = []Plane;

pub const EntityValue = struct {
  const Self = @This();

  value: []const u8,

  pub fn toString(self: Self) []const u8 {
    return self.value;
  }

  pub fn toInteger(self: Self) i64 {
    return try std.fmt.parseInt(i64, self.value, 10);
  }

  pub fn toFloat(self: Self) f32 {
    return try std.fmt.parseFloat(f32, self.value);
  }
};

pub const EntityMap = std.StringHashMap(EntityValue);

pub const Entity = struct {
  const Self = @This();

  fields: EntityMap,
  brushes: []Brush,

  pub fn get(self: Self, key: []const u8) ?EntityValue {
    return self.fields.get(key);
  }
};

const ResultType = enum {
  Ok,
  Error,
};

fn Result(comptime OkType: type, comptime ErrorType: type) type {
  return union(ResultType) {
    Ok: OkType,
    Error: ErrorType,
  };
}

pub const MapError = struct {
  const Self = @This();

  column: i16 = 1,
  row: i16 = 1,
  errorMessage: [255:0]u8 = [_:0]u8{ 0 } ** 255,
};

pub const Map = struct {
  const Self = @This();

  entities: []Entity,
  allocator: std.mem.Allocator,

  pub fn init(allocator: std.mem.Allocator, buffer: []const u8, mapError: *MapError) !Self {
    mapError.column = 1;
    mapError.row = 1;

    const State = enum {
      LOOKING_FOR_ENTITY,
      LOOKING_FOR_KEY,
      PARSING_KEY,
      LOOKING_FOR_VALUE,
      PARSING_VALUE,
      LOOKING_FOR_BRUSH,
      PARSING_BRUSH,
      LOOKING_FOR_POINT,
    };

    // ( 128 0 0 ) ( 128 1 0 ) ( 128 0 1 ) GROUND1_6   0     0       0      1.0    1.0
    // 1st Point   2nd Point   3rd Point   Texture   x_off y_off rotation x_scale y_scale
    const BrushParsingState = enum {
      POINTS,
      TEXTURE,
      X_OFF,
      Y_OFF,
      ROTATION,
      X_SCALE,
      Y_SCALE,
    };

    var list = std.ArrayList(Entity).init(allocator);
    var entityMap: ?EntityMap = null;
    var brushes: ?std.ArrayList(Brush) = null;
    var planes: ?std.ArrayList(Plane) = null;
    var state = State.LOOKING_FOR_ENTITY;
    var substate = BrushParsingState.POINTS;
    var key: ?std.ArrayList(u8) = null;
    var value: ?std.ArrayList(u8) = null;
    // variables for the bruches
    var intpile = [9][]u8{"", "", "", "", "", "", "", "", ""};
    var floatpile = [5][]u8{"", "", "", "", ""};
    var texture: ?[]u8 = null;

    errdefer {
      list.deinit();
      if (brushes != null) {
        brushes.?.deinit();
      }
      if (planes != null) {
        planes.?.deinit();
      }
      if (key) |k| {
        k.deinit();
      }
      if (value) |v| {
        v.deinit();
      }
      if (entityMap) |*em| {
        var it = em.iterator();
        while (it.next()) |entityValue| {
          allocator.free(entityValue.key_ptr.*);
          allocator.free(entityValue.value_ptr.value);
        }
        em.deinit();
      }
      for (intpile) |integer| {
        if (integer.len != 0) allocator.free(integer);
      }
      for (floatpile) |float| {
        if (float.len != 0) allocator.free(float);
      }
      if (texture != null) {
        allocator.free(texture.?);
      }
    }

    for (buffer) |c| {
      switch (state) {
        State.LOOKING_FOR_ENTITY => {
          if (c == '{') {
            entityMap = EntityMap.init(allocator);
            brushes = std.ArrayList(Brush).init(allocator);
            state = State.LOOKING_FOR_KEY;
            continue;
          } else if (!std.ascii.isWhitespace(c) and c != 0) {
            _ = try std.fmt.bufPrint(&mapError.errorMessage,  "was expecting '{{' found {c}", .{ c });
            return error.ParseError;
          }
        },
        State.LOOKING_FOR_KEY => {
          if (c == '"') {
            state = State.PARSING_KEY;
            key = std.ArrayList(u8).init(allocator);
            continue;
          } else if (c == '}') {
            try list.append(Entity{
              .fields = entityMap.?,
              .brushes = if (brushes != null) try brushes.?.toOwnedSlice() else &[_]Brush{},
            });
            if (brushes != null) {
              brushes.?.deinit();
              brushes = null;
            }
            state = State.LOOKING_FOR_ENTITY;
          } else if (c == '{') {
            state = State.LOOKING_FOR_BRUSH;
            planes = std.ArrayList(Plane).init(allocator);
          } else if (!std.ascii.isWhitespace(c)) {
            _ = try std.fmt.bufPrint(&mapError.errorMessage, "was expecting '\"' or '}}' found {c}", .{ c });
            return error.ParseError;
          }
        },
        State.PARSING_KEY => {
          if (c == '"') {
            state = State.LOOKING_FOR_VALUE;
            continue;
          } else {
            try key.?.append(c);
          }
        },
        State.LOOKING_FOR_VALUE => {
          if (c == '"') {
            state = State.PARSING_VALUE;
            value = std.ArrayList(u8).init(allocator);
            continue;
          } else if (!std.ascii.isWhitespace(c)) {
            _ = try std.fmt.bufPrint(&mapError.errorMessage, "was expecting '\"' found {c}", .{ c });
            return error.ParseError;
          }
        },
        State.PARSING_VALUE => {
          if (c == '"') {
            state = State.LOOKING_FOR_KEY;
            try entityMap.?.put(try key.?.toOwnedSlice(), EntityValue{
              .value = try value.?.toOwnedSlice(),
            });
            key.?.deinit();
            value.?.deinit();
            continue;
          } else {
            try value.?.append(c);
          }
        },
        State.LOOKING_FOR_BRUSH => {
          if (c == '(') {
            state = State.LOOKING_FOR_POINT;
          } else if (c == '}') {
            try brushes.?.append(try planes.?.toOwnedSlice());
            planes.?.deinit();
            planes = null;
            state = State.LOOKING_FOR_KEY;
          } else if (!std.ascii.isWhitespace(c)) {
            _ = try std.fmt.bufPrint(&mapError.errorMessage, "was expecting '(' found {c}", .{ c });
            return error.ParseError;
          }
        },
        State.LOOKING_FOR_POINT => {
          if (c == '-' or std.ascii.isDigit(c)) {
            key = std.ArrayList(u8).init(allocator);
            try key.?.append(c);
            state = State.PARSING_BRUSH;
            substate = BrushParsingState.POINTS;
          } else if (!std.ascii.isWhitespace(c)) {
            _ = try std.fmt.bufPrint(&mapError.errorMessage, "was expecting '-' or a digit, found {c}", .{ c });
            return error.ParseError;
          }
        },
        State.PARSING_BRUSH => {
          switch (substate) {
            BrushParsingState.POINTS => {
              if (c == '-' or std.ascii.isDigit(c)) {
                if (key == null) {
                  key = std.ArrayList(u8).init(allocator);
                }
                try key.?.append(c);
              } else if (std.ascii.isWhitespace(c)) {
                if (key != null) {
                  for (&intpile) |*integer| {
                    if (integer.len == 0) {
                      integer.* = try key.?.toOwnedSlice();
                      key.?.deinit();
                      key = null;
                      break;
                    }
                  }
                }
              } else if (c == '(') {
                // Nothing to do
              } else if (c == ')') {
                if (intpile[8].len != 0) {
                  // We have finished the points of this brush
                  substate = BrushParsingState.TEXTURE;
                }
              } else {
                _ = try std.fmt.bufPrint(&mapError.errorMessage, "was expecting '-' or a digit found {c}", .{ c });
                return error.ParseError;
              }
            },
            BrushParsingState.TEXTURE => {
              if (std.ascii.isWhitespace(c) and key == null) {
                // Nothing to do
              } else if (std.ascii.isWhitespace(c) and key != null) {
                texture = try key.?.toOwnedSlice();
                key.?.deinit();
                key = null;
                substate = BrushParsingState.X_OFF;
              } else {
                if (key == null) {
                  key = std.ArrayList(u8).init(allocator);
                }
                try key.?.append(c);
              }
            },
            BrushParsingState.X_OFF,
            BrushParsingState.Y_OFF,
            BrushParsingState.ROTATION,
            BrushParsingState.X_SCALE,
            BrushParsingState.Y_SCALE => {
              if (std.ascii.isWhitespace(c) and key == null) {
                // Nothing to do
              } else if (std.ascii.isWhitespace(c) and key != null) {
                if (substate != BrushParsingState.Y_SCALE) {
                  floatpile[@intFromEnum(substate) - 2] = try key.?.toOwnedSlice();
                  key.?.deinit();
                  key = null;
                  substate = @enumFromInt(@intFromEnum(substate) + 1);
                } else {
                  floatpile[4] = try key.?.toOwnedSlice();
                  key.?.deinit();
                  key = null;
                  try planes.?.append(Plane {
                    .points = [3]@Vector(3, i16){
                      @Vector(3, i16){
                        try std.fmt.parseInt(i16, intpile[0], 10),
                        try std.fmt.parseInt(i16, intpile[1], 10),
                        try std.fmt.parseInt(i16, intpile[2], 10),
                      },
                      @Vector(3, i16){
                        try std.fmt.parseInt(i16, intpile[3], 10),
                        try std.fmt.parseInt(i16, intpile[4], 10),
                        try std.fmt.parseInt(i16, intpile[5], 10),
                      },
                      @Vector(3, i16){
                        try std.fmt.parseInt(i16, intpile[6], 10),
                        try std.fmt.parseInt(i16, intpile[7], 10),
                        try std.fmt.parseInt(i16, intpile[8], 10),
                      },
                    },
                    .texture = texture.?,
                    .xOff = try std.fmt.parseFloat(f32, floatpile[0]),
                    .yOff = try std.fmt.parseFloat(f32, floatpile[1]),
                    .rotation = try std.fmt.parseFloat(f32, floatpile[2]),
                    .xScale = try std.fmt.parseFloat(f32, floatpile[3]),
                    .yScale = try std.fmt.parseFloat(f32, floatpile[4]),
                  });
                  for (&intpile) |*integer| {
                    allocator.free(integer.*);
                    integer.* = "";
                  }
                  for (&floatpile) |*float| {
                    allocator.free(float.*);
                    float.* = "";
                  }
                  state = State.LOOKING_FOR_BRUSH;
                }
              } else {
                if (key == null) {
                  key = std.ArrayList(u8).init(allocator);
                }
                try key.?.append(c);
              }
            },
          }
        }
      }
      // std.log.debug("c {c} column {} row {} state {} substate {}", .{ c, mapError.column, mapError.row, state, substate });
      if (c == '\n') {
        mapError.column = 1;
        mapError.row += 1;
      } else {
        mapError.column += 1;
      }
    }

    return Map{
      .allocator = allocator,
      .entities = try list.toOwnedSlice(),
    };
  }

  pub fn deinit(self: *Self) void {
    for (self.entities) |*entity| {
      var it = entity.fields.iterator();
      while (it.next()) |entityValue| {
        self.allocator.free(entityValue.key_ptr.*);
        self.allocator.free(entityValue.value_ptr.value);
      }
      entity.fields.deinit();
      for (entity.brushes) |brush| {
        for (brush) |plane| {
          self.allocator.free(plane.texture);
        }
        self.allocator.free(brush);
      }
      self.allocator.free(entity.brushes);
    }
    self.allocator.free(self.entities);
    self.* = undefined;
  }

  pub fn get(self: Self, key: []const u8) ?Entity {
    for (self.entities) |entity| {
      if (entity.get("classname")) |entityValue| {
        if (std.mem.eql(u8, entityValue.toString(), key)) {
          return entity;
        }
      }
    }

    return null;
  }
};

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 2 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a bsp29 tool\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <mapfile> - Show the map content\n", .{ args[0] });
    return;
  }
  const mapfilepath = args[1];
  const buffer = misc.load(mapfilepath) catch |err| {
    try stderr.print("error: {}, trying to open open {s}\n", .{ err, args[1] });
    std.posix.exit(1);
  };
  defer std.posix.munmap(buffer);

  var mapError = MapError{};
  var map = Map.init(allocator, buffer, &mapError) catch |err| {
    if (err == error.ParseError) {
      try stderr.print("{s}:{}:{}: {s}\n", .{ mapfilepath, mapError.row, mapError.column, mapError.errorMessage });
      std.posix.exit(1);
    }
    return err;
  };
  defer map.deinit();

  try stdout.print("{s} loaded\n", .{ map.get("worldspawn").?.get("message").?.toString() });
  std.log.debug("{}", .{ map.get("worldspawn").?.brushes.len });
}
