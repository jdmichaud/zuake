// clear && zig build -freference-trace && echo "ready" && zig-out/bin/level ../../data/quake106/id1/pak0.pak "maps/start.bsp"
const std = @import("std");

const ioAdapter = @import("io-adapter.zig");
const zlm = @import("zlm.zig").SpecializeOn(f32);

const draw = @import("draw.zig");
const pakModule = @import("pak.zig");
const bspModule = @import("bsp.zig");
const entityModule = @import("entity.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

const CANVAS_WIDTH: i16 = 640;
const CANVAS_HEIGHT: i16 = 480;
const VIEWPORT_WIDTH: i16 = 640 * 2;
const VIEWPORT_HEIGHT: i16 = 480 * 2;

fn load(pathname: []const u8) ![]align(4096) const u8 {
  var file = try std.fs.cwd().openFile(pathname, .{});
  defer file.close();

  const size = try file.getEndPos();
  const buffer = try std.posix.mmap(
    null,
    size,
    std.posix.PROT.READ,
    .{ .TYPE = .SHARED },
    file.handle,
    0,
  );
  errdefer std.posix.munmap(buffer);

  return buffer;
}

fn getBspOffset(allocator: std.mem.Allocator, buffer: []const u8, mapFilepath: []const u8) !usize {
  const entries = try pakModule.loadPak(allocator, buffer);
  defer allocator.free(entries);
  // Look for bsp file
  const bspEntry = for (entries) |entry| {
    if (std.mem.startsWith(u8, &entry.pathname, mapFilepath)) {
      break entry;
    }
  } else unreachable;
  return bspEntry.offset;
}

const Camera = struct {
  const Self = @This();

  look: zlm.Vec4,
  eye: zlm.Vec4,
  up: zlm.Vec4,
  fovx: f32,
  fovy: f32,

  dirty: bool,

  pub fn init(specs: struct {
    look: zlm.Vec4,
    eye: zlm.Vec4,
    up: zlm.Vec4,
    fovx: f32,
    fovy: f32
  }) Self {
    return Self{
      .look = specs.look,
      .eye = specs.eye,
      .up = specs.up,
      .fovx = specs.fovx,
      .fovy = specs.fovy,
      .dirty = true,
    };
  }

  // cache
  var toMatrix = zlm.Mat4.identity;
  var fromMatrix = zlm.Mat4.identity;

  pub fn from(self: *Self) zlm.Mat4 {
    if (self.dirty) self.computeMatrices();
    return fromMatrix;
  }

  pub fn to(self: *Self) zlm.Mat4 {
    if (self.dirty) self.computeMatrices();
    return toMatrix;
  }

  pub fn translate(self: *Self, v: zlm.Vec4) void {
    self.look = self.look.add(v);
    self.eye = self.eye.add(v);
    self.dirty = true;
  }

  pub fn zoom(self: *Self, factor: f16) void {
    const fovx = self.fovx;
    self.fovx = @max(50, self.fovx * factor);
    // So that we always keep the same ratio between fovx and fovy even when
    // maxing the value.
    self.fovy = self.fovy * (self.fovx / fovx);
    self.dirty = true;
  }

  fn computeMatrices(self: *Self) void {
    const direction = zlm.Vec3.sub(self.look.swizzle("xyz"), self.eye.swizzle("xyz"));
    const f = direction.normalize();
    const s = zlm.Vec3.cross(f, self.up.swizzle("xyz")).normalize().scale(self.fovx / 2);
    const u = zlm.Vec3.cross(s, f).normalize().scale(self.fovy / 2);

    fromMatrix = (zlm.Mat4{
      .fields = [4][4]f32{
        [4]f32{ s.x, u.x, f.x, self.look.x },
        [4]f32{ s.y, u.y, f.y, self.look.y },
        [4]f32{ s.z, u.z, f.z, self.look.z },
        [4]f32{   0,   0,   0,           1 },
      },
    }).transpose();
    toMatrix = zlm.Mat4.invert(fromMatrix) orelse unreachable;
    self.dirty = false;
  }
};

fn topCameraFromLevel(model: bspModule.Model) Camera {
  const look = zlm.Vec4.new(
    model.bound.min.x + (model.bound.max.x - model.bound.min.x) / 2,
    model.bound.min.y + (model.bound.max.y - model.bound.min.y) / 2,
    model.bound.max.z,
    1,
  );
  const eye = look.add(zlm.Vec4.new(0, 0, 1, 0));
  const up = zlm.Vec4.new(0, 1, 0, 0);
  const modelWidth = model.bound.max.x - model.bound.min.x;
  const modelHeight = model.bound.max.y - model.bound.min.y;
  // We always consider a 4/3 screen for now
  var fovy: f32 = 0;
  var fovx: f32 = 0;
  if ((modelHeight * 4 / 3) > modelWidth) {
    fovy = modelHeight * 1.1;
    fovx = fovy * 4 / 3;
  } else {
    fovx = modelWidth * 1.1;
    fovy = fovx * 3 / 4;
  }
  return Camera.init(.{
    .look = look,
    .eye = eye,
    .up = up,
    .fovx = fovx,
    .fovy = fovy,
  });
}

fn frontCameraFromLevel(model: bspModule.Model) Camera {
  const look = zlm.Vec4.new(
    model.bound.min.x + (model.bound.max.x - model.bound.min.x) / 2,
    model.bound.min.y + (model.bound.max.y - model.bound.min.y) / 2,
    model.bound.min.z + (model.bound.max.z - model.bound.min.z) / 2,
    1,
  );
  const eye = look.add(zlm.Vec4.new(0, 1, 0, 0));
  const up = zlm.Vec4.new(0, 0, -1, 0);
  const modelWidth = model.bound.max.x - model.bound.min.x;
  const modelHeight = model.bound.max.z - model.bound.min.z;
  // We always consider a 4/3 screen for now
  var fovy: f32 = 0;
  var fovx: f32 = 0;
  if ((modelHeight * 4 / 3) > modelWidth) {
    fovy = modelHeight * 1.1;
    fovx = fovy * 4 / 3;
  } else {
    fovx = modelWidth * 1.1;
    fovy = fovx * 3 / 4;
  }
  return Camera.init(.{
    .look = look,
    .eye = eye,
    .up = up,
    .fovx = fovx,
    .fovy = fovy,
  });
}

const Model = struct {
  fps: f32,
  bsp: bspModule.Bsp,
  canvasCS: CanvasCS,
  viewportCS: ViewportCS,
  camera: Camera,
  mapName: []const u8,
  mousePosition: zlm.Vec2,
};

// Switch between Camera <-> Canvas
const CanvasCS = struct {
  const Self = @This();

  fromMatrix: zlm.Mat4,
  toMatrix: zlm.Mat4,

  // Initialize a canvas coordinate system
  // width and height are the resolution of the canvas.
  // ratio is the aspect ratio (4/3, 16/9, ...)
  pub fn init(width: i16, height: i16, ratio: f32) Self {
    _ = ratio;
    const fromMatrix = (zlm.Mat4{
      .fields = [4][4]f32{
        [4]f32{ 2 / @as(f32, @floatFromInt(width)),                                   0, 0,   -1 },
        [4]f32{                                  0, 2 / @as(f32, @floatFromInt(height)), 0,   -1 },
        [4]f32{                                  0,                                   0, 1,    0 },
        [4]f32{                                  0,                                   0, 0,    1 },
      },
    }).transpose();
    const toMatrix = zlm.Mat4.invert(fromMatrix) orelse @panic("toMatrix non invertible");
    return Self {
      .fromMatrix = fromMatrix,
      .toMatrix = toMatrix,
    };
  }

  pub fn to(self: Self) zlm.Mat4 {
    return self.toMatrix;
  }
};

// Switch between Viewport <-> Canvas
const ViewportCS = struct {
  const Self = @This();

  fromMatrix: zlm.Mat4,
  toMatrix: zlm.Mat4,

  // Initialize a canvas coordinate system
  // width and height are the resolution of the canvas.
  // ratio is the aspect ratio (4/3, 16/9, ...)
  pub fn init(width: i16, height: i16, ratio: f32) Self {
    _ = ratio;
    const fromMatrix = (zlm.Mat4{
      .fields = [4][4]f32{
        [4]f32{ 2 / @as(f32, @floatFromInt(width)),                                   0, 0, -1 },
        [4]f32{                                  0, 2 / @as(f32, @floatFromInt(height)), 0, -1 },
        [4]f32{                                  0,                                   0, 1,  0 },
        [4]f32{                                  0,                                   0, 0,  1 },
      },
    }).transpose();
    const toMatrix = zlm.Mat4.invert(fromMatrix) orelse @panic("toMatrix non invertible");
    return Self {
      .fromMatrix = fromMatrix,
      .toMatrix = toMatrix,
    };
  }

  pub fn to(self: Self) zlm.Mat4 {
    return self.toMatrix;
  }

  pub fn from(self: Self) zlm.Mat4 {
    return self.fromMatrix;
  }
};

fn as(comptime T: type, integer: anytype) T {
  return @as(T, @intCast(integer));
}

// Print on the 80x60 character terminal
fn print(comptime Context: type, x: u8, y: u8, comptime fmt: []const u8, args: anytype) void {
  var buffer_array: [100]u8 = undefined;
  const buffer: []u8 = buffer_array[0..];
  Context.printText(as(i16, x) * 8, as(i16, y) * 8, std.fmt.bufPrint(buffer, fmt, args) catch unreachable);
}

fn update(comptime Context: type, model: *Model) void {
  Context.color = 0xFF000000;
  Context.clearRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
  Context.color = 0xFFFFFFFF;

  const mousePositionCamera = model.mousePosition.swizzle("xy01")
    .transform(model.viewportCS.from());
  const mousePositionInWorld = mousePositionCamera
    .transform(model.camera.from());

  Context.save() catch unreachable;
  Context.reset();
  print(Context, 0, Context.contextHeight / 8 - 5, "pointer   : X{d:.0} Y{d:.0} (W X{d:.1} Y{d:.1} Z{d:.1} C {d:.1} {d:.1})", .{
    model.mousePosition.x, model.mousePosition.y,
    mousePositionInWorld.x, mousePositionInWorld.y, mousePositionInWorld.z,
    mousePositionCamera.x, mousePositionCamera.y });
  print(Context, 0, Context.contextHeight / 8 - 2, "map       : {s}", .{ model.mapName });
  print(Context, 0, Context.contextHeight / 8 - 1, "fps       : {d:.0}", .{ model.fps });
  Context.restore();

  const worldToCanvas = model.camera.to().mul(model.canvasCS.to());
  const pos = model.mousePosition
    .swizzle("xy11")
    .transform(model.canvasCS.to().mul(model.viewportCS.from()))
    .swizzle("xy");

  var displayedEdgeCount: usize = 0;
  var displayedNodeCount: usize = 0;

  var it = model.bsp.models[0].preOrderNodeIterator(model.bsp);
  while (it.next()) |entry| {
    switch (entry) {
      bspModule.NodeType.Node => |node| {
        const nodeMin = zlm.vec4(@floatFromInt(node.boundingBoxMinX), @floatFromInt(node.boundingBoxMinY), @floatFromInt(node.boundingBoxMinZ), 1).transform(model.camera.to());
        const nodeMax = zlm.vec4(@floatFromInt(node.boundingBoxMaxX), @floatFromInt(node.boundingBoxMaxY), @floatFromInt(node.boundingBoxMaxZ), 1).transform(model.camera.to());
        if (
          (nodeMin.x < -1 and nodeMax.x < -1) or (nodeMin.x > 1 and nodeMax.x > 1) or
          (nodeMin.y < -1 and nodeMax.y < -1) or (nodeMin.y > 1 and nodeMax.y > 1)) {
          it.skip();
          continue;
        }

        displayedNodeCount += 1;
      },
      bspModule.NodeType.Leaf => |leaf| {
        // Check if the cursor is within the boundbox of the leaf
        const checkSelect = mousePositionInWorld.x > @as(f32, @floatFromInt(leaf.boundingBoxMinX))
          and mousePositionInWorld.x < @as(f32, @floatFromInt(leaf.boundingBoxMaxX))
          and mousePositionInWorld.y > @as(f32, @floatFromInt(leaf.boundingBoxMinY))
          and mousePositionInWorld.y < @as(f32, @floatFromInt(leaf.boundingBoxMaxY));
        var faceIterator = leaf.faceIterator(model.bsp);
        while (faceIterator.next()) |face| {
          var edgeIterator = face.edgeIterator(model.bsp);
          while (edgeIterator.next()) |e| {
            displayedEdgeCount += 1;
            const vertex0 = model.bsp.vertices[e.edge.vertex0];
            const vertex1 = model.bsp.vertices[e.edge.vertex1];

            // std.log.debug("{d:.0} {d:.0} {d:.0}  {d:.0} {d:.0} {d:.0}", .{ vertex0.X, vertex0.Y, vertex0.Z, vertex1.X, vertex1.Y, vertex1.Z });
            const start = zlm.vec4(vertex0.X, vertex0.Y, vertex0.Z, 1).transform(worldToCanvas).swizzle("xy");
            const end = zlm.vec4(vertex1.X, vertex1.Y, vertex1.Z, 1).transform(worldToCanvas).swizzle("xy");
            Context.line(@intFromFloat(start.x), @intFromFloat(start.y), @intFromFloat(end.x), @intFromFloat(end.y));

            if (checkSelect) {
              const seg = end.sub(start);
              const d = pos.sub(start).dot(seg) / std.math.pow(f32, seg.length(), 2);
              if (d > 0 and d < 1 and segmentPointDistance(start, end, pos) < 3) {
                // Underline the edge below the cursor
                Context.thickness = 2;
                Context.color = 0xFFE5881E;
                Context.line(@intFromFloat(start.x), @intFromFloat(start.y), @intFromFloat(end.x), @intFromFloat(end.y));
                Context.thickness = 1;
                Context.color = 0xFFFFFFFF;
                print(Context, 0, Context.contextHeight / 8 - 6, "segment   : {}", .{ e.id });
              }
            }
          }
        }
      },
    }
  }
  print(Context, 0, Context.contextHeight / 8 - 4, "disp. node: {}", .{ displayedNodeCount });
  print(Context, 0, Context.contextHeight / 8 - 3, "disp. edge: {}", .{ displayedEdgeCount });

  // Draw bounding box
  Context.color = 0xFFEFD867;
  const minbox = zlm.vec4(model.bsp.models[0].bound.min.x, model.bsp.models[0].bound.min.y, model.bsp.models[0].bound.min.z, 1)
    .transform(worldToCanvas).swizzle("xy");
  const maxbox = zlm.vec4(model.bsp.models[0].bound.max.x, model.bsp.models[0].bound.max.y, model.bsp.models[0].bound.max.z, 1)
    .transform(worldToCanvas).swizzle("xy");
  const mincorner = zlm.vec2(@min(minbox.x, maxbox.x), @min(minbox.y, maxbox.y));
  Context.rect(
    @intFromFloat(@max(std.math.minInt(i16), @min(std.math.maxInt(i16), mincorner.x))),
    @intFromFloat(@max(std.math.minInt(i16), @min(std.math.maxInt(i16), mincorner.y))),
    @intFromFloat(@max(std.math.minInt(i16), @min(std.math.maxInt(i16), maxbox.sub(minbox).swizzle("x0").length()))),
    @intFromFloat(@max(std.math.minInt(i16), @min(std.math.maxInt(i16), maxbox.sub(minbox).swizzle("y0").length()))));

  const origin = zlm.vec4(0, 0, 0, 1).transform(worldToCanvas).swizzle("xy");
  Context.plot(
    @intFromFloat(@max(std.math.minInt(i16), @min(std.math.maxInt(i16), origin.x))),
    @intFromFloat(@max(std.math.minInt(i16), @min(std.math.maxInt(i16), origin.y))),
    0xFF0000FF);
}

const Edge = struct {
  id: u32,
  start: zlm.Vec4,
  end: zlm.Vec4,
};

pub fn segmentPointDistance(start: zlm.Vec2, end: zlm.Vec2, point: zlm.Vec2) f32 {
  return @abs((end.x - start.x) * (point.y - start.y) - (point.x - start.x) * (end.y - start.y))
    / @sqrt(std.math.pow(f32, end.x - start.x, 2) + std.math.pow(f32, end.y - start.y, 2));
}

pub fn main() !void {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  if (args.len != 3 or (args.len != 1 and (std.mem.eql(u8, args[1], "--help")
        or std.mem.eql(u8, args[1], "-h")))) {
    try stdout.print("{s} is a tool used to ???\n\n", .{ args[0] });
    try stdout.print("usage:\n", .{});
    try stdout.print("    {s} <pakfile> <map_path> - ???\n", .{ args[0] });
    return;
  }

  const pakFilepath = args[1];
  const mapFilepath = args[2];

  // Load pak file
  const pak = try load(pakFilepath);
  defer std.posix.munmap(pak);
  // Load bsp file
  var bsp = try bspModule.Bsp.init(allocator, pak[try getBspOffset(allocator, pak, mapFilepath)..]);
  defer bsp.deinit(allocator);

  var entities = try entityModule.EntityList.init(allocator, bsp.entities);
  defer entities.deinit();

  var sdlAdapter = try ioAdapter.SDLAdapter.init(VIEWPORT_WIDTH, VIEWPORT_HEIGHT);
  defer sdlAdapter.deinit();

  const canvasCS = CanvasCS.init(CANVAS_WIDTH, CANVAS_HEIGHT, 4.0 / 3.0);
  const viewportCS = ViewportCS.init(VIEWPORT_WIDTH, VIEWPORT_HEIGHT, 4.0 / 3.0);
  const context = draw.DrawContext(CANVAS_WIDTH, CANVAS_HEIGHT);
  context.setTransform(1, 0, 0, 1, 0, 0);

  var mousebtns: u3 = 0;

  var model = Model {
    .fps = 0,
    .bsp = bsp,
    .canvasCS = canvasCS,
    .viewportCS = viewportCS,
    .camera = topCameraFromLevel(bsp.models[0]),
    .mapName = if (entities.get("worldspawn").?.get("message")) |message| message.toString() else "Not a map",
    .mousePosition = zlm.vec2(0, 0),
  };

  var quit = false;
  var contextDirty = true;
  while (!quit) {
    const then = std.time.microTimestamp();
    while (sdlAdapter.interface.getEvent()) |event| {
      switch (event) {
        ioAdapter.EventType.KeyDown => |keyEvent| {
          switch (keyEvent.scancode) {
            ioAdapter.Scancode.ESCAPE => quit = true,
            ioAdapter.Scancode.F => {
              model.camera = frontCameraFromLevel(bsp.models[0]);
              contextDirty = true;
            },
            ioAdapter.Scancode.T => {
              model.camera = topCameraFromLevel(bsp.models[0]);
              contextDirty = true;
            },
            else => {},
          }
        },
        ioAdapter.EventType.MouseMove => |payload| {
          model.mousePosition.x = @floatFromInt(payload.x);
          model.mousePosition.y = @floatFromInt(payload.y);
          if (mousebtns & @intFromEnum(ioAdapter.MouseButton.Left) != 0) {
            const translation = zlm.vec4(@floatFromInt(-payload.dx), @floatFromInt(-payload.dy), 0, 0)
              .transform(model.viewportCS.from())
              .transform(model.camera.from());

            model.camera.translate(translation);
          }
          contextDirty = true;
        },
        ioAdapter.EventType.MouseDown => |payload| {
          mousebtns |= @as(u3, 1) << as(u2, @intFromEnum(payload.button) - 1);
        },
        ioAdapter.EventType.MouseUp => |payload| {
          mousebtns ^= mousebtns & (@as(u3, 1) << as(u2, (@intFromEnum(payload.button) - 1)));
        },
        ioAdapter.EventType.MouseWheel => |payload| {
          model.camera.zoom(if (payload.y > 0) 0.9 else 1.1);
          contextDirty = true;
        },
        else => {},
      }
    }

    if (contextDirty) {
      update(context, &model);
      sdlAdapter.interface.drawImage(&context.buffer, 0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
      sdlAdapter.interface.renderScene();
      contextDirty = false;
    }

    // Only go for 60fps
    const timePerFrame = std.time.microTimestamp() - then;
    if (timePerFrame > 1000) {
      model.fps = model.fps * 0.9 + 1000000.0 / @as(f32, @floatFromInt(timePerFrame)) * 0.1;
    }
    const delay: i32 = 16 - as(i32, @divTrunc(timePerFrame, 1000));
    std.time.sleep(if (delay > 0) as(u32, delay) else 0);
  }
}
