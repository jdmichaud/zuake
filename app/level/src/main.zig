// clear && zig build -freference-trace && echo "ready" && zig-out/bin/level ../../data/quake106/id1/pak0.pak "maps/start.bsp"
const std = @import("std");

const sdlwrapper = @import("sdl.zig");
const sdl = sdlwrapper.sdl;
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

  // cache
  var toMatrix = zlm.Mat4.identity;
  var fromMatrix = zlm.Mat4.identity;
  var dirty = true;

  pub fn from(self: Self) zlm.Mat4 {
    if (dirty) self.computeMatrices();
    return fromMatrix;
  }

  pub fn to(self: Self) zlm.Mat4 {
    if (dirty) self.computeMatrices();
    return toMatrix;
  }

  pub fn translate(self: *Self, v: zlm.Vec4) void {
    self.look = self.look.add(v);
    self.eye = self.eye.add(v);
    dirty = true;
  }

  pub fn zoom(self: *Self, factor: f16) void {
    self.fovx *= factor;
    self.fovy *= factor;
    dirty = true;
  }

  fn computeMatrices(self: Self) void {
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
    dirty = false;
  }
};

const Model = struct {
  fps: f32,
  bsp: bspModule.Bsp,
  canvasCS: CanvasCS,
  camera: Camera,
  edges: std.ArrayList(Edge),
  boundingBox: bspModule.BoundBox,
  mapName: []const u8,
};

// Switch between Camera <-> Canvas
const CanvasCS = struct {
  const Self = @This();

  fromCanvas: zlm.Mat4,
  toCanvas: zlm.Mat4,

  // Initialize a canvas coordinate system
  // width and height are the resolution of the canvas.
  // ratio is the aspect ratio (4/3, 16/9, ...)
  pub fn init(width: i16, height: i16, ratio: f32) CanvasCS {
    const fromCanvas = (zlm.Mat4{
      .fields = [4][4]f32{
        [4]f32{ 2 * ratio / @as(f32, @floatFromInt(width)),                                   0, 0, -1.0 * ratio },
        [4]f32{                                          0, 2 / @as(f32, @floatFromInt(height)), 0,           -1 },
        [4]f32{                                          0,                                   0, 1,            0 },
        [4]f32{                                          0,                                   0, 0,            1 },
      },
    }).transpose();
    const toCanvas = zlm.Mat4.invert(fromCanvas) orelse @panic("toCanvas non invertible");
    return CanvasCS {
      .fromCanvas = fromCanvas,
      .toCanvas = toCanvas,
    };
  }

  pub fn to(self: Self) zlm.Mat4 {
    return self.toCanvas;
  }
};

fn update(comptime Context: type, model: Model) void {
  var buffer_array: [100]u8 = undefined;
  const buffer: []u8 = buffer_array[0..];

  Context.color = 0xFF000000;
  Context.clearRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
  Context.color = 0xFFFFFFFF;
  Context.save() catch unreachable;
  Context.reset();
  Context.printText(3, CANVAS_HEIGHT - 8 - 3, std.fmt.bufPrint(buffer, "fps: {d:.0}", .{ model.fps }) catch unreachable);
  Context.printText(3, CANVAS_HEIGHT - 16 - 3 - 2, std.fmt.bufPrint(buffer, "map: {s}", .{ model.mapName }) catch unreachable);
  Context.restore();

  const worldToCanvas = model.camera.to().mul(model.canvasCS.to());
  for (model.edges.items) |edge| {
    const start = edge.start.transform(worldToCanvas).swizzle("xy");
    const end = edge.end.transform(worldToCanvas).swizzle("xy");
    Context.line(@intFromFloat(start.x), @intFromFloat(start.y), @intFromFloat(end.x), @intFromFloat(end.y));
  }

  // Draw bounding box
  Context.color = 0xFFEFD867;
  const mincorner = zlm.vec4(model.boundingBox.min.x, model.boundingBox.min.y, model.boundingBox.min.z, 1)
    .transform(worldToCanvas).swizzle("xy");
  const maxcorner = zlm.vec4(model.boundingBox.max.x, model.boundingBox.max.y, model.boundingBox.max.z, 1)
    .transform(worldToCanvas).swizzle("xy");
  Context.rect(@intFromFloat(mincorner.x), @intFromFloat(mincorner.y),
    @intFromFloat(maxcorner.sub(mincorner).swizzle("x0").length()),
    @intFromFloat(maxcorner.sub(mincorner).swizzle("y0").length()));
}

fn cameraTopFromLevel(model: bspModule.Model) Camera {
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
  return Camera {
    .look = look,
    .eye = eye,
    .up = up,
    .fovx = fovx,
    .fovy = fovy,
  };
}

const toViewport: zlm.Mat4 = zlm.Mat4.invert(zlm.Mat4{
  .fields = [4][4]f32{
    [4]f32{ (4.0 / 1.5) / @as(f32, @floatFromInt(VIEWPORT_WIDTH)),                                            0, 0, 0 },
    [4]f32{                                                     0, 2 / @as(f32, @floatFromInt(VIEWPORT_HEIGHT)), 0, 0 },
    [4]f32{                                                     0,                                            0, 1, 0 },
    [4]f32{                                                     0,                                            0, 0,                1 },
  },
}) orelse @panic("toViewport non invertible");

const Edge = struct {
  start: zlm.Vec4,
  end: zlm.Vec4,
};

fn buildEdges(edges: *std.ArrayList(Edge), bsp: bspModule.Bsp, model: bspModule.Model, ) !void {
  for (@intCast(model.faceId)..@intCast(model.faceId + model.faceNum)) |faceId| {
    const face = bsp.faces[bsp.lfaces[faceId]];
    for (@intCast(face.ledgeId)..@intCast(face.ledgeId + face.ledgeNum)) |ledgeId| {
      const edge = bsp.edges[@abs(bsp.ledges[ledgeId])];
      const vertex0 = bsp.vertices[edge.vertex0];
      const vertex1 = bsp.vertices[edge.vertex1];
      if (bsp.ledges[ledgeId] > 0) {
        try edges.append(Edge {
          .start = zlm.vec4(vertex0.X, vertex0.Y, vertex0.Z, 1),
          .end = zlm.vec4(vertex1.X, vertex1.Y, vertex1.Z, 1),
        });
      } else {
        try edges.append(Edge {
          .end = zlm.vec4(vertex0.X, vertex0.Y, vertex0.Z, 1),
          .start = zlm.vec4(vertex1.X, vertex1.Y, vertex1.Z, 1),
        });
      }
    }
  }
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

  var subsystem = try sdlwrapper.SdlSubsystem.init(VIEWPORT_WIDTH, VIEWPORT_HEIGHT);
  defer subsystem.deinit();

  const canvasCS = CanvasCS.init(CANVAS_WIDTH, CANVAS_HEIGHT, 4.0 / 3.0);
  const context = draw.DrawContext(CANVAS_WIDTH, CANVAS_HEIGHT);
  context.setTransform(1, 0, 0, 1, 0, 0);

  var mousebtns: u3 = 0;

  var edges = std.ArrayList(Edge).init(allocator);
  defer edges.deinit();
  var boundingBox = bspModule.BoundBox {
    .min = bspModule.vec3 { .x =  std.math.inf(f64), .y =  std.math.inf(f64), .z =  std.math.inf(f64) },
    .max = bspModule.vec3 { .x = -std.math.inf(f64), .y = -std.math.inf(f64), .z = -std.math.inf(f64) },
  };
  for (bsp.models) |model| {
    try buildEdges(&edges, bsp, model);
    if (model.bound.min.x < boundingBox.min.x) {
      boundingBox.min.x = model.bound.min.x;
    }
    if (model.bound.min.y < boundingBox.min.y) {
      boundingBox.min.y = model.bound.min.y;
    }
    if (model.bound.min.z < boundingBox.min.z) {
      boundingBox.min.z = model.bound.min.z;
    }
    if (model.bound.max.x > boundingBox.max.x) {
      boundingBox.max.x = model.bound.max.x;
    }
    if (model.bound.max.y > boundingBox.max.y) {
      boundingBox.max.y = model.bound.max.y;
    }
    if (model.bound.max.z > boundingBox.max.z) {
      boundingBox.max.z = model.bound.max.z;
    }
  }
  std.log.debug("{} edges loaded", .{ edges.items.len });

  var model = Model {
    .fps = 0,
    .bsp = bsp,
    .canvasCS = canvasCS,
    .camera = cameraTopFromLevel(bsp.models[0]),
    .edges = edges,
    .boundingBox = boundingBox,
    .mapName = (try entities.get("worldspawn")).get("message").?.toString(),
  };

  var quit = false;
  var contextDirty = true;
  while (!quit) {
    var event: sdl.SDL_Event = undefined;
    const then = std.time.microTimestamp();
    while (sdl.SDL_PollEvent(&event) != 0) {
      switch (event.type) {
        sdl.SDL_QUIT => { quit = true; },
        sdl.SDL_KEYDOWN => {
          switch (event.key.keysym.sym) {
            sdl.SDLK_ESCAPE => quit = true,
            else => {},
          }
        },
        sdl.SDL_MOUSEMOTION => {
          if (mousebtns & @intFromEnum(sdlwrapper.MouseBtn.LEFT) != 0) {
            const translation = zlm.vec4(@floatFromInt(-event.motion.xrel), @floatFromInt(-event.motion.yrel), 0, 0)
              .transform(zlm.Mat4.invert(toViewport) orelse unreachable)
              .transform(model.camera.from());

            model.camera.translate(translation);
            contextDirty = true;
          }
        },
        sdl.SDL_MOUSEBUTTONDOWN => {
          mousebtns |= @as(u3, 1) << @as(u2, @intCast(event.button.button - 1));
        },
        sdl.SDL_MOUSEBUTTONUP => {
          mousebtns ^= mousebtns & (@as(u3, 1) << @as(u2, @intCast((event.button.button - 1))));
        },
        sdl.SDL_MOUSEWHEEL => {
          model.camera.zoom(if (event.wheel.y > 0) 0.9 else 1.1);
          contextDirty = true;
        },
        else => {},
      }
    }

    if (contextDirty) {
      update(context, model);
      subsystem.drawImage(&context.buffer, 0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
      subsystem.renderScene();
      contextDirty = false;
    }

    // Only go for 60fps
    const timePerFrame = std.time.microTimestamp() - then;
    if (timePerFrame > 1000) {
      model.fps = model.fps * 0.9 + 1000000.0 / @as(f32, @floatFromInt(timePerFrame)) * 0.1;
    }
    const delay: i32 = 16 - @as(i32, @intCast(@divTrunc(timePerFrame, 1000)));
    sdl.SDL_Delay(if (delay > 0) @as(u32, @intCast(delay)) else 0);
  }
}
