// clear && zig build -freference-trace && echo "ready" && zig-out/bin/level ../../data/quake106/id1/pak0.pak "maps/start.bsp"
const std = @import("std");

const sdlwrapper = @import("sdl.zig");
const sdl = sdlwrapper.sdl;
const zlm = @import("zlm.zig").SpecializeOn(f32);

const draw = @import("draw.zig");
const pakModule = @import("pak.zig");
const bspModule = @import("bsp.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

const WIDTH: i16 = 640;
const HEIGHT: i16 = 480;
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

const Model = struct {
  fps: f32,
};

pub fn update(comptime Context: type, model: Model) void {
  var buffer_array: [100]u8 = undefined;
  const buffer: []u8 = buffer_array[0..];

  Context.color = 0xFF000000;
  Context.clearRect(0, 0, WIDTH, HEIGHT);
  Context.color = 0xFFFFFFFF;
  Context.save() catch unreachable;
  Context.reset();
  Context.printText(0, 0, std.fmt.bufPrint(buffer, "fps: {d}", .{ model.fps }) catch unreachable);
  Context.restore();
  Context.line(0, 0, WIDTH - 1, HEIGHT - 1);
  Context.line(WIDTH - 1, 0, 0, HEIGHT - 1);
}

const toCanvas = zlm.Mat4.invert(zlm.Mat4{
  .fields = [4][4]f32{
    [4]f32{ VIEWPORT_WIDTH / WIDTH,                         0, 0, 0 },
    [4]f32{                      0, -VIEWPORT_HEIGHT / HEIGHT, 0, 0 },
    [4]f32{                      0,                         0, 1, 0 },
    [4]f32{                      0,                         0, 0, 1 },
  },
}) orelse unreachable;

const toViewport: zlm.Mat4 = zlm.Mat4.invert(zlm.Mat4{
  .fields = [4][4]f32{
    [4]f32{   1,   0, 0, 0 },
    [4]f32{   0,  -1, 0, 0 },
    [4]f32{   0,   0, 1, 0 },
    [4]f32{   0,   0, 0, 1 },
  },
}) orelse unreachable;

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

  var subsystem = try sdlwrapper.SdlSubsystem.init(VIEWPORT_WIDTH, VIEWPORT_HEIGHT);
  defer subsystem.deinit();

  const context = draw.DrawContext(WIDTH, HEIGHT);
  context.setTransform(1, 0, 0, 1, 0, 0);

  var mousebtns: u3 = 0;
  const MouseBtn = enum(u3) {
    LEFT = 1,
    MIDDLE = 2,
    RIGHT = 4,
  };

  var model = Model { .fps = 0 };

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
          // std.log.debug("{}", .{ event.motion });
          if (mousebtns & @intFromEnum(MouseBtn.LEFT) != 0) {
            // std.log.debug("before {any} yrel {}", .{ context.getTransform(), event.motion.yrel });
            const translation = zlm.vec4(@floatFromInt(event.motion.xrel), @floatFromInt(event.motion.yrel), 0, 0)
              .transform(zlm.Mat4.invert(toViewport) orelse unreachable)
              .transform(toCanvas)
            ;
            context.translate(translation.x, translation.y);
            contextDirty = true;
            // std.log.debug("after {any}", .{ context.getTransform() });
          }
        },
        sdl.SDL_MOUSEBUTTONDOWN => {
          // std.log.debug("{}", .{ event.button });
          mousebtns |= @as(u3, 1) << @as(u2, @intCast(event.button.button - 1));
        },
        sdl.SDL_MOUSEBUTTONUP => {
          // std.log.debug("{}", .{ event.button });
          mousebtns ^= mousebtns & (@as(u3, 1) << @as(u2, @intCast((event.button.button - 1))));
        },
        else => {},
      }
    }

    if (contextDirty) {
      update(context, model);
      subsystem.drawImage(&context.buffer, 0, 0, WIDTH, HEIGHT);
      subsystem.renderScene();
      contextDirty = false;
    }

    // Only go for 60fps
    const timePerFrame = std.time.microTimestamp() - then;
    model.fps = 1000000.0 / @as(f32, @floatFromInt(timePerFrame));
    const delay: i32 = 16 - @as(i32, @intCast(@divTrunc(timePerFrame, 1000)));
    sdl.SDL_Delay(if (delay > 0) @as(u32, @intCast(delay)) else 0);
  }
}
