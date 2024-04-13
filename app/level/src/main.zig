// clear && zig build -freference-trace && echo "ready" && zig-out/bin/level
const std = @import("std");
const draw = @import("draw.zig");
const sdlwrapper = @import("sdl.zig");
const sdl = sdlwrapper.sdl;
const zlm = @import("zlm.zig").SpecializeOn(f32);

const WIDTH: u16 = 640;
const HEIGHT: u16 = 320;

pub fn main() !void {
  var subsystem = try sdlwrapper.SdlSubsystem.init(640, 480);
  defer subsystem.deinit();

  const context = draw.DrawContext(WIDTH, HEIGHT);
  context.line(0, 0, WIDTH - 1, HEIGHT - 1);
  context.line(WIDTH - 1, 0, 0, HEIGHT - 1);
  subsystem.drawImage(&context.buffer, 0, 0, WIDTH, HEIGHT);

  var quit = false;
  while (!quit) {
    var event: sdl.SDL_Event = undefined;
    while (sdl.SDL_PollEvent(&event) != 0) {
      switch (event.type) {
        sdl.SDL_QUIT => { quit = true; },
        sdl.SDL_KEYDOWN => {
          switch (event.key.keysym.sym) {
            sdl.SDLK_ESCAPE => quit = true,
            else => {},
          }
        },
        else => {},
      }
      subsystem.renderScene();
    }
  }
}
