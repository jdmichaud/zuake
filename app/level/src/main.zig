const sdlwrapper = @import("sdl.zig");
const sdl = sdlwrapper.sdl;

pub fn main() !void {
  const subsystem = try sdlwrapper.init(640, 480);
  defer sdlwrapper.deinit(subsystem);

  var buffer = [_]u32 { 0 } ** (640*480);
  buffer[100] = 0xFFFFFFFF;
  sdlwrapper.prepareScene(subsystem, &buffer);
  sdlwrapper.renderScene(subsystem);

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
    }
  }
}
