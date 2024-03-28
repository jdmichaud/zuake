const std = @import("std");

pub const sdl = @cImport({
  @cInclude("SDL2/SDL.h");
});

const SdlSubsystem = struct {
  window: *sdl.SDL_Window,
  texture: *sdl.SDL_Texture,
  renderer: *sdl.SDL_Renderer,
};

pub fn init(width: u16, height: u16) anyerror!SdlSubsystem {
  if (sdl.SDL_Init(sdl.SDL_INIT_VIDEO) != 0) {
    sdl.SDL_Log("Unable to initialize SDL: %s", sdl.SDL_GetError());
    return error.SDLInitializationFailed;
  }
  errdefer sdl.SDL_Quit();

  const window = sdl.SDL_CreateWindow("level", sdl.SDL_WINDOWPOS_UNDEFINED,
    sdl.SDL_WINDOWPOS_UNDEFINED, width, height, sdl.SDL_WINDOW_OPENGL) orelse {
    sdl.SDL_Log("Unable to create window: %s", sdl.SDL_GetError());
    return error.SDLInitializationFailed;
  };
  errdefer sdl.SDL_DestroyWindow(window);

  const renderer = sdl.SDL_CreateRenderer(window, -1, 0) orelse {
    sdl.SDL_Log("Unable to create renderer: %s", sdl.SDL_GetError());
    return error.SDLInitializationFailed;
  };
  errdefer sdl.SDL_DestroyRenderer(renderer);

  const texture = sdl.SDL_CreateTexture(renderer, sdl.SDL_PIXELFORMAT_RGBA32,
    sdl.SDL_TEXTUREACCESS_STREAMING, width, height) orelse {
    sdl.SDL_Log("Unable to create texture: %s", sdl.SDL_GetError());
    return error.SDLInitializationFailed;
  };
  errdefer sdl.SDL_DestroyTexture(texture);

  return SdlSubsystem {
    .window = window,
    .texture = texture,
    .renderer = renderer,
  };
}

pub fn prepareScene(sdl_subsystem: SdlSubsystem, image: []u32) void {
  const texture = sdl_subsystem.texture;

  var buffer: [*c]u32 = undefined;
  var pitch: i32 = undefined;
  const res = sdl.SDL_LockTexture(texture, null, @as([*c]?*anyopaque, @ptrCast(&buffer)), &pitch);
  if (res < 0) {
    sdl.SDL_Log("Unable to lock texture: %s", sdl.SDL_GetError());
    std.posix.exit(0);
  }

  std.mem.copyForwards(u32, buffer[0..image.len], image);

  sdl.SDL_UnlockTexture(texture);
}

pub fn renderScene(sdl_subsystem: SdlSubsystem) void {
  if (sdl.SDL_SetRenderDrawColor(sdl_subsystem.renderer, 0x00, 0x00, 0x00, 0xFF) < 0) {
    sdl.SDL_Log("Unable to draw color: %s", sdl.SDL_GetError());
    std.posix.exit(0);
  }
  if (sdl.SDL_RenderClear(sdl_subsystem.renderer) < 0) {
    sdl.SDL_Log("Unable to clear: %s", sdl.SDL_GetError());
    std.posix.exit(0);
  }
  // blit the surface
  if (sdl.SDL_RenderCopy(sdl_subsystem.renderer, sdl_subsystem.texture, null, null) < 0) {
    sdl.SDL_Log("Unable to copy texture: %s", sdl.SDL_GetError());
    std.posix.exit(0);
  }
  sdl.SDL_RenderPresent(sdl_subsystem.renderer);
}

pub fn deinit(sdl_subsystem: SdlSubsystem) void {
  sdl.SDL_DestroyRenderer(sdl_subsystem.renderer);
  sdl.SDL_DestroyTexture(sdl_subsystem.texture);
  sdl.SDL_DestroyWindow(sdl_subsystem.window);
  sdl.SDL_Quit();
}

