const std = @import("std");

pub const sdl = @cImport({
  @cInclude("SDL2/SDL.h");
});

pub const SdlSubsystem = struct {
  const Self = @This();

  window: *sdl.SDL_Window,
  texture: *sdl.SDL_Texture,
  renderer: *sdl.SDL_Renderer,

  width: u16,
  height: u16,

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
      .width = width,
      .height = height,
    };
  }

  pub fn drawImage(self: *Self, image: []u32, sx: u16, sy: u16, sWidth: u16, sHeight: u16) void {
    const texture = self.texture;

    var buffer: [*c]u32 = undefined;
    var pitch: i32 = undefined;
    const res = sdl.SDL_LockTexture(texture, null, @as([*c]?*anyopaque, @ptrCast(&buffer)), &pitch);
    if (res < 0) {
      sdl.SDL_Log("Unable to lock texture: %s", sdl.SDL_GetError());
      std.posix.exit(0);
    }

    if (sx == 0 and sy == 0 and sWidth == self.width and sHeight == self.height) {
      std.mem.copyForwards(u32, buffer[0..image.len], image);
    } else {
      const ifactor: f32 = @as(f32, @floatFromInt(sWidth)) / @as(f32, @floatFromInt(self.width));
      const jfactor: f32 = @as(f32, @floatFromInt(sHeight)) / @as(f32, @floatFromInt(self.height));
      for (0..self.height) |j| {
        const offset = j * self.width;
        const sourceOffset = @as(usize, @intFromFloat(@as(f32, @floatFromInt(j)) * jfactor)) * sWidth;
        for (0..self.width) |i| {
          const imageIndex: usize = sourceOffset + @as(usize, @intFromFloat(@as(f32, @floatFromInt(i)) * ifactor));
          buffer[offset + i] = image[imageIndex];
        }
      }
    }

    sdl.SDL_UnlockTexture(texture);
  }

  pub fn renderScene(self: *Self) void {
    if (sdl.SDL_SetRenderDrawColor(self.renderer, 0x00, 0x00, 0x00, 0xFF) < 0) {
      sdl.SDL_Log("Unable to draw color: %s", sdl.SDL_GetError());
      std.posix.exit(0);
    }
    if (sdl.SDL_RenderClear(self.renderer) < 0) {
      sdl.SDL_Log("Unable to clear: %s", sdl.SDL_GetError());
      std.posix.exit(0);
    }
    // blit the surface
    if (sdl.SDL_RenderCopy(self.renderer, self.texture, null, null) < 0) {
      sdl.SDL_Log("Unable to copy texture: %s", sdl.SDL_GetError());
      std.posix.exit(0);
    }
    sdl.SDL_RenderPresent(self.renderer);
  }

  pub fn deinit(self: *Self) void {
    sdl.SDL_DestroyRenderer(self.renderer);
    sdl.SDL_DestroyTexture(self.texture);
    sdl.SDL_DestroyWindow(self.window);
    sdl.SDL_Quit();
  }
};
