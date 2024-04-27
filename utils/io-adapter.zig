// https://zig.news/david_vanderson/interfaces-in-zig-o1c
const std = @import("std");
pub const sdl = @cImport({
  @cInclude("SDL2/SDL.h");
});

pub const MouseMove = struct {
  x: i32,
  y: i32,
  dx: i32,
  dy: i32,
};

pub const MouseButton = enum(u8) {
  Left = 1,
  Center = 2,
  Right = 3,
};

pub const MouseButtonEvent = struct {
  button: MouseButton,
};

pub const MouseWheel = struct {
  y: i32,
};

// From SDL_Scancode
// These values are from usage page 0x07 (USB keyboard page).
pub const Scancode = enum(u32) {
  A = 4,
  B = 5,
  C = 6,
  D = 7,
  E = 8,
  F = 9,
  G = 10,
  H = 11,
  I = 12,
  J = 13,
  K = 14,
  L = 15,
  M = 16,
  N = 17,
  O = 18,
  P = 19,
  Q = 20,
  R = 21,
  S = 22,
  T = 23,
  U = 24,
  V = 25,
  W = 26,
  X = 27,
  Y = 28,
  Z = 29,

  K1 = 30,
  K2 = 31,
  K3 = 32,
  K4 = 33,
  K5 = 34,
  K6 = 35,
  K7 = 36,
  K8 = 37,
  K9 = 38,
  K0 = 39,

  RETURN = 40,
  ESCAPE = 41,
  BACKSPACE = 42,
  TAB = 43,
  SPACE = 44,

  MINUS = 45,
  EQUALS = 46,
  LEFTBRACKET = 47,
  RIGHTBRACKET = 48,
  BACKSLASH = 49, // < Located at the lower left of the return
                               //   key on ISO keyboards and at the right end
                               //   of the QWERTY row on ANSI keyboards.
                               //   Produces REVERSE SOLIDUS (backslash) and
                               //   VERTICAL LINE in a US layout, REVERSE
                               //   SOLIDUS and VERTICAL LINE in a UK Mac
                               //   layout, NUMBER SIGN and TILDE in a UK
                               //   Windows layout, DOLLAR SIGN and POUND SIGN
                               //   in a Swiss German layout, NUMBER SIGN and
                               //   APOSTROPHE in a German layout, GRAVE
                               //   ACCENT and POUND SIGN in a French Mac
                               //   layout, and ASTERISK and MICRO SIGN in a
                               //   French Windows layout.
                               //
  NONUSHASH = 50, // < ISO USB keyboards actually use this code
                               //   instead of 49 for the same key, but all
                               //   OSes I've seen treat the two codes
                               //   identically. So, as an implementor, unless
                               //   your keyboard generates both of those
                               //   codes and your OS treats them differently,
                               //   you should generate BACKSLASH
                               //   instead of this code. As a user, you
                               //   should not rely on this code because SDL
                               //   will never generate it with most (all?)
                               //   keyboards.
                               //
  SEMICOLON = 51,
  APOSTROPHE = 52,
  GRAVE = 53, // < Located in the top left corner (on both ANSI
                           //   and ISO keyboards). Produces GRAVE ACCENT and
                           //   TILDE in a US Windows layout and in US and UK
                           //   Mac layouts on ANSI keyboards, GRAVE ACCENT
                           //   and NOT SIGN in a UK Windows layout, SECTION
                           //   SIGN and PLUS-MINUS SIGN in US and UK Mac
                           //   layouts on ISO keyboards, SECTION SIGN and
                           //   DEGREE SIGN in a Swiss German layout (Mac:
                           //   only on ISO keyboards), CIRCUMFLEX ACCENT and
                           //   DEGREE SIGN in a German layout (Mac: only on
                           //   ISO keyboards), SUPERSCRIPT TWO and TILDE in a
                           //   French Windows layout, COMMERCIAL AT and
                           //   NUMBER SIGN in a French Mac layout on ISO
                           //   keyboards, and LESS-THAN SIGN and GREATER-THAN
                           //   SIGN in a Swiss German, German, or French Mac
                           //   layout on ANSI keyboards.
                           //
  COMMA = 54,
  PERIOD = 55,
  SLASH = 56,

  CAPSLOCK = 57,

  F1 = 58,
  F2 = 59,
  F3 = 60,
  F4 = 61,
  F5 = 62,
  F6 = 63,
  F7 = 64,
  F8 = 65,
  F9 = 66,
  F10 = 67,
  F11 = 68,
  F12 = 69,

  PRINTSCREEN = 70,
  SCROLLLOCK = 71,
  PAUSE = 72,
  INSERT = 73, // < insert on PC, help on some Mac keyboards (but
                            //   does send code 73, not 117)
  HOME = 74,
  PAGEUP = 75,
  DELETE = 76,
  END = 77,
  PAGEDOWN = 78,
  RIGHT = 79,
  LEFT = 80,
  DOWN = 81,
  UP = 82,

  NUMLOCKCLEAR = 83, // < num lock on PC, clear on Mac keyboards

  KP_DIVIDE = 84,
  KP_MULTIPLY = 85,
  KP_MINUS = 86,
  KP_PLUS = 87,
  KP_ENTER = 88,
  KP_1 = 89,
  KP_2 = 90,
  KP_3 = 91,
  KP_4 = 92,
  KP_5 = 93,
  KP_6 = 94,
  KP_7 = 95,
  KP_8 = 96,
  KP_9 = 97,
  KP_0 = 98,
  KP_PERIOD = 99,

  NONUSBACKSLASH = 100, // < This is the additional key that ISO
                                     //   keyboards have over ANSI ones,
                                     //   located between left shift and Y.
                                     //   Produces GRAVE ACCENT and TILDE in a
                                     //   US or UK Mac layout, REVERSE SOLIDUS
                                     //   (backslash) and VERTICAL LINE in a
                                     //   US or UK Windows layout, and
                                     //   LESS-THAN SIGN and GREATER-THAN SIGN
                                     //   in a Swiss German, German, or French
                                     //   layout.
  APPLICATION = 101, // < windows contextual menu, compose
  POWER = 102, // < The USB document says this is a status flag,
                            //   not a physical key - but some Mac keyboards
                            //   do have a power key.
  KP_EQUALS = 103,
  F13 = 104,
  F14 = 105,
  F15 = 106,
  F16 = 107,
  F17 = 108,
  F18 = 109,
  F19 = 110,
  F20 = 111,
  F21 = 112,
  F22 = 113,
  F23 = 114,
  F24 = 115,
  EXECUTE = 116,
  HELP = 117,    // < AL Integrated Help Center
  MENU = 118,    // < Menu (show menu)
  SELECT = 119,
  STOP = 120,    // < AC Stop
  AGAIN = 121,   // < AC Redo/Repeat
  UNDO = 122,    // < AC Undo
  CUT = 123,     // < AC Cut
  COPY = 124,    // < AC Copy
  PASTE = 125,   // < AC Paste
  FIND = 126,    // < AC Find
  MUTE = 127,
  VOLUMEUP = 128,
  VOLUMEDOWN = 129,
// not sure whether there's a reason to enable these
//     LOCKINGCAPSLOCK = 130,
//     LOCKINGNUMLOCK = 131,
//     LOCKINGSCROLLLOCK = 132,
  KP_COMMA = 133,
  KP_EQUALSAS400 = 134,

  INTERNATIONAL1 = 135, // < used on Asian keyboards, see
                                     //   footnotes in USB doc
  INTERNATIONAL2 = 136,
  INTERNATIONAL3 = 137, // < Yen
  INTERNATIONAL4 = 138,
  INTERNATIONAL5 = 139,
  INTERNATIONAL6 = 140,
  INTERNATIONAL7 = 141,
  INTERNATIONAL8 = 142,
  INTERNATIONAL9 = 143,
  LANG1 = 144, // < Hangul/English toggle
  LANG2 = 145, // < Hanja conversion
  LANG3 = 146, // < Katakana
  LANG4 = 147, // < Hiragana
  LANG5 = 148, // < Zenkaku/Hankaku
  LANG6 = 149, // < reserved
  LANG7 = 150, // < reserved
  LANG8 = 151, // < reserved
  LANG9 = 152, // < reserved

  ALTERASE = 153,    // < Erase-Eaze
  SYSREQ = 154,
  CANCEL = 155,      // < AC Cancel
  CLEAR = 156,
  PRIOR = 157,
  RETURN2 = 158,
  SEPARATOR = 159,
  OUT = 160,
  OPER = 161,
  CLEARAGAIN = 162,
  CRSEL = 163,
  EXSEL = 164,

  KP_00 = 176,
  KP_000 = 177,
  THOUSANDSSEPARATOR = 178,
  DECIMALSEPARATOR = 179,
  CURRENCYUNIT = 180,
  CURRENCYSUBUNIT = 181,
  KP_LEFTPAREN = 182,
  KP_RIGHTPAREN = 183,
  KP_LEFTBRACE = 184,
  KP_RIGHTBRACE = 185,
  KP_TAB = 186,
  KP_BACKSPACE = 187,
  KP_A = 188,
  KP_B = 189,
  KP_C = 190,
  KP_D = 191,
  KP_E = 192,
  KP_F = 193,
  KP_XOR = 194,
  KP_POWER = 195,
  KP_PERCENT = 196,
  KP_LESS = 197,
  KP_GREATER = 198,
  KP_AMPERSAND = 199,
  KP_DBLAMPERSAND = 200,
  KP_VERTICALBAR = 201,
  KP_DBLVERTICALBAR = 202,
  KP_COLON = 203,
  KP_HASH = 204,
  KP_SPACE = 205,
  KP_AT = 206,
  KP_EXCLAM = 207,
  KP_MEMSTORE = 208,
  KP_MEMRECALL = 209,
  KP_MEMCLEAR = 210,
  KP_MEMADD = 211,
  KP_MEMSUBTRACT = 212,
  KP_MEMMULTIPLY = 213,
  KP_MEMDIVIDE = 214,
  KP_PLUSMINUS = 215,
  KP_CLEAR = 216,
  KP_CLEARENTRY = 217,
  KP_BINARY = 218,
  KP_OCTAL = 219,
  KP_DECIMAL = 220,
  KP_HEXADECIMAL = 221,

  LCTRL = 224,
  LSHIFT = 225,
  LALT = 226, // < alt, option
  LGUI = 227, // < windows, command (apple), meta
  RCTRL = 228,
  RSHIFT = 229,
  RALT = 230, // < alt gr, option
  RGUI = 231, // < windows, command (apple), meta

  MODE = 257,    // < I'm not sure if this is really not covered
                              //   by any of the above, but since there's a
                              //   special KMOD_MODE for it I'm adding it here
                              //
};

pub const Keycode = enum(u8) {
  None,
};

pub const KeyEvent = struct {
  mod: u16,
  scancode: Scancode,
  keycode: Keycode,
};

pub const EventType = enum {
  MouseMove,
  MouseClick,
  MouseDown,
  MouseUp,
  MouseWheel,
  KeyDown,
};

pub const InputEvent = union(EventType) {
  MouseMove: MouseMove,
  MouseClick: MouseButtonEvent,
  MouseDown: MouseButtonEvent,
  MouseUp: MouseButtonEvent,
  MouseWheel: MouseWheel,
  KeyDown: KeyEvent,
};

pub const IOAdapter = struct {
  getEventFn: *const fn (*IOAdapter) ?InputEvent,
  drawImageFn: *const fn (*IOAdapter, []const u32, u16, u16, u16, u16) void,
  renderSceneFn: *const fn (*IOAdapter) void,

  pub fn getEvent(adapter: *IOAdapter) ?InputEvent {
    return adapter.getEventFn(adapter);
  }

  pub fn drawImage(adapter: *IOAdapter, image: []const u32, sx: u16, sy: u16, sWidth: u16, sHeight: u16) void {
    return adapter.drawImageFn(adapter, image, sx, sy, sWidth, sHeight);
  }

  pub fn renderScene(adapter: *IOAdapter) void {
    return adapter.renderSceneFn(adapter);
  }
};

pub const SDLAdapter = struct {
  const Self = @This();

  window: *sdl.SDL_Window,
  texture: *sdl.SDL_Texture,
  renderer: *sdl.SDL_Renderer,
  width: usize,
  height: usize,
  // tv: std.posix.timeval,

  interface: IOAdapter,

  pub fn init(width: u16, height: u16) anyerror!Self {
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

    return Self{
      .window = window,
      .texture = texture,
      .renderer = renderer,
      .width = width,
      .height = height,

      .interface = IOAdapter {
        .getEventFn = getEvent,
        .drawImageFn = drawImage,
        .renderSceneFn = renderScene,
      },
    };
  }

  pub fn deinit(self: *Self) void {
    sdl.SDL_DestroyRenderer(self.renderer);
    sdl.SDL_DestroyTexture(self.texture);
    sdl.SDL_DestroyWindow(self.window);
    sdl.SDL_Quit();
  }

  pub fn getEvent(adapter: *IOAdapter) ?InputEvent {
    const self: *SDLAdapter = @fieldParentPtr("interface", adapter);
    _ = self;

    var event: sdl.SDL_Event = undefined;
    if (sdl.SDL_PollEvent(&event) != 0) {
      switch (event.type) {
        sdl.SDL_KEYDOWN => {
          return InputEvent{
            .KeyDown = KeyEvent{
              .mod = 0,
              .scancode = @enumFromInt(event.key.keysym.scancode),
              .keycode = Keycode.None,
            },
          };
        },
        sdl.SDL_MOUSEMOTION => {
          return InputEvent{
            .MouseMove = MouseMove{
              .x = event.motion.x,
              .y = event.motion.y,
              .dx = event.motion.xrel,
              .dy = event.motion.yrel,
            },
          };
        },
        sdl.SDL_MOUSEBUTTONDOWN => {
          return InputEvent{ .MouseDown = MouseButtonEvent{ .button = @enumFromInt(event.button.button) } };
        },
        sdl.SDL_MOUSEBUTTONUP => {
          return InputEvent{ .MouseUp = MouseButtonEvent{ .button = @enumFromInt(event.button.button) } };
        },
        sdl.SDL_MOUSEWHEEL => {
          return InputEvent{ .MouseWheel = MouseWheel{ .y = event.wheel.y } };
        },
        else => {},
      }
    }

    return null;
  }

  pub fn drawImage(adapter: *IOAdapter, image: []const u32, sx: u16, sy: u16, sWidth: u16, sHeight: u16) void {
    const self: *SDLAdapter = @fieldParentPtr("interface", adapter);

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
    } else if (sWidth * 2 == self.width and sHeight * 2 == self.height) {
      @setRuntimeSafety(false); // Too slow otherwise
      for (0..self.height) |j| {
        const offset = j * self.width;
        const sourceOffset = j / 2 * sWidth;
        for (0..self.width) |i| {
          const imageIndex: usize = sourceOffset + i / 2;
          buffer[offset + i] = image[imageIndex];
        }
      }
      @setRuntimeSafety(true);
    } else {
      @setRuntimeSafety(false); // Too slow otherwise
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
      @setRuntimeSafety(true);
    }

    sdl.SDL_UnlockTexture(texture);
  }

  pub fn renderScene(adapter: *IOAdapter) void {
    const self: *SDLAdapter = @fieldParentPtr("interface", adapter);

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
};