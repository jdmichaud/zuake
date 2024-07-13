// Loads the end screen and display it on a unix terminal.
//
// reference:
//  https://www.gamers.org/dEngine/quake/spec/quake-spec34/index0.htm
//   3.6 The end screen (.BIN)
//   A classical DOS text screen, 80x25 with color tags. Same as the end screen in DOOM.
//  https://en.wikipedia.org/wiki/VGA_text_mode
//  https://en.wikipedia.org/wiki/Code_page_437
//
// cmd: clear && zig build-exe -freference-trace -OReleaseSmall bin.zig && ./bin ../data/pak/end1.bin

const std = @import("std");
const misc = @import("misc.zig");

const stdout = std.io.getStdOut().writer();

const code_page_457 = [_][]const u8 {
  "?",  "☺", "☻",  "♥", "♦", "♣", "♠", "•", "◘", "○", "◙", "♂", "♀",  "♪", "♫", "☼",
  "►",  "◄", "↕",  "‼", "¶", "§", "▬", "↨", "↑", "↓", "→", "←", "∟",  "↔", "▲", "▼",
  " ", "!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",",  "-", ".", "/",
  "0",  "1", "2",  "3", "4", "5", "6", "7", "8", "9", ":", ";", "<",  "=", ">", "?",
  "@",  "A", "B",  "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",  "M", "N", "O",
  "P",  "Q", "R",  "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_",
  "`",  "a", "b",  "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",  "m", "n", "o",
  "p",  "q", "r",  "s", "t", "u", "v", "w", "x", "y", "z", "{", "|",  "}", "~", "⌂",
  "Ç",  "ü", "é",  "â", "ä", "à", "å", "ç", "ê", "ë", "è", "ï", "î",  "ì", "Ä", "Å",
  "É",  "æ", "Æ",  "ô", "ö", "ò", "û", "ù", "ÿ", "Ö", "Ü", "¢", "£",  "¥", "₧", "ƒ",
  "á",  "í", "ó",  "ú", "ñ", "Ñ", "ª", "º", "¿", "⌐", "¬", "½", "¼",  "¡", "«", "»",
  "░",  "▒", "▓",  "│", "┤", "╡", "╢", "╖", "╕", "╣", "║", "╗", "╝",  "╜", "╛", "┐",
  "└",  "┴", "┬",  "├", "─", "┼", "╞", "╟", "╚", "╔", "╩", "╦", "╠",  "═", "╬", "╧",
  "╨",  "╤", "╥",  "╙", "╘", "╒", "╓", "╫", "╪", "┘", "┌", "█", "▄",  "▌", "▐", "▀",
  "α",  "ß", "Γ",  "π", "Σ", "σ", "µ", "τ", "Φ", "Θ", "Ω", "δ", "∞",  "φ", "ε", "∩",
  "≡",  "±", "≥",  "≤", "⌠", "⌡", "÷", "≈", "°", "∙", "·", "√", "ⁿ",  "²", "■", " ",
};

// https://lospec.com/palette-list/4-bit-rgb
// Not the greatest palette
const vga_color_palette = [_]std.io.tty.Color {
  .black, .green, .green,   .bright_green,
  .blue,  .blue,  .cyan,    .cyan,
  .red,   .red,   .yellow,  .yellow,
  .red,   .red,   .red,     .white,
};

pub fn displayBin(writer: anytype, buffer: []const u8) !void {
  const ttyconf = std.io.tty.detectConfig(writer);
  for (0..25) |row| {
    for (0..80) |col| {
      std.debug.assert((row * 80 + col) * 2 + 1 < buffer.len);
      const codepoint = buffer[(row * 80 + col) * 2    ];
      const attribute = buffer[(row * 80 + col) * 2 + 1];

      const palette_index = attribute & 0x0F;
      try ttyconf.setColor(stdout, vga_color_palette[palette_index]);
      if (attribute & 0x80 != 0) { // Supposed to be bling
        try ttyconf.setColor(stdout, .bold);
      }
      try stdout.print("{s}", .{ code_page_457[codepoint] });
    }
    try stdout.print("\n", .{});
  }
}

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
    try stdout.print("    {s} <bspfile> - Show bsp content\n", .{ args[0] });
    return;
  }

  const buffer = try misc.load(args[1]);
  defer std.posix.munmap(buffer);

  try displayBin(std.io.getStdOut(), buffer);
}
