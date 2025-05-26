// QuakeC Compiler
//
// test: clear && zig run -freference-trace qcc.zig -- ../tests/quakec/hello.qc

const std = @import("std");
const misc = @import("misc.zig");
const clap = @import("clap.zig");
const qc_parser = @import("qc-parser.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

pub fn main() !u8 {
  var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = general_purpose_allocator.allocator();
  defer _ = general_purpose_allocator.deinit();

  const args = try std.process.argsAlloc(allocator);
  defer std.process.argsFree(allocator, args);

  const parsedArgs = clap.parser(clap.ArgDescriptor{
    .name = "qcc",
    .description = "A QuakeC compiler",
    .withHelp = true,
    .version = "0.1.0",
    .expectArgs = &[_][]const u8{ "qcfile" },
    .options = &[_]clap.OptionDescription{ .{
      .short = "p",
      .long = "pretty-print",
      .help = "Pretty print the parsed file",
    }, .{
      .short = "d",
      .long = "debug-print",
      .help = "Print the AST",
    } },
  }).parse(args);

  const filepath = parsedArgs.arguments.items[0];
  const buffer = misc.load(filepath) catch |err| {
    try stderr.print("error: {}, trying to open {s}\n", .{ err, filepath });
    std.posix.exit(1);
  };
  defer std.posix.munmap(buffer);

  var ast: [512]qc_parser.Ast.Node = undefined;
  var output: [4096:0]u8 = .{ 0 } ** 4096;
  var parser = qc_parser.Parser.init(buffer, ast[0..]);
  var err = qc_parser.GenericError{};
  parser.parse(&err) catch |e| {
    const n = try err.prettyPrint(&output);
    std.log.debug("{s}", .{ output[0..n] });
    return e;
  };

  if (parsedArgs.getSwitch("debug-print")) {
    var debug_str: [4096:0]u8 = .{ 0 } ** 4096;
    const debug_written = try ast[0].debugPrint(&ast, &debug_str, 0);
    try stdout.print("ast:\n{s}\n", .{ debug_str[0..debug_written] });
  }

  if (parsedArgs.getSwitch("pretty-print")) {
    const n = try ast[0].prettyPrint(&ast, &output);
    try stdout.print("{s}\n", .{ output[0..n] });
  }
  return 0;
}