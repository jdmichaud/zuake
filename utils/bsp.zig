// clear && zig build-exe -freference-trace bsp.zig && ./bsp ../data/pak/maps/start.bsp
const std = @import("std");

const stdout = std.io.getStdOut().writer();

fn load(pathname: []const u8) ![]align(1096) const u8 {
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

// The BSP29 file format is as follows:
// A file version on 4 bytes (little-endian integer)
// 15 lumps
//   each lumps contains:
//     - An offset on 4 bytes (little-endian integer)
//     - A size on 4 bytes (little-endian integer)
// Lumps are in the following order:
//  0. entities
//  1. planes
//  2. miptextures
//  3. vertexes
//  4. visibilities
//  5. nodes
//  6. textureInfos
//  7. faces
//  8. lighting
//  9. clipNodes
// 10. leafs
// 11. mark_surfaces
// 12. edges
// 13. surf_edges
// 14. models

const LumpHeader = extern struct {
  offset: u32,
  size: u32,
};

const BspHeader = extern struct {
  version: u32,                  // Model version, must be 0x17 (23).
  entities: LumpHeader,          // List of Entities.
  planes: LumpHeader,            // Map Planes.
                                 // numplanes = size/sizeof(plane_t)
  miptextures: LumpHeader,       // Wall Textures.
  vertices: LumpHeader,          // Map Vertices.
                                 // numvertices = size/sizeof(vertex_t)
  visibilities: LumpHeader,      // Leaves Visibility lists.
  nodes: LumpHeader,             // BSP Nodes.
                                 // numnodes = size/sizeof(node_t)
  textureInfo: LumpHeader,       // Texture Info for faces.
                                 // numtexinfo = size/sizeof(texinfo_t)
  faces: LumpHeader,             // Faces of each surface.
                                 // numfaces = size/sizeof(face_t)
  lightmaps: LumpHeader,         // Wall Light Maps.
  clipNodes: LumpHeader,         // clip nodes, for Models.
                                 // numclips = size/sizeof(clipnode_t)
  leaves: LumpHeader,            // BSP Leaves.
                                 // numlaves = size/sizeof(leaf_t)
  lfaces: LumpHeader,            // List of Faces.
  edges: LumpHeader,             // Edges of faces.
                                 // numedges = Size/sizeof(edge_t)
  ledges: LumpHeader,            // List of Edges.
  models: LumpHeader,            // List of Models.
                                 // nummodels = Size/sizeof(model_t)
};

const Entities = struct {
  data: []const u8,

  pub fn decode(bsp: []const u8, lumpHeader: LumpHeader) !Entities {
    return .{
      .data = bsp[lumpHeader.offset..lumpHeader.offset + lumpHeader.size],
    };
  }
};

const PlaneType = enum(u32) {
   XPLANE = 0, // Axial plane aligned to the x-axis.
   YPLANE = 1, // Axial plane aligned to the y-axis.
   ZPLANE = 2, // Axial plane aligned to the z-axis.
  OXPLANE = 3, // Non-axial plane roughly aligned to the x-axis.
  OYPLANE = 4, // Non-axial plane roughly aligned to the y-axis.
  OZPLANE = 5, // Non-axial plane roughly aligned to the z-axis.
};

const Plane = extern struct {
  normal: [3]f32,
  distance: f32,
  type: PlaneType,
};

const MipTexturesHeader = extern struct {
  numtex: u32,                 // Number of textures in Mip Texture list
  offset: [0]u32,              // Offset to each of the individual texture

  // Returns the offset of the mipTextures relative to the beginning of the file
  pub fn getOffsets(self: *align(1) const MipTexturesHeader) []u32 {
    return @as([*]u32, &self.offset)[0..self.numtex];
  }
};

pub const MipTexture = extern struct {
  name: [16] u8,               // Name of the texture.
  width: u32,                  // width of picture, must be a multiple of 8
  height: u32,                 // height of picture, must be a multiple of 8
  // All these offset are relative to the beginning of the MipTexture struct
  offset1: u32,                // offset to u_char Pix[width   * height]
  offset2: u32,                // offset to u_char Pix[width/2 * height/2]
  offset4: u32,                // offset to u_char Pix[width/4 * height/4]
  offset8: u32,                // offset to u_char Pix[width/8 * height/8]
  // To get the size of the pixels array, divide width and height by 2 four
  // times and you get floor(width*height*85/64).
  pixels: [*]u8,
};

const Vertex = extern struct {
  X: f32,                       // X,Y,Z coordinates of the vertex
  Y: f32,                       // usually some integer value
  Z: f32,                       // but coded in floating point
};

const Node = extern struct {
  planeId: i32,                 // The plane that splits the node
                                //           must be in [0,numplanes[
  front: u16,                   // If bit15==0, index of Front child node
                                // If bit15==1, ~front = index of child leaf
  back: u16,                    // If bit15==0, id of Back child node
                                // If bit15==1, ~back =  id of child leaf
  boundingBoxMinX: i16,
  boundingBoxMinY: i16,
  boundingBoxMinZ: i16,
  boundingBoxMaxX: i16,
  boundingBoxMaxY: i16,
  boundingBoxMaxZ: i16,
  // bboxshort_t box,              // Bounding box of node and all childs
  face_id: u16,                 // Index of first Polygons in the node
  face_num: u16,                // Number of faces in the node
};

const vec3 = extern struct {
  x: f32,
  y: f32,
  z: f32,
};

const TextureInfo = extern struct {
  vectorS: vec3,               // S vector, horizontal in texture space)
  distS: f32,                  // horizontal offset in texture space
  vectorT: vec3,               // T vector, vertical in texture space
  distT: f32,                  // vertical offset in texture space
  textureId: u32,              // Index of Mip Texture
                               //           must be in [0,numtex[
  animated: u32,               // 0 for ordinary textures, 1 for water
};

const LightType = enum(u8) {
  LIGHT_MAP = 0,               // To be used with a lightmap
  FAST_PULSATING = 1,          // produces a fast pulsating light
  SLOW_PULSATING = 2,          // produces a slow pulsating light
  UNKNOWN_3 = 3,               // ??
  UNKNOWN_4 = 4,               // ??
  UNKNOWN_5 = 5,               // ??
  UNKNOWN_6 = 6,               // ??
  UNKNOWN_7 = 7,               // ??
  UNKNOWN_8 = 8,               // ??
  UNKNOWN_9 = 9,               // ??
  UNKNOWN_10 = 10,             // ??
  NO_LIGHT_MAP = 0xFF,         // is to be used when there is no light map.
};

const Face = extern struct {
  planeId: u16,                // The plane in which the face lies
                               //           must be in [0,numplanes[
  side: u16,                   // 0 if in front of the plane, 1 if behind the plane
  ledgeId: i32,                // first edge in the List of edges
                               //           must be in [0,numledges[
  ledgeNum: u16,               // number of edges in the List of edges
  texinfoId: u16,              // index of the Texture info the face is part of
                               //           must be in [0,numtexinfos[
  typelight: LightType,        // type of lighting, for the face
  baselight: u8,               // from 0xFF (dark) to 0 (bright)
                               // minimum light level for the light map,
                               // or the constant light level in the absence of light map
  light: [2]u8,                // two additional light models
  lightmap: i32,               // Pointer inside the general light map, or -1
                               // this define the start of the face light map
};

const ClipNode = extern struct {
  planeId: u32,                // The plane which splits the node
  front: i16,                  // If positive, id of Front child node
                               // If -2, the Front part is inside the model
                               // If -1, the Front part is outside the model
  back: i16,                   // If positive, id of Back child node
                               // If -2, the Back part is inside the model
                               // If -1, the Back part is outside the model
};

const LeafType = enum(i32) {
  ORDINARY = -1,// ordinary leaf
  SOLID = -2,// the leaf is entirely inside a solid (nothing is displayed).
  WATER = -3,// Water, the vision is troubled.
  SLIME = -4,// Slime, green acid that hurts the player.
  LAVA = -5,// Lava, vision turns red and the player is badly hurt.
  SKY = -6,// Behaves like water, but is used for sky.
};

const Leaf = extern struct {
  type: LeafType,              // Special type of leaf
  vislist: i32,                // Beginning of visibility lists
                               //     must be -1 or in [0,numvislist[
  boundingBoxMinX: i16,
  boundingBoxMinY: i16,
  boundingBoxMinZ: i16,
  boundingBoxMaxX: i16,
  boundingBoxMaxY: i16,
  boundingBoxMaxZ: i16,
  // bboxshort_t bound,           // Bounding box of the leaf
  lfaceId: u16,                // First item of the list of faces
                               //     must be in [0,numlfaces[
  lfaceNum: u16,               // Number of faces in the leaf
  sndwater: u8,                // level of the four ambient sounds:
  sndsky: u8,                  //   0    is no sound
  sndslime: u8,                //   0xFF is maximum volume
  sndlava: u8,                 //
};

const Edge = extern struct {
  vertex0: u16,                // index of the start vertex
                               //  must be in [0,numvertices[
  vertex1: u16,                // index of the end vertex
                               //  must be in [0,numvertices[
};

const BoundBox = extern struct { // Bounding Box, Float values
  min: vec3,                   // minimum values of X,Y,Z
  max: vec3,                   // maximum values of X,Y,Z
};

const Model = extern struct {
  bound: BoundBox,             // The bounding box of the Model
  origin: vec3,                // origin of model, usually (0,0,0)
  nodeId0: i32,                // index of first BSP node
  nodeId1: i32,                // index of the first Clip node
  nodeId2: i32,                // index of the second Clip node
  nodeId3: i32,                // usually zero
  numleafs: i32,               // number of BSP leaves
  faceId: i32,                 // index of Faces
  faceNum: i32,                // number of Faces
};

fn loadLumpArray(comptime T: type, bsp: []const u8,
  lumpHeader: LumpHeader) ![]align(1) const T {
  const ts: [*]align(1) const T = @alignCast(@ptrCast(&bsp[lumpHeader.offset]));
  const nbT = lumpHeader.size / @sizeOf(T);
  return ts[0..nbT];
}

fn loadMipTextures(allocator: std.mem.Allocator, bsp: []const u8,
  mipTexturesHeader: *align(1) const MipTexturesHeader) ![]*align(1) const MipTexture {
  var textures: []*align(1) const MipTexture = try allocator.alloc(*align(1) const MipTexture, mipTexturesHeader.numtex);
  var i: usize = 0;
  while (i < mipTexturesHeader.numtex) {
    const mipTextureOffset = @intFromPtr(mipTexturesHeader) - @intFromPtr(bsp.ptr) + mipTexturesHeader.getOffsets()[i];
    textures[i] = @alignCast(@ptrCast(&bsp[mipTextureOffset]));
    i += 1;
  }
  return textures[0..mipTexturesHeader.numtex];
}

pub const Bsp = struct {
  const Self = @This();

  header: BspHeader,
  entities: Entities,
  planes: []align(1) const Plane,
  mipTexturesHeader: *align(1) const MipTexturesHeader,
  mipTextures: []*align(1) const MipTexture,
  vertices: []align(1) const Vertex,
  nodes: []align(1) const Node,
  textureInfos: []align(1) const TextureInfo,
  faces: []align(1) const Face,
  clipNodes: []align(1) const ClipNode,
  lfaces: []align(1) const u16,
  ledges: []align(1) const i16,
  edges: []align(1) const Edge,
  leaves: []align(1) const Leaf,
  models: []align(1) const Model,

  pub fn init(allocator: std.mem.Allocator, buffer: []const u8) !Bsp {
    const bspHeader: *align(1) const BspHeader = @alignCast(@ptrCast(&buffer[0]));
    if (bspHeader.version != 29) {
      @panic("not a bps29 file");
    }
    const bsp = try decodeBsp(allocator, bspHeader.*, buffer);
    return bsp;
  }

  pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.mipTextures);
  }
};

pub fn decodeBsp(allocator: std.mem.Allocator, header: BspHeader, bsp: []const u8) !Bsp {
  const entities = try Entities.decode(bsp, header.entities);
  const planes = try loadLumpArray(Plane, bsp, header.planes);
  const mipTexturesHeader: *align(1) const MipTexturesHeader = @alignCast(@ptrCast(&bsp[header.miptextures.offset]));
  const mipTextures = try loadMipTextures(allocator, bsp, mipTexturesHeader);
  const vertices = try loadLumpArray(Vertex, bsp, header.vertices);
  const nodes = try loadLumpArray(Node, bsp, header.nodes);
  {
    var i: usize = 0;
    while (i < nodes.len) {
      std.debug.assert(nodes[i].boundingBoxMinX < nodes[i].boundingBoxMaxX);
      std.debug.assert(nodes[i].boundingBoxMinY < nodes[i].boundingBoxMaxY);
      std.debug.assert(nodes[i].boundingBoxMinZ < nodes[i].boundingBoxMaxZ);
      std.debug.assert(nodes[i].planeId < planes.len);
      i += 1;
    }
  }
  const textureInfos = try loadLumpArray(TextureInfo, bsp, header.textureInfo);
  {
    var i: usize = 0;
    while (i < textureInfos.len) {
      std.debug.assert(textureInfos[i].textureId < mipTextures.len);
      std.debug.assert(textureInfos[i].animated == 0 or textureInfos[i].animated == 1);
      i += 1;
    }
  }
  const faces = try loadLumpArray(Face, bsp, header.faces);
  {
    var i: usize = 0;
    while (i < faces.len) {
      std.debug.assert(faces[i].planeId < planes.len);
      std.debug.assert(faces[i].texinfoId < textureInfos.len);
      std.debug.assert(@intFromEnum(faces[i].typelight) == 0xFF or
        (0 <= @intFromEnum(faces[i].typelight) and @intFromEnum(faces[i].typelight) <= 10));
      i += 1;
    }
  }
  const clipNodes = try loadLumpArray(ClipNode, bsp, header.clipNodes);
  {
    var i: usize = 0;
    while (i < clipNodes.len) {
      std.debug.assert(clipNodes[i].planeId < planes.len);
      std.debug.assert(clipNodes[i].front >= -2);
      std.debug.assert(clipNodes[i].back >= -2);
      i += 1;
    }
  }
  const lfaces = try loadLumpArray(u16, bsp, header.lfaces);
  const leaves = try loadLumpArray(Leaf, bsp, header.leaves);
  {
    var i: usize = 0;
    while (i < leaves.len) {
      std.debug.assert(leaves[i].lfaceId + leaves[i].lfaceNum <= lfaces.len);
      i += 1;
    }
  }
  const edges = try loadLumpArray(Edge, bsp, header.edges);
  {
    var i: usize = 0;
    while (i < edges.len) {
      std.debug.assert(edges[i].vertex0 < vertices.len);
      std.debug.assert(edges[i].vertex1 < vertices.len);
      i += 1;
    }
  }
  const ledges = try loadLumpArray(i16, bsp, header.ledges);
  const models = try loadLumpArray(Model, bsp, header.models);
  {
    var i: usize = 0;
    while (i < models.len) {
      std.debug.assert(models[i].nodeId0 < nodes.len);
      std.debug.assert(models[i].nodeId1 < clipNodes.len);
      std.debug.assert(models[i].nodeId2 < clipNodes.len);
      std.debug.assert(models[i].nodeId3 < clipNodes.len);
      std.debug.assert(models[i].numleafs < leaves.len);
      std.debug.assert(models[i].faceId + models[i].faceNum <= faces.len);
      i += 1;
    }
  }

  return .{
    .header = header,
    .entities = entities,
    .planes = planes,
    .mipTexturesHeader = mipTexturesHeader,
    .mipTextures = mipTextures,
    .vertices = vertices,
    .nodes = nodes,
    .textureInfos = textureInfos,
    .faces = faces,
    .clipNodes = clipNodes,
    .lfaces = lfaces,
    .ledges = ledges,
    .edges = edges,
    .leaves = leaves,
    .models = models,
  };
}

fn prettyprint(bsp: Bsp, nodeId: u16, level: u16, w: anytype) !void {
  _ = w;
  {
    // var i: u16 = 0;
    // while (i < level) {
    //   try w.writeAll(" ");
    //   i += 1;
    // }
    if (bsp.nodes[nodeId].front & 0x8000 == 0) {
      // try w.print("front node {}\n", .{ bsp.nodes[nodeId].front });
      try prettyprint(bsp, bsp.nodes[nodeId].front, level + 1, stdout);
    } else {
      std.log.debug("{}", .{ level });
      // try w.print("front leaf {} type {}\n", .{ ~bsp.nodes[nodeId].front, bsp.leaves[~bsp.nodes[nodeId].front].type });
    }
  }
  {
    // var i: u16 = 0;
    // while (i < level) {
    //   try w.writeAll(" ");
    //   i += 1;
    // }
    if (bsp.nodes[nodeId].back & 0x8000 == 0) {
      // try w.print("back node {}\n", .{ bsp.nodes[nodeId].back });
      try prettyprint(bsp, bsp.nodes[nodeId].back, level + 1, stdout);
    } else {
      std.log.debug("{}", .{ level });
      // try w.print("back leaf {} type {}\n", .{ ~bsp.nodes[nodeId].back, bsp.leaves[~bsp.nodes[nodeId].back].type });
    }
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

  const buffer = try load(args[1]);
  defer std.posix.munmap(buffer);

  const bspHeader: *const BspHeader = @ptrCast(&buffer[0]);
  if (bspHeader.version != 29) {
    @panic("not a bps29 file");
  }
  std.log.debug("version: {}\n", .{ bspHeader.version });

  const bsp = try decodeBsp(allocator, bspHeader.*, buffer);
  defer allocator.free(bsp.mipTextures);

  std.log.debug("entities: {}K loaded", .{ bsp.entities.data.len / 1024 });
  std.log.debug("planes: {} loaded", .{ bsp.planes.len });
  std.log.debug("mipTextures: {} loaded", .{ bsp.mipTextures.len });
  std.log.debug("vertices: {} loaded", .{ bsp.vertices.len });
  std.log.debug("visibility: 0x{x} ({}) ignored", .{ bsp.header.visibilities.offset, bspHeader.visibilities.size });
  std.log.debug("nodes: {} loaded", .{ bsp.nodes.len });
  std.log.debug("textureInfos: {} loaded", .{ bsp.textureInfos.len });
  std.log.debug("faces: {} loaded", .{ bsp.faces.len });
  std.log.debug("light map: 0x{x} ({}) ignored", .{ bsp.header.lightmaps.offset, bspHeader.lightmaps.size });
  std.log.debug("clipNodes: {} loaded", .{ bsp.clipNodes.len });
  std.log.debug("lfaces: {} loaded", .{ bsp.lfaces.len });
  std.log.debug("leaves: {} loaded", .{ bsp.leaves.len });
  std.log.debug("edges: {} loaded", .{ bsp.edges.len });
  std.log.debug("ledges: {} loaded", .{ bsp.ledges.len });
  std.log.debug("models: {} loaded", .{ bsp.models.len });
  std.log.debug("level: {any}", .{ bsp.models[0] });

  std.log.debug("{}", .{ bsp.models[0].nodeId0 });
  // var timer = try std.time.Timer.start();
  // const then = timer.read();
  // try prettyprint(bsp, @intCast(bsp.models[0].nodeId0), 0, stdout);
  // try stdout.print("{d}ms\n", .{ @as(f32, @floatFromInt(timer.read() - then)) / 1000000 });
}
