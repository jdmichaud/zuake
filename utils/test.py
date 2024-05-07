from ctypes import *

class MipTexture(Structure):
  _pack_ = 1
  _fields_ = [
    ("name", c_char * 16),
    ("width", c_uint32),
    ("height", c_uint32),
    ("offset1", c_uint32),
    ("offset2", c_uint32),
    ("offset4", c_uint32),
    ("offset8", c_uint32),
    ("pixels", c_char_p),
  ]

def s(cstring):
  return c_char_p(bytes(cstring, "utf8"))

quake = CDLL("./libpython.so")
quake.loadBsp.argtypes = (c_char_p,)
quake.loadBsp.restype = c_void_p
quake.getMipTextures.argtypes = (c_void_p, c_uint64)
quake.getMipTextures.restype = POINTER(MipTexture)
# quake.getMipTextures.restype = c_void_p

bsp = quake.loadBsp(b"../data/pak/maps/start.bsp")
texture = quake.getMipTextures(bsp, 1);
print(texture.contents.name, texture.contents.width, texture.contents.height)
