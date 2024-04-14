#!/usr/local/bin/python 
## 
##
##     QC2HTML Copyright (c) 1996, Olivier Montanuy 
##          Version 1.0,  August 1st, 1996
##
##
## Run this Python program in a directory, with no arguments. 
## It will look for file "progs.src", read it, and then process all
## .QC (Quake C) files into HTML, with anchors for functions.
## It assumes that file builtin.htm exists, and contains definitions
## for the Quake C buit-in types, variables, functions.
##
## About Python:
##  see http://www.python.org/
##  Basically it's a (slow) scripting language, that can run on
##  Unix, Linux, Windows 95 (I recommend PythonWin there).
##
## Technical info: 
##  This program isn't a true Quake C analyser. It works with regular 
##  expressions,so it's slow as hell, and might bug in some cases. 
##  Go rewrite it in CAML or C (yacc) if you have time to waste.
##
## Disclaimer:
##  Use at your own risk. No liabilities. None at all. Even toward *you*.
##  Don't try to understand that code. You have been warned. If your brain
##  fries, or you die laughting, I'm not feeling concerned.
##

## bugs: fileds are not tagged correctly

## file where the list of files shall be found
ProgsSrc = "progs.src"
## Pattern of the files to transform
##   <anything>.qc
TransPattern = "\(\w[a-zA-Z0-9_]*[.]qc\)"
## Quake-C  keywords
KeywordList = [ 'do', 'else', 'for', 'if', 'local', 'return', 'while' ]
##
## Built-in Required functions, or documented function:
##
## use a tag <a name="f_xxxx">xxxx</a> to define
## them, where xxxx is the name of the function.
## qc2html will look for that tag in the KnownAnchor
## files, and link the function definition to it.
## That's an easy way to document your functions.
##
##
## Declare all the files that contain known anchors
##  Those anchors will be found according to pattern <a name="...">
##  To define anchors not to be referenced by quake-C, use <A NAME="..."> 
#KnownAnchors = [ ... , "manual.htm" ]
KnownAnchors = ["qc-mdl.htm", "qc-types.htm", "qc-defs.htm", "qc-enty.htm", 
                "qc-glob.htm", "qc-built.htm", "qc-net.htm", ]

#
##
## User defined functions
##
#  TransTableAdd(expc.group(1)) #add to translation table
# declare expc.group(1) as an anchor location
def UNewAnchor1(expc, infos):
  infos.Anchs.Declare( expc.group(1))
  return ""
## declare a variable  
def UVariable123(expc, infos):
  space = expc.group(1)
  if space == None: space = ""
  typ= expc.group(2)
  name = expc.group(3)
  # this is a hack, because return val; is confused with a declaration
  if typ == "return": 
    return expc.group(0) #everything 
  # check if variable is known
  if infos.Anchs.IsKnown(typ):
    typ = infos.Anchs.Href( typ, typ )
  if infos.Anchs.IsKnown(name): 
    name = infos.Anchs.Href( name, name)  
  else:
    infos.Anchs.Declare(name)
    name = "<a name=\"" + name + "\">" + name + "</a>"
  return space + typ + " <b>" + name + "</b>;"
## declare a constant
def UConstant1234(expc, infos):
  space = expc.group(1)
  if space == None: space = ""
  typ= expc.group(2)
  name = expc.group(3)
  value = expc.group(4)
  if not infos.Anchs.IsKnown(name):
    infos.Anchs.Declare(name)
    name = "<a name=\"" + name + "\">" + name + "</a>"
  if infos.Anchs.IsKnown(typ):
    typ = infos.Anchs.Href( typ, typ )
  return space + typ + " <b>" + name + "</b> = " + value + ";"
## transform expc.group(1)
def UToken1(expc, infos):
  token = expc.group(1)
  if infos.Keys.IsKeyword(token):
    return "<b>"+token+"</b>"
  elif infos.Anchs.IsKnown(token):
    return infos.Anchs.Href(token, token)
  return token
## define a new function
def UFunction1(expc, infos):
  token = expc.group(1)
  infos.Anchs.Declare(token)  # declare function
  # required functions have anchor = "f_xxxx"
  fname = "f_"+token
  if infos.Anchs.Required.has_key(fname): #required
    func = infos.Anchs.Href(fname, token) 
  else:                       # normal function
    func = token 
  return ") <b>"+ func + "</b><a name=\""+token+"\">=</a>"
## pre-declare a new function
def UFDeclare1(expc, infos):
  token = expc.group(1)
  infos.Anchs.PreDeclare(token) # pre-declare function
  func = infos.Anchs.Href(token, token) # point to function
  return ") " + func + ";"
# declare a ".xxxx" stuff
def UDotField12(expc, infos):
  field = expc.group(1)
  paren = expc.group(2)
  if paren == None:  paren = ""
  hack = "dot_" + field
  if infos.Anchs.IsKnown(hack):
    field =  infos.Anchs.Href(hack, field)
  return "." + field + paren
# declare a new dollar definition (for models)
def UDollar12(expc, infos):
  token = expc.group(1) # definition
  data = expc.group(2)  # data of definition
  hack = "s_"+token     # anchor is s_xxxx
  anchor = infos.Anchs.Href(hack, token) # anchor "$"
  return "<b>$</b>" + anchor + " <b>" + data + "</b>"
##
## Translation table = [ (Exp, Sub ,FFun), ... ]
##
##  Exp = regular expression to be matched, with groups
##  Sub = substituted regular expression, with groups 
##  FFun = None or User-defined function to execute on expression
##
TransDef = [
# Strings:   anything in quotes, is highlighted
( "\"\(.*\)\"",  
  "<b>\"\\1\"</b>",  None ),
# Comments: 
# ex:   // anything
( "//\(.*\)\n", 
  "<i>//\\1</i>\n",   None),
# Comments
# ex:   /* anything */
( "/[*]\([^*]*\)[*]/", 
  "<i>/*\\1*/</i>\n",   None),
# < and >
( "<", "&lt;", None),
( ">", "&gt;", None),
( "&", "&amp;", None),
# dot field 
( "[.]\([a-zA-Z_][a-zA-Z0-9_]*\)\([ \t]*()\)?",
  None, UDotField12),
# builtin function definition
# ex: ") scv_xxx = #"
( ")[ _t]*\(\w[a-zA-Z0-9_]*\)[ \t]*=[ \t]*#[ \t]*\([0-9]+\)[ \t]*;",
  ") <b>\\1</b> = #<b>\\2</b>;", None),
# function definition  
# ex:   ") boss_missile ="
( ")[ \t]*\(\w[a-zA-Z0-9_]*\)[ \t]*=",
  None, UFunction1),
# function declaration
# ex:   ") movetarget_f;"
( ")[ \t]*\(\w[a-zA-Z0-9_]*\)[ \t]*;",
  None, UFDeclare1),
# variable declaration
# ex:   "float current_yaw;"
( "^\([ \t]*\)\(\w[a-zA-Z0-9_]*\)[ \t]+\(\w[a-zA-Z0-9_]*\)[ \t]*;",
  None, UVariable123),
# constant declaration
# ex:   "float ATTN_STATIC = 3;"
( "^\([ \t]*\)\(\w[a-zA-Z0-9_]*\)[ \t]+\(\w[a-zA-Z0-9_]*\)[ \t]*=[ \t]*\(.*\);",
  None, UConstant1234),
# definition
# ex: $skin skin 
( "^$\(\w[a-zA-Z0-9_]*\)[ \t]+\(.*\)",
  None, UDollar12),
# token
( "\\b\([a-zA-Z0-9_][a-zA-Z0-9_---]*\)\\b",
  None,  UToken1) ] 
## RegExp
## "\w+"        = one or more word characters
## "\(\w+\)"    = a word in a group (group(1), group(2), etc...)
## "\\b"        = a word boundary
## " *"         = zero or more spaces

#
# Html Header
#
def TransHtmlHead(fileName):
 res = "<html><head><title>" + fileName + "</title></head>"
 res = res + "<body bgcolor=\"#C0F0D0\">\n"
 res = res + "<base target=content>\n<pre>\n" 
 return res 
#
# Html Trailer
#
def TransHtmlTrail(fileName):
 return "\n</pre></body></html>"

######################################################################
# Do not edit after this line
######################################################################
import regex
import regsub
import string
import fnmatch
import os

##
## Save a file
##  info = (file, txt)
def FileWrite(info):
  try:
    (file, txt) = info
    fp = open(file,"w")
    fp.write(txt)
    fp.close()
    return info
  except:
    print "Can't write file ", file
    return None
##
## Read a file
## file = filename
def FileRead(file):
  try: 
    fp=open(file,"r")
    txt=fp.read() #
    fp.close()
    return txt
  except:
    print "Can't read file ", file
    return None
#
# convert extension of file name to .html 
#
def FileHtml(file):
  (root,ext)=os.path.splitext(file)
  return root + ".htm"

##
## KeyWords
##
class KeyWords:
   # IsKeyword(self, token)
   Keys= {}
   def __init__(self, keylist):
     self.Keys= {}
     for k in keylist:
       self.Keys[k] = "" # declare all keywords
     return
   # return 1 if token exists
   def IsKeyword(self, token):
     if self.Keys.has_key(token):
       return 1
     return 0
##
## Modules
##
class Modules:
  # Declaration of the paths of the modules
  #  m = Modules()		# create an instance of class Modules
  #  mod = m.DeclarePath(file)	# declare new module file, return module name
  #  m.DeclareBasePath(path)	# set base path
  #  file = m.GetFile(mod)	# get root file of a given module (no extension)
  # Inquiries about module usage:
  #  m.NoneUsed()		# clear off module used
  #  m.Used(mod)		# declare module is used in the current file
  #  bool = m.IsUsed(mod)	# return 1 if module exists
  # Hidden variables:
  # self._mods = { "moduleName":"modulePath", ...}
  # self._curr ={ "moduleName":1, ...}
  # self._base = "basePathForAllModules"
  ##
  ## m = Modules()
  ##
  def __init__(self):
    self._mods = {}  # dictionary of known modules     
    self._curr = {} # dictionary of current modules
    self._base = ""
    return
  ##
  ## m.DeclarePath( file)
  ##   file = "/directory/file.ext"
  def DeclarePath(self, file):
    (root, ext) = os.path.splitext(file) # get extension
    (path, mod) = os.path.split(root)    # get path
    self._mods[mod]= path # declare
    return mod # return module name
  ##
  ## m.DeclareBasePath( file)
  ##  path = "/directory/"
  def DeclareBasePath(self,file):
    ( self._base, name) = os.path.split(file)
    return
  ##
  ## file= m.GetFile(mod)
  ##  file = "/directory/file"  no extension
  def GetFile(self, mod):
    if not self._mods.has_key(mod):
      return ""
    path= os.path.join(self._base,self._mods[mod])
    return os.path.join(path, mod)
  # m.NoneUsed()
  #   declare no modules are used yet
  def NoneUsed(self):
    self._curr = {}
    return
  # m.Used(mod)
  #   declare that module is used
  def Used(self, mod):
    self._curr[mod] = 1
  # m.IsUsed(mod)
  #   check if module is used
  def IsUsed(self, mod):
    return self._curr.has_key(token)
##
## Anchors
##
class Anchors:
  # a =Anchors(KnownAnchors)
  # a.DeclareModule(mod)	# declare name of current module
  # a.Declare(anchor)    	# declare an achor
  # bool = a.IsKnown(anchor)	# returns true if it is an anchor
  # file = a.GetFile(anchor)    # return Href of anchor
  # hidden variables
  # dictionary of anchors 
  #_known = {} # {"name":"module", ...}
  # duplicated anchors
  #_dupli = {} # {"name":[ "module1", "module2", ...], ...}
  # name of current module
  _currentMod ="" # "moduleName"
  # anchor pattern
  _apatn = regex.compile("<a[ \t]+name=\"\(\w[a-zA-Z0-9_---]*\)\">")
  def __init__(self, knownanchor):
    self.Required = {}
    self.Mods = Modules()
    self._known = {}
    self._dupli = {}
    # declare all known anchors
    #for kn in known:
    #  (file, anchlist) = kn 
    #  self.DeclareModule(file)
    #  for anch in anchlist:
    #    self.Declare(anch)
    #  self._currentMod = ""
    for file in knownanchor:
      self.LookForAnchors(file)
    return
  # a.LookForAnchors(file)
  #  try to find known anchors in file
  def LookForAnchors(self, file):
    text = FileRead(file)
    if text == None: # can't read file
      return
    self.DeclareModule(file)
    print "Looking for anchors in ", file
    pos = 0
    while(1):
      pos = self._apatn.search(text, pos)
      if pos<0: break
      pos = pos + len(self._apatn.group(0))
#      print "   Found: ", self._apatn.group(1)
      anchor = self._apatn.group(1)
      # 
      self.Declare(anchor)
      # hack: declare a required function
      if anchor[0:2] == "f_":
        self.Required[anchor] = 1
    self._currentMod = ""
    return
  # a.DeclareModule(mod)
  #  mod = name of current module
  def DeclareModule(self, file):
    self._currentMod = self.Mods.DeclarePath(file)
    return
  # a.Declare(anchor)
  #  anchor = string, name of an anchor detected
  #   Declare anchor, only if not known already
  def Declare(self, anchor):
    if self._currentMod == None  or self._currentMod == "":
      print "ERROR no current module"
      return
    # detect duplicate anchors
    if self._known.has_key(anchor): #  
      if self._known[anchor] != "":  # not a  predeclared anchor
        if not self._dupli.has_key(anchor): self._dupli[anchor] = []
        self._dupli[anchor].append( self._currentMod )
        return
    # declare anchor, only if not known
    self._known[anchor] = self._currentMod
  # a.PreDeclare(anchor)
  #  anchor = string, name of an anchor detected
  # Pre-declare anchor
  def PreDeclare(self, anchor):
    # do not pre-declare if already known
    if self._known.has_key(anchor): return
    # pre-declare anchor, only if not known
    self._known[anchor] = ""
  ## bool= a.IsKnown(anchor)
  ##  return 1 if anchor is really an achor
  def IsKnown(self, anchor):
    return self._known.has_key(anchor)
  ##
  ## file = a.GetFile(anchor)
  ##  file = file reference, for anchor, suitable for use in <A HREF="">
  def GetFile(self, anchor): 
    if not self._known.has_key(anchor):
      print "ERROR unknown anchor: ", anchor
      return ""
    mod = self._known[anchor] # get module name
    return FileHtml(self.Mods.GetFile(mod)) # get file name
  ##
  ## anchor = a.Href(anchor, title) 
  ##   <a href=" file(anchor) # anchor "> title </a>
  def Href(self, anchor, title):
    #return "<a href=\"" + self.GetFile(anchor) + "#" + pointer + "\">" + title + "</a>"
    return "<a href=\"" + "@-@%@-@" + anchor + "@-@%@-@"+ "#" + anchor + "\">" + title + "</a>"
  def LateResolve(self, text):
    liste = string.splitfields(text, "@-@%@-@")
    for l in xrange(1, len(liste), 2):
      liste[l]= self.GetFile(liste[l]) # anchor
    return string.joinfields(liste,"")
##
## Translation table: [ (ExpC, Sub ,FFun, Start), ... ]
##
class Translater:
  ExpC = None # compiler regular expression
  Sub = ""    # substitution string
  FFun = None # function to execute
  Start = 0   # 0 if invalid, -1 if no more position
  Size = 0    # size of match
  ##
  ## Initialise
  ##
  def __init__(self, t):
    (exp , sub, ffun) = t
    self.ExpC = regex.compile(exp)
    self.Sub = sub
    self.FFun = ffun
    self.Reset()
    return
  def Reset(self):
    self.Start = 0
    self.Size = 0
  ##
  ## find next occurence
  ##
  def FindNext(self, text, pos):
    if self.Start < 0:    # no more patterns of that kind
      return -1
    if (self.Start > 0)&(self.Start>=pos):   # position is valid
      return self.Start
    else:
      self.Start = self.ExpC.search(text,pos)  
      if self.Start >= 0:
        self.Size = self.ExpC.match(text,self.Start)
      return self.Start  #last posisition found
  ##
  ## apply function
  ##
  def ApplyFunction(self, infos):
    if self.FFun == None:
      return ""
    return self.FFun(self.ExpC, infos)
  ##
  ## translate some text
  ##
  def Translate(self, text):
    if self.Sub == None:
      return ""
    return regsub.sub(self.ExpC.realpat, self.Sub, text)   
##
## Add an anchor to translation table
##
def TransTableAdd(word):
  t=Translater( ("\\b\("+word+"\)\\b", "<A HREF=\"#\\1\">\\1</A>", None))
  TransTable.append(t)
  return
##
## Reset the translation table
##
def TransTableReset():
  for translater in TransTable:
    translater.Reset()
  return
# 
  
## Compile translation table from TransDef
#
TransTable = map(Translater,TransDef)



  
##
## Infos structure for functions
##
class Transform:
  FileName = ""  # Obtain the current File name from here
  BasePath = ""
  LineNumber = 0 # Obtain the current Line Number from here
  def __init__(self, file):
    # declare known keywords, for user-defined functions
    self.Keys = KeyWords(KeywordList)
    # declare anchors, for user-defined functions
    self.Anchs = Anchors(KnownAnchors)
    liste = self._getFileList(file)
    self.TransFileList(liste)
    return
  ##
  ## infos._getFileList()
  ##  find, in file,  a list of files to parse
  def _getFileList(self, file): 
    print "Looking for file definitions in %s" % file
    txt = FileRead(file)
    # find file names according to TransPattern
    patn = regex.compile(TransPattern) 
    liste=[]
    pos = 0
    while 1:
      pos = patn.search(txt,pos)
      if pos<0: break
      pos = pos+len(patn.group(0))
      name = patn.group(1)
      print "found ", name
      liste.append( name)
    # save list in HTM  
    res = []
    res.append("<html><head><title>")
    res.append("Modules of Quake-C")
    res.append("</title></head><body>")
    res.append("<base target=content>")
    res.append("<h2>Quake-C modules</h2>")
    res.append("<small>Generated by <a href=\"qc2html.py\">qc2html.py</a><br>")
    res.append("Underpowered by <a href=\"http://www.python.org\">Python</a>.</small>")
    res.append("<p><ul>")
    for name in liste:
      (root, ext) = os.path.splitext(name) 
      res.append("<li> <a href=\"" + FileHtml(root) + "\">" + name + "</a>")
    res.append("</ul></p>")
    res.append("</body></html>")
    file = self.SaveHtmlRaw(file, string.joinfields(res,"\n"))
    # create a void
    #res = []
    #res.append("<html><body>")
    #res.append("<h2>Quake-C</h2>")
    #res.append("</body></html>")
    #vide = self.SaveHtmlRaw("void", string.joinfields(res,"\n"))
    # create a frame
    res = []
    res.append("<frameset cols=\"30%,*\">")
    res.append("  <frame src=\""+file+"\" name=menu>")
    res.append("  <frame src=\"qc-menu.htm\" name=content>")
    res.append("</frameset>")
    res.append("<noframes>")
    res.append("<h2>Your browser is not frame capable (shame)!</h2>")
    res.append("<p>Maybe you would like the <a href=\""+ file + "\">No frame version</a>.</p>")
    res.append("</noframes>")
    self.SaveHtmlRaw("index", string.joinfields(res,"\n"))   
    # transform all the files in the list
    return liste
  ##
  ## res = self.TransformText(text)
  ##  res= Transformed text
  ##
  def TransformText(self, text):
    #
    # Translate text
    #
    TransTableReset()	# Clear search indexes in translation table
    liste = []		# clear result
    pos = 0 		# current position
    end = 0 		# current length  
    textlen = len(text) # length of text
    while end < textlen:
      # find closest regular expression in translation table
      pos = textlen
      mintranslater = None
      for translater in TransTable:
        if translater.FindNext(text, end) < 0 : # no more patterns of that kind
          continue #next translater
        if translater.Start < pos: # check is closest pattern
          #print " pos = ", translater.Start, " for ", translater.Sub
          pos = translater.Start
          mintranslater = translater
      # Preserve text that wasn't matched by regular expressions
      liste.append( text[end:pos] )
      # Add result of regular expressions
      if pos >= textlen:  # no regular expressions were matched
        end = textlen 	  # jump to end of text
      else:               # one regular expression matched
        translater = mintranslater  
        # find end of matched area
        end = pos + translater.Size
        # text[pos:end] = matched expression
        # transform the matched area, with a function
        res = translater.ApplyFunction(self)
        # transform the matched area, with regsub
        res = res + translater.Translate(text[pos:end])
	# append result
        liste.append(res)
    return string.joinfields(liste,"") # faster than + on strings
  ##
  ## Transform a list of files
  ##
  def TransFileList(self, liste):
    #
    print "Transforming all files..."
    processed = []
    for name in liste:
      if name == None or len(name)<1: 
        continue
      file = name
      print "Parsing File", file
      try:
        text= FileRead(file)
      except:
        print "Can't read file " + file 
      if text != None:
        #declare module
        self.LineNumber = 0
        self.FileName = file
        self.BasePath = ""
        # make module current, for anchors
        self.Anchs.DeclareModule(file)
        # transform
        text = self.TransformText(text)
        processed.append((file, text))
#        self.SaveHtml(file, text) # temporary file
    # resolve anchors, once everything is processed
    for pro in processed:
      (file, text) = pro
      text = self.Anchs.LateResolve(text)
      self.SaveHtml(file, text)
    return
  def SaveHtmlRaw(self, file, text):
    file = FileHtml(file)
    print "Saving file: ", file
    try:
      FileWrite( (file , text)) 
    except:
      print "Can't write file " + file
    return file
  def SaveHtml(self, file, text):
    res = TransHtmlHead(file) + text + TransHtmlTrail(file)
    return self.SaveHtmlRaw(file, res)
#
    
#
# Parse all the files
#   
print "Parsing files..."
infos = Transform(ProgsSrc)
