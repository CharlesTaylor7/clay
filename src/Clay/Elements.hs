{-# LANGUAGE OverloadedStrings #-}
module Clay.Elements where

import Data.String

import Clay.Selector

-- From: http://www.whatwg.org/specs/web-apps/current-work/multipage/section-index.html#index

-- | Special cases, these items occur both as an HTML tag and an HTML
-- attribute. We keep them polymorph.

abbr, cite, command, data_, form, label, span, style, title :: IsString a => a

abbr = "abbr"
cite = "cite"
command = "command"
data_ = "data"
form = "form"
label = "label"
span = "span"
style = "style"
title = "title"

a, address, area, article, aside, audio, b, base, bdi, bdo, blockquote,
  body, br, button, canvas, caption, code, col, colgroup, datalist, dd, del,
  details, dfn, dialog, div, dl, dt, em, embed, fieldset, figcaption, figure,
  footer, h1, h2, h3, h4, h5, h6, head, header, hgroup, hr, html, i, iframe,
  img, input, ins, kbd, keygen, legend, li, link, map, mark, menu, meta, meter,
  nav, noscript, object, ol, optgroup, option, output, p, param, pre, progress,
  q, rp, rt, ruby, s, samp, script, section, select, small, source, strong,
  sub, summary, sup, table, tbody, td, textarea, tfoot, th, thead, time, tr,
  track, u, ul, var, video, wbr :: Selector

a = "a"
address = "address"
area = "area"
article = "article"
aside = "aside"
audio = "audio"
b = "b"
base = "base"
bdi = "bdi"
bdo = "bdo"
blockquote = "blockquote"
body = "body"
br = "br"
button = "button"
canvas = "canvas"
caption = "caption"
code = "code"
col = "col"
colgroup = "colgroup"
datalist = "datalist"
dd = "dd"
del = "del"
details = "details"
dfn = "dfn"
dialog = "dialog"
div = "div"
dl = "dl"
dt = "dt"
em = "em"
embed = "embed"
fieldset = "fieldset"
figcaption = "figcaption"
figure = "figure"
footer = "footer"
h1 = "h1"
h2 = "h2"
h3 = "h3"
h4 = "h4"
h5 = "h5"
h6 = "h6"
head = "head"
header = "header"
hgroup = "hgroup"
hr = "hr"
html = "html"
i = "i"
iframe = "iframe"
img = "img"
input = "input"
ins = "ins"
kbd = "kbd"
keygen = "keygen"
legend = "legend"
li = "li"
link = "link"
map = "map"
mark = "mark"
menu = "menu"
meta = "meta"
meter = "meter"
nav = "nav"
noscript = "noscript"
object = "object"
ol = "ol"
optgroup = "optgroup"
option = "option"
output = "output"
p = "p"
param = "param"
pre = "pre"
progress = "progress"
q = "q"
rp = "rp"
rt = "rt"
ruby = "ruby"
s = "s"
samp = "samp"
script = "script"
section = "section"
select = "select"
small = "small"
source = "source"
strong = "strong"
sub = "sub"
summary = "summary"
sup = "sup"
table = "table"
tbody = "tbody"
td = "td"
textarea = "textarea"
tfoot = "tfoot"
th = "th"
thead = "thead"
time = "time"
tr = "tr"
track = "track"
u = "u"
ul = "ul"
var = "var"
video = "video"
wbr = "wbr"

