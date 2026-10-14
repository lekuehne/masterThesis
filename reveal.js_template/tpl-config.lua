-- Reads colors / logo / author from the YAML header and exposes them to CSS and JS.
local function str(v, default)
  if v == nil then return default end
  return pandoc.utils.stringify(v)
end

local function js(s)
  s = s:gsub("\\", "\\\\")
  s = s:gsub('"', '\\"')
  return '"' .. s .. '"'
end

function Meta(m)
  local c = m.colors or {}
  local css = string.format(
    ":root{--primary:%s;--secondary:%s;--text:%s;--bg:%s;}",
    str(c.primary, "#1f3a5f"), str(c.secondary, "#e6ebf2"),
    str(c.text, "#1b1f24"), str(c.background, "#ffffff"))
  quarto.doc.include_text("in-header", "<style>" .. css .. "</style>")

  local author = str(m["footer-author"], nil)
  if author == nil then
    local ok, a = pcall(function() return str(m.author[1].name.literal, "") end)
    author = ok and a or ""
  end

  local cfg = string.format(
    "<script>window.TPL={logo:%s,author:%s,titleImage:%s};</script>",
    js(str(m.logo, "")), js(author), js(str(m["title-image"], "")))
  quarto.doc.include_text("after-body", cfg)
end
