-- Collects all figure captions and inserts copies at the #figure-caps div.
-- Set `hide-figures: true` in YAML front matter to ghost body figures in PDF.

local figure_captions = {}
local fig_count = 0
local in_appendix = false
local hide_figures = false  -- default: show figures normally

return {
  -- Pass 0: read hide-figures from document metadata
  {
    Meta = function(meta)
      hide_figures = meta["hide-figures"] == true
    end
  },
  -- Pass 1: collect captions; emit \figureghostmodefalse at appendix
  {
    Header = function(el)
      if el.classes:includes("appendix") then
        in_appendix = true
        if hide_figures and quarto.doc.isFormat("latex") then
          return pandoc.List({
            pandoc.RawBlock("latex", "\\figureghostmodefalse\n"),
            el
          })
        end
      end
    end,
    FloatRefTarget = function(el)
      if in_appendix or el.type ~= "Figure" then return end
      if el.caption_long then
        fig_count = fig_count + 1
        local prefix = pandoc.List({
          pandoc.Strong({pandoc.Str("Figure " .. fig_count .. ".")}),
          pandoc.Space()
        })
        local inlines = prefix .. pandoc.List(el.caption_long.content)
        table.insert(figure_captions, pandoc.Para(inlines))
      end
    end
  },
  -- Pass 2: if not hiding, disable ghost mode at the top of the document body
  {
    Pandoc = function(doc)
      if not hide_figures and quarto.doc.isFormat("latex") then
        table.insert(doc.blocks, 1, pandoc.RawBlock("latex", "\\figureghostmodefalse\n"))
      end
      return doc
    end
  },
  -- Pass 3: replace #figure-caps div with collected captions
  {
    Div = function(div)
      if div.identifier == "figure-caps" then
        return pandoc.Div(figure_captions, div.attr)
      end
    end
  }
}
