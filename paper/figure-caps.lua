-- Collects all figure captions and inserts copies at the #figure-caps div.

local figure_captions = {}
local fig_count = 0
local in_appendix = false

return {
  -- First pass: collect captions, stopping when the appendix begins
  {
    Header = function(el)
      if el.classes:includes("appendix") then
        in_appendix = true
        -- Disable ghost mode so appendix figures render normally
        if quarto.doc.isFormat("latex") then
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
  -- Second pass: replace #figure-caps div with the collected captions
  {
    Div = function(div)
      if div.identifier == "figure-caps" then
        return pandoc.Div(figure_captions, div.attr)
      end
    end
  }
}
