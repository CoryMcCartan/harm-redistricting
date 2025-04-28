-- Move references and handle appendices

seen_app = false
seen_div = false

function proc_app(el)
    if el.attr.classes:includes("appendix") and not seen_app then
        seen_app = true
        if quarto.doc.isFormat("latex") then
            if not seen_div then
                return pandoc.List({
                    pandoc.RawInline("latex", "\\hypertarget{refs}{}\n"),
                    pandoc.RawInline("latex", "\\begin{CSLReferences}{0}{0}\\end{CSLReferences}\n\n"),
                    pandoc.RawBlock("latex", "\\appendix\n"),
                    pandoc.RawInline("latex", "\\renewcommand\\thefigure{\\thesection\\arabic{figure}}\n"),
                    pandoc.RawInline("latex", "\\renewcommand\\thetable{\\thesection\\arabic{table}}\n"),
                    pandoc.RawInline("latex", "\\setcounter{figure}{0}\n\n"),
                    el
                })
            else
                return pandoc.List({
                    pandoc.RawBlock("latex", "\\appendix\n"),
                    pandoc.RawInline("latex", "\\renewcommand\\thefigure{\\thesection\\arabic{figure}}\n"),
                    pandoc.RawInline("latex", "\\renewcommand\\thetable{\\thesection\\arabic{table}}\n"),
                    pandoc.RawInline("latex", "\\setcounter{figure}{0}\n\n"),
                    el
                })
            end
        end
    end
end

function proc_div(el)
    if quarto.doc.isFormat("latex") and el.identifier == "refs" then
        seen_div = true
        return pandoc.List({
            pandoc.RawInline("latex", "\\hypertarget{refs}{}\n"),
            pandoc.RawInline("latex", "\\begin{CSLReferences}{0}{0}\\end{CSLReferences}\n\n"),
        })
    end
end

return {
    { Div = proc_div },
    { Header = proc_app }
}
