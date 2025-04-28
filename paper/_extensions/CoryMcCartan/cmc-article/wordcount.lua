-- global counters (reset as needed)
words = 0
words_til_refs = 0
words_til_app = 0

increment = 1 -- if set to 0 we'll stop counting words temporarily
-- if increment != 1, once we hit a heading at or above this level
-- we'll reset the increment to 1
h_level_reset = -1
last_h = nil
last_h_words = 0

-- words per display equation
display_math_words = 1

-- special class name to indicate "don't count this element/section"
skip_cl = "nowords"

skip = function(el)
    if el.classes ~= nil then
        return el.classes:includes(skip_cl, 1)
    end
    return false
end

-- Functions to count words in different types of Pandoc objects
-- Working off of <https://github.com/pandoc/lua-filters/blob/master/wordcount/wordcount.lua>
counting_fns = {
    traverse = "topdown",

    Str = function(el)
        if el.text:match("%P") then -- match non-punctuation only
            words = words + increment
        end
    end,

    Math = function(el)
        if el.mathtype == "DisplayMath" then
            words = words + increment * display_math_words
        else
            words = words + increment
        end
    end,


    Code = function(el)
        _, n = el.text:gsub("%S+", "")
        words = words + n * increment
    end,

    CodeBlock = function(el)
        if skip(el) then return nil end
        _, n = el.text:gsub("%S+", "")
        words = words + n * increment
    end,

    Header = function(el)
        -- reset increment if needed
        if el.level <= h_level_reset then
            increment = 1
        end

        if el.level == 1 then
            if last_h ~= nil then
                print(string.format("  (%-12.12s...) %6d", last_h, words - last_h_words))
            end
            last_h_words = words
            last_h = pandoc.utils.stringify(el.content)
        end

         -- count appendices separately
        if el.classes ~= nil and
                el.classes:includes("appendix", 1) and
                words_til_app <= 0 then
             words_til_app = words
        end

        if skip(el) then
            increment = 0
            h_level_reset = el.level
            return el, false
        end
    end,

    Div = function(el)
        if skip(el) then return el, false end
        -- count refs separately
        if el.identifier == "refs" and words_til_refs <= 0 then
            words_til_refs = words
        end
    end,

    Figure = function(el)
        if skip(el) then return el, false end
    end,
    Image = function(el)
        if skip(el) then return el, false end
    end,
    Table = function(el)
        if skip(el) then return el, false end
    end,
    RawBlock = function(el)
        if skip(el) then return el, false end
    end
}


-- The main filter
function Pandoc(el)
    -- process citations
    el = pandoc.utils.citeproc(el)

    -- count and output as we go
    cuml_words = 0
    print("\n WORD COUNT\n-----------------------------------")
    print(" Section             Words   Cuml.")
    print("-----------------------------------")

    if el.meta.title ~= nil then
        words = 0 -- reset
        el.meta.title:walk(counting_fns)
        -- don't include title in total
        -- cuml_words = cuml_words + words
        -- print(string.format(" Title       %6d %6d", words, cuml_words))
        print(string.format(" Title              %6d       ", words))
    end
    if el.meta.abstract ~= nil then
        words = 0
        el.meta.abstract:walk(counting_fns)
        cuml_words = cuml_words + words
        print(string.format(" Abstract           %6d %6d", words, cuml_words))
    end

    words = 0
    el.blocks:walk(counting_fns)
    if words_til_app <= words_til_refs then
        words_body = words_til_app
        words_app = words_til_refs - words_body
        words_refs = words - words_til_refs
        print(string.format("  (%-12.12s...) %6d", last_h, words_til_refs - last_h_words))
    else
        words_body = words_til_refs
        words_refs = words_til_app - words_body
        words_app = words - words_til_app
    end

    cuml_words = cuml_words + words_body
    print(string.format(" Body               %6d %6d", words_body, cuml_words))
    cuml_words = cuml_words + words_refs
    print(string.format(" References         %6d %6d", words_refs, cuml_words))
    cuml_words = cuml_words + words_app
    print(string.format(" Appendices         %6d %6d", words_app, cuml_words))

    print("-----------------------------------\n\n")
    return nil
end
