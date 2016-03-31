# numeral-elm

Elm module for (advanced) number formatting. It is a direct port of [Numeral.js](http://numeraljs.com/) and it is possible to use the same format strings. Manipulation and unformatting of numbers is not yet supported.

If you create a new language-file, please let me know or send a pull request.

# Formatting

Format a number with a given language.

    import Language.Japanese as Japanese

    myFormat = formatWithLanguage Japanese.lang "0.0a"

    -- map myFormat [10123.12, 235798239.3242] == ["10.1千","235.8百万"]

The format-function works the same way as formatWithLanguage, but English is set as default language.

    format "$0,0.00" 1000.234 == "$1,000.23"