[ ![Codeship Status for ggb/numeral-elm](https://codeship.com/projects/f4ffde60-8427-0134-d486-12c4a3847234/status?branch=master)](https://codeship.com/projects/183086)

# numeral-elm

Elm module for (advanced) number formatting. It is a direct port of [Numeral.js](http://numeraljs.com/) and it is possible to use the same format strings. Manipulation and unformatting of numbers is not yet supported.

If you create a new language-file, please let me know or send a pull request.

# Formatting

Format a number with a given language.

```elm
import Languages.Japanese as Japanese

myFormat = formatWithLanguage Japanese.lang "0.0a"

-- map myFormat [10123.12, 235798239.3242] == ["10.1千","235.8百万"]
```

The format-function works the same way as formatWithLanguage, but English is set as default language.

```elm
format "$0,0.00" 1000.234 == "$1,000.23"
```

## Numbers

| Number  | Format  | Result |
|---------|---------|--------|
| 10000 | '0,0.0000' | 10,000.0000 |
| 10000.23 | '0,0' | 10,000 |
| 10000.23 | '+0,0' | +10,000 |
| -10000 | '0,0.0' | -10,000.0 |
| 10000.1234 | '0.000' | 10000.123 |
| 10000.1234 | '0[.]00000' | 10000.12340 |
| -10000 | '(0,0.0000)' | (10,000.0000) |
| -0.23 | '.00' | -.23 |
| -0.23 | '(.00)' | (.23) |
| 0.23 | '0.00000' | 0.23000 |
| 0.23 | '0.0[0000]' | 0.23 |
| 1230974 | '0.0a' | 1.2m |
| 1460 | '0 a' | 1 k |
| -104000 | '0a' | -104k |
| 1 | '0o' | 1st |
| 52 | '0o' | 52nd |
| 23 | '0o' | 23rd |
| 100 | '0o' | 100th |

## Currency

| Number  | Format  | Result |
|---------|---------|--------|
| 1000.234 | '$0,0.00' | $1,000.23 |
| 1000.2 | '0,0[.]00 $' | 1,000.20 $ |
| 1001 | '$ 0,0[.]00' | $ 1,001 |
| -1000.234 | '($0,0)' | ($1,000) |
| -1000.234 | '$0.00' | -$1000.23 |
| 1230974 | '($ 0.00 a)' | $ 1.23 m |

## Bytes

| Number  | Format  | Result |
|---------|---------|--------|
| 100 | '0b' | 100B |
| 2048 | '0 b' | 2 KB |
| 7884486213 | '0.0b' | 7.3GB |
| 3467479682787 | '0.000 b' | 3.154 TB |

## Percentages

| Number  | Format  | Result |
|---------|---------|--------|
| 1 | '0%' | 100% |
| 0.974878234 | '0.000%' | 97.488% |
| -0.43 | '0 %' | -43 % | 
| 0.43 | '(0.000 %)' | 43.000 % |

## Time

| Number  | Format  | Result |
|---------|---------|--------|
| 25 | '00:00:00' | 0:00:25 |
| 238 | '00:00:00' | 0:03:58 |
| 63846 | '00:00:00' | 17:44:06 |

## Custom Unit Suffixes

| Number  | Format  | Result |
|---------|---------|--------|
| 12345 | 0,0[ pcs.] | 12,345 pcs. |
| 12345 | 0,0[pcs.] | 12,345pcs. |
| 300000 | 0,0 [ ponies] | 300,000 ponies |