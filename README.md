publications-scraper
====================

This a very early version, but works. Web scraping by regexes is hairy
business, so the code may break in funny ways. Let me know, or leave
an issue, or a pull request.

Web scraper for collecting combined list of publications for a group
of people from University of Helsinki [TUHAT research database][1] web
pages. Which is an [Elsevier Pure][2] product, so this script might
work with minor modifications for similar installationa at other
institutions.

Write a list of names, one per line, into a file, say `people.txt`.
For example:

    Ditte Mogensen
    Sampo Smolander

and then run

    ./scraper.scala 2011 people.txt > list.txt 

to get a joint list of publications. Ordered by year, then by first
author, then by second author, then by title.

At the moment only publication types

    A1 Refereed journal article
    A3 Contribution to book/other compilations (refereed)
    B2 Contribution to book/other compilations (non-refereed)

are collected. If you need more, contact me (or leave an issue here at
Github, or a pull request) and I will try to modify the code.

To do
-----

*   Add HTML output (add `toHTML` to class `Publication`)

*   Handle more types of publications (in `matchToPublication` and in
    class `Publication`)

*   (Tidy up code)

- - -
sampo.smolander@helsinki.fi

[1]: https://tuhat.halvi.helsinki.fi/portal/
[2]: http://www.elsevier.com/online-tools/research-intelligence/products-and-services/pure
