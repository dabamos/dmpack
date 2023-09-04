<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:xhtml="http://www.w3.org/1999/xhtml">
<xsl:output indent="yes" method="html" encoding="utf-8" doctype-system="about:legacy-compat" />
<xsl:template match="/">
<html>
<head>
<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1" />
<title><xsl:value-of select="atom:feed/atom:title" /> – <xsl:value-of select="atom:feed/atom:subtitle" /></title>
<style type="text/css">
:root, html[data-theme='light'] {--rem: 12pt;--width: 60rem;--navpos: absolute;--font-p: 1em/1.7 'Open Sans', Helvetica, sans-serif;--font-h: .9em/1.5 'Open Sans', Helvetica, sans-serif;--font-c: .9em/1.4 monospace;--border: 1px solid var(--cmed);--ornament: "‹‹‹ ›››";--cfg: #433;--cbg: #fff;--cdark: #888;--clight: #f5f6f7;--cmed: #d1d1d1;--clink: #069;--cemph: SlateGray;--cemphbg: SlateGray;}* {box-sizing: border-box;border-spacing: 0;margin: 0;padding: 0;}header, footer, figure, table, video, details, blockquote, ul, ol, dl, fieldset, pre, pre > code, caption {display: block;margin: 0.5rem 0rem 1rem;width: 100%;overflow: auto hidden;text-align: left;}video, summary, input, select {outline:none;}a, button, select, summary {color: var(--clink);cursor: pointer;}html {font-size: var(--rem);background: var(--cbg);}body {position: relative;margin: auto;max-width: var(--width);font: var(--font-p);color: var(--cfg);padding: 0.6rem 0.6rem 0;overflow-x: hidden;}body > footer {margin: 10rem 0rem 0rem;font-size: 90%;}footer p {text-align: center;}p {margin: .6em 0;}a[href]{text-decoration: none;}a[href^="#"] {text-decoration: none;}a:hover, button:not([disabled]):hover, summary:hover {filter: brightness(92%);color: var(--cemph);border-color: var(--cemph);}ul, ol, dl {margin: 1rem 0;padding: 0 0 0 2em;}li:not(:last-child), dd:not(:last-child) {margin-bottom: 0.5rem;}dt {font-weight: bold;}h1, h2, h3, h4, h5 {margin: 2.5em 0 .5rem;font: var(--font-h);line-height: 1.2em;clear: both;}h1+h2, h2+h3, h3+h4, h4+h5 {margin-top: .5em;padding-top: 0;}h1 {font-size: 2.2em;font-weight: 500;}h2 {font-size: 1.8em;font-weight: 400;}h3 {font-size: 1.5em;font-weight: 500;}h4 {font-size: 1.1em;font-weight: 700;}h5 {font-size: 1.2em;font-weight: 400;color: var(--cfg);}h6 {font-size: 1.0em;font-weight: 700;font-style: italic;display: inline;}h6 + p {display: inline;}h1 small, h2 small, h3 small {color: var(--cdark);margin-left: 1rem;}td, th {padding: 0.5em 0.8em;border-bottom: 0.1rem solid var(--cmed);font-size: 95%;vertical-align: text-top;}nav td, nav th {border-bottom: none;}th {white-space: nowrap;}thead th[colspan] {padding: .2em 0.8em;text-align: center;}thead tr:not(:only-child) td {padding: .2em 0.8em;}thead+tbody tr:first-child td {border-top: 0.1rem solid var(--cdark);}tr:hover{background-color: var(--clight);}table img {display: block;}img, svg {max-width: 100%;vertical-align: text-top;object-fit: cover;}p>img:not(:only-child) {float: right;margin: 0 0 .5em .5em;}figure > img {display: inline-block;width: auto;}figure > img:only-of-type, figure > svg:only-of-type {max-width: 100%;display: block;margin: 0 auto 0.4em;}figcaption, caption {font: var(--font-h);color: var(--cdark);width: 100%;}figcaption > *:first-child, caption > *:first-child {display: inline-block;margin: 0;}figure > *:not(:last-child) {margin-bottom: 0.4rem;}pre > code {margin: 0;position: relative;padding: 0.8em;border-left: .4rem solid var(--cemph);}code, kbd, samp {padding: 0.2em;font: var(--font-c);background: var(--clight);}kbd {border: 1px solid var(--cmed);}blockquote {border-left: 0.4rem solid var(--cmed);padding: 0 1rem 0 1rem;}time{color: var(--cdark);}hr {border: 0;border-top: 0.1rem solid var(--cmed);}nav {width: 100%;background-color: var(--clight);}::selection, mark {background: var(--clink);color: var(--cbg);}article {counter-reset: h2 0 h3 0 tab 0 fig 0 lst 0 ref 0 eq 0;}article figure figcaption:before {color: var(--cemph);counter-increment: fig;content: "Figure " counter(fig) ": ";}figure {counter-reset: subfig 0 }article figure figure {counter-reset: none;}article figure > figure {display: inline-grid;width: auto;}figure > figure:not(:last-of-type) {padding-right: 1rem;}article figure figure figcaption:before {counter-increment: subfig 1;content: counter(subfig, lower-alpha) ": ";}article figure pre + figcaption:before {counter-increment: lst 1;content: "Listing " counter(lst) ": ";}figure > table:only-of-type {display: table;margin: 0.5em auto !important;width: fit-content;}article figure > table caption {display: table-caption;caption-side: bottom;}article figure > table + figcaption:before, article table caption:before {color: var(--cemph);counter-increment: tab 1;content: "Table " counter(tab) ": ";}article h2, h3 {position: relative;}article h2:before, article h3:before {display: inline-block;position: relative;font-size: 0.6em;text-align: right;vertical-align: baseline;left: -1rem;width: 2.5em;margin-left: -2.5em;}article h1 {counter-set: h2;}article h2:before {counter-increment: h2;content: counter(h2) ". ";counter-set: h3;}article h3:before {counter-increment: h3;content: counter(h2) "." counter(h3) ". ";}@media (max-width: 60rem) {h2:before, h3:before {display: none;}}article p>cite:before {padding: 0 .5em 0 0;counter-increment: ref;content: " [" counter(ref) "] ";vertical-align: super;font-size: .6em;}article p>cite > *:only-child {display: none;}article p>cite:hover > *:only-child, [data-tooltip]:hover:before {display: inline-block;z-index: 40;white-space: pre-wrap;position: absolute;left: 1rem;right: 1rem;padding: 1em 2em;text-align: center;transform:translateY( calc(-100%) );content: attr(data-tooltip);color: var(--cbg);background-color: var(--cemph);box-shadow: 0 2px 10px 0 black;}[data-tooltip], article p>cite:before {color: var(--clink);border: .8rem solid transparent;margin: -.8rem;}abbr[title], [data-tooltip] {cursor: help;}nav+* {margin-top: 3rem;}body>nav, header nav {position: var(--navpos);top: 0;left: 0;right: 0;z-index: 41;box-shadow: 0vw -50vw 0 50vw var(--clight), 0 calc(-50vw + 2px) 4px 50vw var(--cdark);}nav ul {list-style-type: none;}nav ul:first-child {margin: 0;padding: 0;overflow: visible;}nav ul:first-child > li {display: inline-block;margin: 0;padding: 0.8rem .6rem;}nav ul > li:first-child {font-weight: bold;}nav ul > li > ul {display: none;width: auto;position: absolute;margin: 0.5rem 0;padding: 1rem 2rem;background-color: var(--clight);border: var(--border);z-index: 42;}nav ul > li > ul > li {white-space: nowrap;}nav ul > li:hover > ul {display: block;}@media (max-width: 40rem) {nav ul:first-child > li:first-child:after {content: " \25BE";}nav ul:first-child > li:not(:first-child):not(.sticky) {display: none;}nav ul:first-child:hover > li:not(:first-child):not(.sticky) {display: block;float: none !important;}}summary>* {display: inline;}.card, details {display: block;margin: 0.5rem 0rem 1rem;padding: 0 .6rem;overflow: hidden;}.card, details[open] {outline: 1px solid var(--cmed);}.card>img:first-child {margin: -3px -.6rem;max-width: calc(100% + 1.2rem);}summary:hover, details[open] summary, .card>p:first-child {box-shadow: inset 0 0 0 2em var(--clight), 0 -.8rem 0 .8rem var(--clight);}.hint {--cmed: var(--cemph);--clight: var(--cemphbg);background-color: var(--clight);}.warn {--cmed: #c11;--clight: #e221;background-color: var(--clight);}article > section:first-of-type > h2:first-of-type + p:first-letter, article > h2:first-of-type + p:first-letter, .lettrine {float: left;font-size: 3.5em;padding: 0.1em 0.1em 0 0;line-height: 0.68em;color: var(--cemph);}section:after {display: block;margin: 1em 0;color: var(--cmed);text-align: center;font-size: 1.5em;content: var(--ornament);}main aside {position: absolute;width: 8rem;right: -8.6rem;font-size: 0.8em;line-height: 1.4em;}@media (max-width: 70rem) {main aside {display: none;}}textarea, input:not([type=range]), button, select {font: var(--font-h);border: 1.5px solid var(--cmed);padding: 0.4em 0.8em;}fieldset select, input:not([type=checkbox]):not([type=radio]) {display: block;width: 100%;margin: 0 0 1rem;}button, select {font-weight: bold;background-color: var(--clight);margin: .5em;border: 1.5px solid var(--clink);}button {padding: 0.4em 1em;font-size: 85%;letter-spacing: 0.1em;}button[disabled]{color: var(--cdark);border-color: var(--cmed);}fieldset {border: var(--border);padding: .5em 1em;}textarea:hover, input:not([type=checkbox]):not([type*='ra']):hover, select:hover{border: 1.5px solid var(--cemph);}textarea:focus, input:not([type=checkbox]):not([type*='ra']):focus{border: 1.5px solid var(--clink);box-shadow: 0 0 5px var(--clink);}p>button {padding: 0 .5em;margin: 0 .5em;}p>select {padding: 0;margin: 0 .5em;}td em {font-style: normal;}td strong {color: #c11;font-weight: bold;text-decoration: none;}.row {display: flex;margin: 0.5rem -0.6rem;align-items: stretch;}.row [class*="col"] {padding: 0 0.6rem;}.row .col {flex: 1 1 100%;}.row .col-2 {flex: 0 0 16.66%;max-width: 16.66%;}.row .col-3 {flex: 0 0 25%;max-width: 25%;}.row .col-4 {flex: 0 0 33.33%;max-width: 33.33%;}.row .col-5 {flex: 0 0 41.66%;max-width: 41.66%;}.row .col-6 {flex: 0 0 50%;max-width: 50%;}@media (max-width: 40rem) {.row {flex-direction: column;}}@media print {@page {margin: 1.5cm 2cm;}html {font-size: 9pt!important;}body {max-width: 27cm;}p {orphans: 2;widows: 2;}caption, figcaption {page-break-before: avoid;}h2, h3, h4, h5 {page-break-after: avoid;}.noprint, body>nav, section:after {display: none;}.row {flex-direction: row;}td, th {padding: 0.25em 0.4em;}thead th[colspan] {padding: 0.1em 0.4em;text-align: center;}thead tr:not(:only-child) td {padding: 0.1em 0.4em;}}
</style>
</head>
<body>
<h1>
<xsl:value-of select="atom:feed/atom:title" />
<small><xsl:value-of select="atom:feed/atom:subtitle" /></small>
</h1>
<hr />
<div class="card">
<p>Atom Feed</p>
<p><strong>This is a web feed</strong>, also known as an Atom feed, of DMPACK log messages. <strong>Subscribe</strong> to the feed by copying the URL from the address bar into your newsreader app.</p>
</div>
<h2>Recent Logs</h2>
<hr />
<p>Updated: <xsl:value-of select="atom:feed/atom:updated" /></p>
<xsl:apply-templates select="atom:feed/atom:entry" />
</body>
</html>
</xsl:template>
<xsl:template match="atom:entry">
<h4><xsl:value-of select="atom:published" /></h4>
<div class="card">
<p><strong><xsl:value-of select="atom:summary" /></strong></p>
<xsl:copy-of select="atom:content/xhtml:div/xhtml:table" />
</div>
</xsl:template>
</xsl:stylesheet>
