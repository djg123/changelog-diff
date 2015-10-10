{-# LANGUAGE QuasiQuotes #-}
module Css (writeCss) where
import Text.Lucius
import qualified Data.Text.Lazy.IO as TLIO

-- Dummy render function
render = undefined

css :: t -> Css
css = 
  [lucius|
/* http://meyerweb.com/eric/tools/css/reset/ 
   v2.0 | 20110126
   License: none (public domain)
*/

html, body, div, span, applet, object, iframe,
h1, h2, h3, h4, h5, h6, p, blockquote, pre,
a, abbr, acronym, address, big, cite, code,
del, dfn, em, img, ins, kbd, q, s, samp,
small, strike, strong, sub, sup, tt, var,
b, u, i, center,
dl, dt, dd, ol, ul, li,
fieldset, form, label, legend,
table, caption, tbody, tfoot, thead, tr, th, td,
article, aside, canvas, details, embed, 
figure, figcaption, footer, header, hgroup, 
menu, nav, output, ruby, section, summary,
time, mark, audio, video {
	margin: 0;
	padding: 0;
	border: 0;
	font-size: 100%;
	font: inherit;
	vertical-align: baseline;
}
/* HTML5 display-role reset for older browsers */
article, aside, details, figcaption, figure, 
footer, header, hgroup, menu, nav, section {
	display: block;
}
body {
	line-height: 1;
}
ol, ul {
	list-style: none;
}
blockquote, q {
	quotes: none;
}
blockquote:before, blockquote:after,
q:before, q:after {
	content: '';
	content: none;
}
table {
	border-collapse: collapse;
	border-spacing: 0;
}
/* Changelog css */
body {
  font: normal medium/1.4 sans-serif;
}
table {
  border-collapse: collapse;
  width: 100%;
  border: 0 solid #ccc;
}

td, th {
  padding: 0.25rem;
}

th {
  font-weight: bold;
  font-size: 24px;
}

.functionName {
  padding: 0.25rem;
  text-align: right;
}

.type {
  text-align: left;
}

/* Odd and even entry needed because Changed Type entries span two lines */
.odd-entry {
  background: #eee;
}

.even-entry {

}
  |]

writeCss :: FilePath -> IO ()
writeCss path = TLIO.writeFile path (renderCss (css render))
