   '' // new_line('a') // &
   '/* Articles */' // new_line('a') // &
   'article {' // new_line('a') // &
   '   margin: 0 0 0 235px;' // new_line('a') // &
   '   border-left: silver solid thick;' // new_line('a') // &
   '   padding: 0 20px;' // new_line('a') // &
   '   text-align: justify;' // new_line('a') // &
   '   -moz-hyphens: auto;' // new_line('a') // &
   '   -ms-hyphens: auto;' // new_line('a') // &
   '   -webkit-hyphens: auto;' // new_line('a') // &
   '   hyphens: auto;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article > header {' // new_line('a') // &
   '   padding-bottom: 0.15ex;' // new_line('a') // &
   '   border-bottom: thin solid #ff9040;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article > header > h3 {' // new_line('a') // &
   '   margin: 0 -20px;' // new_line('a') // &
   '   color: #506088;' // new_line('a') // &
   '   text-shadow: 1px 1px #ccc;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article> header > h3 > a {' // new_line('a') // &
   '   text-decoration: none;' // new_line('a') // &
   '   background: transparent;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article> header > h3 > a:focus {' // new_line('a') // &
   '   text-shadow: none;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article > * {' // new_line('a') // &
   '   margin-top: 2ex;' // new_line('a') // &
   '   padding: inherit;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article > p {' // new_line('a') // &
   '   text-indent: 1.0em;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article  img {' // new_line('a') // &
   '   border: silver solid thin;' // new_line('a') // &
   '   border-radius: 4px;' // new_line('a') // &
   '   box-shadow: 2px 2px silver;' // new_line('a') // &
   '   max-width: 100%;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'article > footer {' // new_line('a') // &
   '   display: block;' // new_line('a') // &
   '   text-align: right;' // new_line('a') // &
   '   font-size: 8pt;' // new_line('a') // &
   '   font-weight: bolder;' // new_line('a') // &
   '   padding: 3px;' // new_line('a') // &
   '   background: #ddd;' // new_line('a') // &
   '   background: linear-gradient(270deg, #ddd, #eee);' // new_line('a') // &
   '   border-radius: 3px;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '/* Bottom arrow (home page) */' // new_line('a') // &
   'body > nav:nth-of-type(2) > a {' // new_line('a') // &
   '   clear: both;' // new_line('a') // &
   '   float: right;' // new_line('a') // &
   '   margin:30px 20px 10px 20px;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '/* Text from the config file in [footer] */' // new_line('a') // &
   'body > footer {' // new_line('a') // &
   '   clear: both;' // new_line('a') // &
   '   float: left;' // new_line('a') // &
   '   padding: 5px 10px;' // new_line('a') // &
   '   font-style: italic;' // new_line('a') // &
   '   font-size: 8pt;' // new_line('a') // &
   '   opacity: 0.7;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'body::after {' // new_line('a') // &
   '   clear: both;' // new_line('a') // &
   '   width: 100%;' // new_line('a') // &
   '   display: block;' // new_line('a') // &
   '   border-top: thin solid #ff9040;' // new_line('a') // &
   '   margin-top: 15px;' // new_line('a') // &
   '   padding: 5px 0 10px 0;' // new_line('a') // &
   '   font-size: 8pt;' // new_line('a') // &
   '   color: #777;' // new_line('a') // &
   '   content: "fBlog - SlateBlue";' // new_line('a') // &
   '   text-align: center;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '/* ---- Case of language with rigth to left direction ---*/' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] dd {' // new_line('a') // &
   '    margin: 0 4em 1em 0;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] body > aside {' // new_line('a') // &
   '   float: right;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] body > nav:first-of-type {' // new_line('a') // &
   '   margin: 0 10px 20px 0;' // new_line('a') // &
   '   float: right;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] body > nav:first-of-type > a[rel="prev"] {' // new_line('a') // &
   '   margin: 0 0 2ex 10px;' // new_line('a') // &
   '   float: left;' // new_line('a') // &
   '   clear: left;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] body > nav:first-of-type > a[rel="next"] {' // new_line('a') // &
   '   margin: 0 10px 2ex 0;' // new_line('a') // &
   '   float: right;' // new_line('a') // &
   '   clear: left;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] article {' // new_line('a') // &
   '   margin: 0 235px 3em 0;' // new_line('a') // &
   '   border-left: none;' // new_line('a') // &
   '   border-right: silver solid thick;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] article > p {' // new_line('a') // &
   '   text-indent: 0;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] body > nav:nth-of-type(2) > a {' // new_line('a') // &
   '   float: left;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'html[dir="rtl"] body > footer {' // new_line('a') // &
   '   float: right;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '/*@media screen and (max-width: 1100px){*/' // new_line('a') // &
   '@media screen and (max-width: 1280px){' // new_line('a') // &
   '' // new_line('a') // &
   '' // new_line('a') // &
   'body {' // new_line('a') // &
   '   font-size: 11pt;' // new_line('a') // &
   '   margin-top: 40px;' // new_line('a') // &
   '   box-shadow: none;' // new_line('a') // &
   '   border-radius: 0;' // new_line('a') // &
   '}' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '' // new_line('a') // &
   '@media screen and (max-width: 1024px){' // new_line('a') // &
   '' // new_line('a') // &
   'body {' // new_line('a') // &
   '   font-size: 10pt;' // new_line('a') // &
   '}' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '' // new_line('a') // &
   '@media screen and (max-width: 800px) {' // new_line('a') // &
   '' // new_line('a') // &
   'body {' // new_line('a') // &
   '   width: auto;' // new_line('a') // &
   '   margin: 0;' // new_line('a') // &
   '   padding-top: 0;' // new_line('a') // &
   '   min-width: 480px;' // new_line('a') // &
   '   font-size: 11pt;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'body > header {' // new_line('a') // &
   '   position: relative;' // new_line('a') // &
   '   margin: -1px;' // new_line('a') // &
   '   padding: 0;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   '/* Description of the blog */' // new_line('a') // &
   'body >  aside {' // new_line('a') // &
   '   width: auto;' // new_line('a') // &
   '   padding: 5px 15px;' // new_line('a') // &
   '   margin: 0;' // new_line('a') // &
   '   text-align: inherit;' // new_line('a') // &
   '   float: none;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'body > nav:first-of-type {' // new_line('a') // &
   '   width: auto;' // new_line('a') // &
   '   margin: 0;' // new_line('a') // &
   '   padding: 5px 10px 7px 10px;' // new_line('a') // &
   '   border-top: #506088 thin solid;' // new_line('a') // &
   '   border-bottom: #506088 thin solid;' // new_line('a') // &
   '   float: none;' // new_line('a') // &
   '}' // new_line('a') // &
   '' // new_line('a') // &
   'header+nav:first-of-type {' // new_line('a') // &
   '   margin-top: 5px;' // new_line('a') // &
   '}' // new_line('a') // &
   ''