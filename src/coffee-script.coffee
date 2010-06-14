# CoffeeScript can be used both on the server, as a command-line compiler based
# on Node.js/V8, or to run CoffeeScripts directly in the browser. This module
# contains the main entry functions for tokenzing, parsing, and compiling source
# CoffeeScript into JavaScript.
#
# If included on a webpage, it will automatically sniff out, compile, and
# execute all scripts present in `text/coffeescript` tags.

# Set up dependencies correctly for both the server and the browser.
if process?
  path:         require 'path'
  Lexer:        require('./lexer').Lexer
  parser:       require('./parser').parser
  helpers:      require('./helpers').helpers
  helpers.extend global, require './nodes'
  if require.registerExtension
    require.registerExtension '.coffee', (content) -> compile content
else
  this.exports: this.CoffeeScript: {}
  Lexer:        this.Lexer
  parser:       this.parser
  helpers:      this.helpers

# The current CoffeeScript version number.
exports.VERSION: '0.6.2'

# Instantiate a Lexer for our use here.
lexer: new Lexer()

# Compile a string of CoffeeScript code to JavaScript, using the Coffee/Jison
# compiler.
exports.compile: compile: (code, options) ->
  options: or {}
  try
    (parser.parse lexer.tokenize code).compile options
  catch err
    err.message: "In $options.source, $err.message" if options.source
    throw err

# Tokenize a string of CoffeeScript code, and return the array of tokens.
exports.tokens: (code) ->
  lexer.tokenize code

# Tokenize and parse a string of CoffeeScript code, and return the AST. You can
# then compile it by calling `.compile()` on the root, or traverse it by using
# `.traverse()` with a callback.
exports.nodes: (code) ->
  parser.parse lexer.tokenize code

# Compile and execute a string of CoffeeScript (on the server), correctly
# setting `__filename`, `__dirname`, and relative `require()`.
exports.run: ((code, options) ->
  module.filename: __filename: options.source
  __dirname: path.dirname(__filename)
  eval exports.compile code, options
)

# Extend CoffeeScript with a custom language extension. It should hook in to
# the **Lexer** (as a peer of any of the lexer's tokenizing methods), and
# push a token on to the stack that contains a **Node** as the value (as a
# peer of the nodes in [nodes.coffee](nodes.html)).
exports.extend: (func) ->
  Lexer.extensions.push func

# The real Lexer produces a generic stream of tokens. This object provides a
# thin wrapper around it, compatible with the Jison API. We can then pass it
# directly as a "Jison lexer".
parser.lexer: {
  lex: ->
    token: @tokens[@pos] or [""]
    @pos: + 1
    this.yylineno: token[2]
    this.yytext:   token[1]
    token[0]
  setInput: (tokens) ->
    @tokens: tokens
    @pos: 0
  upcomingInput: -> ""
  showPosition: -> @pos
}

# Activate CoffeeScript in the browser by having it compile and evaluate
# all script tags with a content-type of `text/coffeescript`. This happens
# on page load. Unfortunately, the text contents of remote scripts cannot be
# accessed from the browser, so only inline script tags will work.
if document? and document.getElementsByTagName
  process_scripts: ->
    for tag in document.getElementsByTagName('script') when tag.type is 'text/coffeescript'
      eval exports.compile tag.innerHTML
  if window.addEventListener
    window.addEventListener 'load', process_scripts, false
  else if window.attachEvent
    window.attachEvent 'onload', process_scripts
