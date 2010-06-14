# Test classes with a four-level inheritance chain.
class Base
  func: (string) ->
    "zero/$string"

  @static: (string) ->
    "static/$string"

class FirstChild extends Base
  func: (string) ->
    super('one/') + string

class SecondChild extends FirstChild
  func: (string) ->
    super('two/') + string

class ThirdChild extends SecondChild
  constructor: ->
    @array: [1, 2, 3]

  # Gratuitous comment for testing.
  func: (string) ->
    super('three/') + string

result: (new ThirdChild()).func 'four'

ok result is 'zero/one/two/three/four'
ok Base.static('word') is 'static/word'


class TopClass
  constructor: (arg) ->
    @prop: 'top-' + arg

class SuperClass extends TopClass
  constructor: (arg) ->
    super 'super-' + arg

class SubClass extends SuperClass
  constructor: ->
    super 'sub'

ok (new SubClass()).prop is 'top-super-sub'


class OneClass
  constructor: (name) -> @name: name

class TwoClass extends OneClass

ok (new TwoClass('three')).name is 'three'


# And now the same tests, but written in the manual style:
Base: ->
Base::func: (string) ->
  'zero/' + string

FirstChild: ->
FirstChild extends Base
FirstChild::func: (string) ->
  super('one/') + string

SecondChild: ->
SecondChild extends FirstChild
SecondChild::func: (string) ->
  super('two/') + string

ThirdChild: ->
  @array: [1, 2, 3]
  this
ThirdChild extends SecondChild
ThirdChild::func: (string) ->
  super('three/') + string

result: (new ThirdChild()).func 'four'

ok result is 'zero/one/two/three/four'


TopClass: (arg) ->
  @prop: 'top-' + arg
  this

SuperClass: (arg) ->
  super 'super-' + arg
  this

SubClass: ->
  super 'sub'
  this

SuperClass extends TopClass
SubClass extends SuperClass

ok (new SubClass()).prop is 'top-super-sub'