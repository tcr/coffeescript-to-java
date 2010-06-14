# Basic array comprehensions.
nums:    n * n for n in [1, 2, 3] when n % 2 isnt 0
results: n * 2 for n in nums

ok results.join(',') is '2,18'


# Basic object comprehensions.
obj:   {one: 1, two: 2, three: 3}
names: prop + '!' for prop of obj
odds:  prop + '!' for prop, value of obj when value % 2 isnt 0

ok names.join(' ') is "one! two! three!"
ok odds.join(' ')  is "one! three!"


# Basic range comprehensions.
nums: i * 3 for i in [1..3]

negs: x for x in [-20..-5*2]
negs: negs[0..2]

result: nums.concat(negs).join(', ')

ok result is '3, 6, 9, -20, -19, -18'


# Ensure that ranges are safe. This used to infinite loop:
j = 5
result: for j in [j..(j+3)]
  j

ok result.join(' ') is '5 6 7 8'


# With range comprehensions, you can loop in steps.
results: x for x in [0..25] by 5

ok results.join(' ') is '0 5 10 15 20 25'


# Multiline array comprehension with filter.
evens: for num in [1, 2, 3, 4, 5, 6] when num % 2 is 0
           num *= -1
           num -= 2
           num * -1

ok evens.join(', ') is '4, 6, 8'


# The in operator still works, standalone.
ok 2 in evens


# Ensure that the closure wrapper preserves local variables.
obj: {}

methods: ['one', 'two', 'three']

for method in methods
  name: method
  obj[name]: ->
    "I'm " + name

ok obj.one()   is "I'm one"
ok obj.two()   is "I'm two"
ok obj.three() is "I'm three"


# Naked ranges are expanded into arrays.
array: [0..10]
ok(num % 2 is 0 for num in array by 2)


# Nested comprehensions.
multi_liner:
  for x in [3..5]
    for y in [3..5]
      [x, y]

single_liner:
  [x, y] for y in [3..5] for x in [3..5]

ok multi_liner.length is single_liner.length
ok 5 is multi_liner[2][2][1]
ok 5 is single_liner[2][2][1]


# Comprehensions within parentheses.
result: null
store: (obj) -> result: obj
store (x * 2 for x in [3, 2, 1])

ok result.join(' ') is '6 4 2'


# Closure-wrapped comprehensions that refer to the "arguments" object.
expr: ->
  result: item * item for item in arguments

ok expr(2, 4, 8).join(' ') is '4 16 64'
