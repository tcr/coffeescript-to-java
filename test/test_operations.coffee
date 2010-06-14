# CoffeeScript's operations should be chainable, like Python's.
ok 500 > 50 > 5 > -5

ok true is not false is true is not false

ok 0 is 0 isnt 50 is 50

ok 10 < 20 > 10

ok 50 > 10 > 5 is parseInt('5', 10)


# Make sure that each argument is only evaluated once, even if used
# more than once.
i: 0
func: -> i++

ok 1 > func() < 1


# `:` and `=` should be interchangeable, as should be `==` and `is`.
a: 1
b: 1

ok a is 1 and b is 1
ok a == b
ok a is b


# Ensure that chained operations don't cause functions to be evaluated more
# than once.
val: 0
func: -> val: + 1

ok 2 > (func null) < 2
ok val is 1
