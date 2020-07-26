
p1        = x * 3 + y
  where x = 3
        y = 1000

p2 = x * 5
  where x = 10 * 5 + y
        y = 10


p3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

z = 7
x = y ^ 2

waxOn = x * 5

y = z + 8

triple x = x * 3

waxOnWhere = a * 5
  where a = y ^ 2
        y = z + 8
        z = 7

waxOff x = triple x
