import functools
def rec_stream(next, *initial):
    initial = list(initial)
    for v in initial:
        yield v
    while True:
        v = next(*initial)
        yield v
        initial = initial[1:]+[v]

def prim(seq, v0, dt):
    return rec_stream(lambda v: v+next(seq)*dt, v0)

fib = rec_stream(lambda a,b: a + b, 0, 1)
dt = 0.01
reals = rec_stream(lambda n: n+dt, 0)
square = map(lambda x: x*x, reals)
prim_square = prim(square, 0 , dt)



for _ in range(101):
    print(next(prim_square))
