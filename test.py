import tracemalloc

tracemalloc.start()

def fib(n):
    if n <= 1:
        return 1
    return fib(n - 1) + fib(n - 2)

print(fib(25))

snapshot = tracemalloc.take_snapshot()
top_stats = snapshot.statistics('lineno')

print("[ Top 10 ]")
for stat in top_stats[:10]:
    print(stat)
