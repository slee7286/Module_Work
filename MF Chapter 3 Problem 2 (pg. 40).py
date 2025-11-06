print('Enter your natural number:')
number = int(input())
if number < 1:
    raise Exception("Sorry, only natural numbers")

print('Enter your number of decimal places (>=1):')
decimal = int(input())
if decimal < 1:
    raise Exception("Sorry, only decimal places greater than 0")

sqrt = 0

for i in range(number // 2 + 1):
    if i ** 2 > number:
        sqrt = i - 1
        break
    elif i ** 2 == number:
        sqrt = i
        break
else:
    sqrt = number // 2

for i in range(1, decimal + 1):
    for j in range(10):
        if (sqrt + j * 10**(-i))**2 > number:
            sqrt = sqrt + (j - 1) * 10**(-i)
            break
    else:
        sqrt = sqrt + 9 * 10**(-i)

print(f"\nApproximate sqrt({number}) is {round(sqrt, decimal)}")