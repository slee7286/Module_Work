import math
import matplotlib.pyplot as plt

# Compute the sequence a_{n+1} = sqrt(2 + a_n)
terms = [0]  # Start with a0 = 0
for _ in range(20):
    terms.append(math.sqrt(2 + terms[-1]))

# Display the terms
for i, value in enumerate(terms):
    print(f"a_{i} = {value}")

# Plot the terms to visualize convergence
plt.figure()
plt.plot(range(len(terms)), terms, marker='o')
plt.xlabel("n")
plt.ylabel("a_n")
plt.title("Convergence of a_{n+1} = sqrt(2 + a_n)")
plt.show()
