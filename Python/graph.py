import matplotlib.pyplot as plt

# Data for Java and Erlang Scaling Efficiency (SE)
java_se = {
    "500x500": [0.85637696, 0.681381046, 0.217871005],
    "1000x1000": [0.918864332, 0.74103875, 0.394660138],
    "1500x1500": [1.416919736, 0.783843094, 0.438986906],
    "2000x2000": [1.090034146, 0.884034485, 0.393308912]
}

erlang_se = {
    "500x500": [0.47300166, 0.239699841, 0.107514203],
    "1000x1000": [0.729897896, 0.390733569, 0.166602566],
    "1500x1500": [0.745877307, 0.403961818, 0.181673632],
    "2000x2000": [0.864976839, 0.465348782, 0.213261548]
}

threads = [6, 12, 24]

# Plotting the data
plt.figure(figsize=(12, 8))

for size in java_se:
    plt.plot(threads, java_se[size], marker='o', label=f'Java {size}', linestyle='-')

for size in erlang_se:
    plt.plot(threads, erlang_se[size], marker='x', label=f'Erlang {size}', linestyle='--')

plt.xlabel('Number of Threads')
plt.ylabel('Scaling Efficiency (SE)')
plt.title('Scaling Efficiency (SE) of Java and Erlang for Different Matrix Sizes')
plt.legend()
plt.grid(True)
plt.show()