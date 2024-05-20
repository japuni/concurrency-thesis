import numpy as np
from scipy.stats import ttest_ind

# Speed Up Factors
java_speed_up = {
    "6 threads": [3.819046999, 3.974947815, 4.310796076],
    "12 threads": [3.955596773, 4.200686, 4.34788863],
    "24 threads": [3.992452415, 4.017499977, 4.286059359]
}

erlang_speed_up = {
    "6 threads": [2.553894292, 2.554143104, 2.551229782],
    "12 threads": [2.893129111, 2.84436009, 2.876733669],
    "24 threads": [3.044299771, 2.965888072, 3.029477867]
}

# Perform T-tests
results = {}
for thread in ["6 threads", "12 threads", "24 threads"]:
    t_stat, p_value = ttest_ind(java_speed_up[thread], erlang_speed_up[thread])
    results[thread] = {"t_stat": t_stat, "p_value": p_value}

# Display the results
for thread, result in results.items():
    print(f"T-test results for {thread}:")
    print(f"T-statistic: {result['t_stat']}")
    print(f"P-value: {result['p_value']}\n")