import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats
# import seaborn as sns
import sys
import numpy as np

# print(sys.argv[1:])
gridstep, vol = sys.argv[1], sys.argv[2]

# Read the CSV data file
df = pd.read_csv(f'test_sim_reset_2_offers_rep_{gridstep}_{vol}.csv',
                 sep=',')
                #  skipinitialspace=True)  # Skip spaces after separator

df.columns = df.columns.str.strip()

# compute regression line
x=df['final price']
y=df['return']
coefficients = np.polyfit(x, y, deg=1)
slope, intercept = coefficients
print(f'regression line: slope {round(slope,5)}, intercept {round(intercept,5)}')

# Predicted values
y_pred = slope * x + intercept
# plt.plot(x, y_pred, color='black', label='Fitted Line')

# adjust returns (simulates a short)
df['areturn'] = df['return'] - y_pred

# print(df.columns)
# print(df)

# histogram of returns
plt.figure(figsize=(10, 6))
plt.hist(df['areturn'], bins=50, edgecolor='black')

# data = df['areturn']
# kde_x = np.linspace(min(data), max(data), 200)
# kde = stats.gaussian_kde(data)
# plt.plot(kde_x, kde(kde_x), 'r-', linewidth=2, label='KDE')

# Set title and labels
plt.title('Distribution of returns')
plt.xlabel('Returns')
plt.ylabel('Frequency')

# Add grid
plt.grid(True, linestyle='--', alpha=0.7)

# Save the plot
plt.savefig(f'return_distributions_{gridstep}_{vol}.png')
# plt.show()

plt.figure(figsize=(12, 8))
# Plot points below y=0 in transparent red
plt.scatter(df[df['areturn'] <= 0]['final price'], 
            df[df['areturn'] <= 0]['areturn'], 
            color='red', alpha=0.4, label='Negative Return')

# Plot points above y=0 in transparent green
plt.scatter(df[df['areturn'] > 0]['final price'], 
            df[df['areturn'] > 0]['areturn'], 
            color='green', alpha=0.4, label='Positive Return')

# Set title and labels
plt.title(f'Joint Distribution of b&h adjusted Returns vs Final Price (ratio = {gridstep}, vol = {vol})', fontsize=16)
plt.xlabel('Final Price', fontsize=12)
plt.ylabel('Return', fontsize=12)

# Save the plot
plt.savefig(f'return_vs_final_price_scatter_{gridstep}_{vol}.png', dpi=300)

# Print summary statistics
print(df[['areturn','final price','up crossings','down crossings']].describe())
# print(df['down crossings']/(df['up crossings']+df['down crossings']).describe())
# Print correlation coefficient
print("\nCorrelation coefficient between adjusted return and final price:")
print(df['areturn'].corr(df['final price']))