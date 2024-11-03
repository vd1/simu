import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats
# import seaborn as sns
import sys
import numpy as np

# print(sys.argv[1:])
gridstep, vol = sys.argv[1], sys.argv[2]

# Read the CSV data file
df = pd.read_csv(f'alms/csv/test_sim_reset_2_offers_rep_{gridstep}_{vol}.csv',
                 sep=',')

df.columns = df.columns.str.strip()

df['priceChange'] = df['final price']/1000 - 1

# compute priceChange/return regression line
x=df['priceChange']
y=df['return']
coefficients = np.polyfit(x, y, deg=1)
slope, intercept = coefficients
y_pred = slope * x + intercept

# adjust returns (simulates a short)
df['areturn'] = df['return'] - y_pred
# print(df.columns)
# print(df)

############################
# histogram of returns
plt.figure(figsize=(10, 6))
data = df['areturn']

counts, bins, _ = plt.hist(data, bins=50, edgecolor='black', 
                              label='Histogram', color='skyblue')

kde_x = np.linspace(min(data), max(data), 200)
kde = stats.gaussian_kde(data)
# Scale factor = bin width * total number of samples
scale_factor = (bins[1] - bins[0]) * len(data)
plt.plot(kde_x, kde(kde_x) * scale_factor, 'r-', 
        linewidth=2, label='KDE')

mean = np.mean(data)
std = np.std(data)
stats_text = f'Mean: {mean:.3f}\nStd Dev: {std:.3f}'
# Position the text at the upper right corner
text_x = plt.xlim()[1] * 0.95  # 95% of the way to the right
text_y = plt.ylim()[1] * 0.95  # 95% of the way to the top
plt.text(text_x, text_y, stats_text,
        bbox=dict(facecolor='white', alpha=0.8, edgecolor='none'),
        horizontalalignment='right',
        verticalalignment='top')

plt.title(f'Distribution of returns  (ratio = {gridstep}, vol = {vol})')
plt.xlabel('Returns')
plt.ylabel('Frequency')
plt.grid(True, linestyle='--', alpha=0.7)
plt.savefig(f'alms/png/return_distributions_{gridstep}_{vol}.png')
# plt.show()

############################
# scatter plot of relative price change vs adjusted returns
plt.figure(figsize=(12, 8))
# Plot points below y=0 in transparent red
plt.scatter(df[df['areturn'] <= 0]['priceChange'], 
            df[df['areturn'] <= 0]['areturn'], 
            color='red', alpha=0.4, label='Negative Return')

# Plot points above y=0 in transparent green
plt.scatter(df[df['areturn'] > 0]['priceChange'], 
            df[df['areturn'] > 0]['areturn'], 
            color='green', alpha=0.4, label='Positive Return')

# Set title and labels
plt.title(f'Joint Distribution of adjusted Returns vs relative price change (ratio = {gridstep}, vol = {vol})', fontsize=16)
plt.xlabel('Relative Price change', fontsize=12)
plt.ylabel('Return', fontsize=12)
plt.savefig(f'alms/png/areturn_vs_priceChange_scatter_{gridstep}_{vol}.png', dpi=300)

############################
# scatter plot of relative price change vs returns
plt.figure(figsize=(12, 8))
# Plot points below y=0 in transparent red
plt.scatter(df[df['return'] <= 0]['priceChange'], 
            df[df['return'] <= 0]['return'], 
            color='red', alpha=0.4, label='Negative Return')

# Plot points above y=0 in transparent green
plt.scatter(df[df['return'] > 0]['priceChange'], 
            df[df['return'] > 0]['return'], 
            color='green', alpha=0.4, label='Positive Return')

# add regression line
plt.plot(x, y_pred, color='black', label='regression Line')
plt.plot(x, y_pred, color='black', label='buy and hold')

# Set title and labels
plt.title(f'Joint Distribution of Returns vs relative price change (ratio = {gridstep}, vol = {vol})', fontsize=16)
plt.xlabel('Relative Price change', fontsize=12)
plt.ylabel('Return', fontsize=12)

plt.savefig(f'alms/png/return_vs_priceChange_scatter_{gridstep}_{vol}.png', dpi=300)

# Print summary statistics
print(f'''regression line: 
  slope     {round(slope,5)}, 
  intercept {round(intercept,5)}
correlation adjusted return vs final price:
  {df['areturn'].corr(df['final price'])}''')
print(df[['areturn','final price','up crossings','down crossings']].describe())
# print(df['down crossings']/(df['up crossings']+df['down crossings']).describe())
# Print correlation coefficient
