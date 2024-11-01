import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sys
import numpy as np

# print(sys.argv[1:])
gridstep, vol = sys.argv[1], sys.argv[2]

# Read the CSV file
df = pd.read_csv(f'test_sim_reset_2_offers_rep_{gridstep}_{vol}.csv',
                 sep=',')
                #  skipinitialspace=True)  # Skip spaces after separator

print(df)
print(df.columns)
df.columns = df.columns.str.strip()

# plt.figure(figsize=(10, 6))
# plt.hist(df['return'], bins=50, edgecolor='black')

# # Set title and labels
# plt.title('Distribution of returns')
# plt.xlabel('Returns')
# plt.ylabel('Frequency')

# # Add grid
# plt.grid(True, linestyle='--', alpha=0.7)

# # Save the plot
# plt.savefig('distributions_histogram_1.04_5.png')

# # plt.show()

plt.figure(figsize=(12, 8))
# Plot points above y=0 in transparent green
plt.scatter(df[df['return'] > 0]['final price'], 
            df[df['return'] > 0]['return'], 
            color='green', alpha=0.4, label='Positive Return')

# Plot points below y=0 in transparent red
plt.scatter(df[df['return'] <= 0]['final price'], 
            df[df['return'] <= 0]['return'], 
            color='red', alpha=0.4, label='Negative Return')


# regression line
x=df['final price']
y=df['return']
coefficients = np.polyfit(x, y, deg=1)
slope, intercept = coefficients
print(f'coefficients: slope {round(slope,5)}, intercept {round(intercept,5)}')

# Predicted values
y_pred = slope * x + intercept
plt.plot(x, y_pred, color='black', label='Fitted Line')

# equation_text = f'Equation: return = 0.5 * (p/p0 -1)'
# plt.text(100, 100, equation_text, fontsize=24, color='blue')

# sns.scatterplot(data=df, x='final price', y='return', alpha=0.6)

# Set title and labels
plt.title(f'Joint Distribution of Return vs Final Price (ratio = {gridstep}, vol = {vol})', fontsize=16)
plt.xlabel('Final Price', fontsize=12)
plt.ylabel('Return', fontsize=12)

# Add a trend line
# sns.regplot(data=df, x='final price', y='return', scatter=False, color='red')

# Improve the layout
# plt.tight_layout()

# Save the plot
plt.savefig(f'return_vs_final_price_scatter_{gridstep}_{vol}.png', dpi=300)

# Display the plot (optional, comment out if running in a non-interactive environment)
# plt.show()

# Print summary statistics
print(df[['return', 'final price','up crossings','down crossings']].describe())
# print(df['down crossings']/(df['up crossings']+df['down crossings']).describe())
# Print correlation coefficient
print("\nCorrelation coefficient between return and final price:")
print(df['return'].corr(df['final price']))