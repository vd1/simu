import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Read the CSV file
df = pd.read_csv('test_sim.csv',
                 sep=',')
                #  skipinitialspace=True)  # Skip spaces after separator

print(df)
print(df.columns)
df.columns = df.columns.str.strip()

plt.figure(figsize=(10, 6))
plt.hist(df['return'], bins=50, edgecolor='black')

# Set title and labels
plt.title('Distribution of returns')
plt.xlabel('Returns')
plt.ylabel('Frequency')

# Add grid
plt.grid(True, linestyle='--', alpha=0.7)

# Save the plot
plt.savefig('distributions_histogram_1.04_5.png')

# plt.show()

plt.figure(figsize=(12, 8))
# Plot points above y=0 in transparent green
plt.scatter(df[df['return'] > 0]['final price'], 
            df[df['return'] > 0]['return'], 
            color='green', alpha=0.4, label='Positive Return')

# Plot points below y=0 in transparent red
plt.scatter(df[df['return'] <= 0]['final price'], 
            df[df['return'] <= 0]['return'], 
            color='red', alpha=0.4, label='Negative Return')

# sns.scatterplot(data=df, x='final price', y='return', alpha=0.6)

# Set title and labels
plt.title('Joint Distribution of Return vs Final Price', fontsize=16)
plt.xlabel('Final Price', fontsize=12)
plt.ylabel('Return', fontsize=12)

# Add a trend line
# sns.regplot(data=df, x='final price', y='return', scatter=False, color='red')

# Improve the layout
plt.tight_layout()

# Save the plot
plt.savefig('return_vs_final_price_scatter_1.04_5.png', dpi=300)

# Display the plot (optional, comment out if running in a non-interactive environment)
# plt.show()

# Print summary statistics
print(df[['return', 'final price']].describe())

# Print correlation coefficient
print("\nCorrelation coefficient between return and final price:")
print(df['return'].corr(df['final price']))