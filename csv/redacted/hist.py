import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sys

# tag = '1.01_20'
# tag = '1.04_5'
# tag = "1.04_10"

gridStep = float(sys.argv[2])
numberOfPoints = int(sys.argv[1])
rangeMultiplier = gridStep ** numberOfPoints

print(f"numberOfPoints  : {numberOfPoints}")
print(f"gridStep        : {gridStep}")
print(f"rangeMultiplier : {round(rangeMultiplier, 2)}")

tag =  sys.argv[1] + "_" + sys.argv[2]

# Read the CSV file
df = pd.read_csv(f'test_sim_stopped_{tag}.csv',
                 sep=',')
                #  skipinitialspace=True)  # Skip spaces after separator

print(df)
df.columns = df.columns.str.strip()
print(df.columns)

# buy and hold return - Kandle return
# df['B&H'] = 1/2 * (df['final price'] - 1) - df['return']
# print(df.columns)


# # histogram 1
# plt.figure(figsize=(10, 6))
# plt.hist(df['return'], bins=50, edgecolor='black')
# plt.title('Distribution of returns')
# plt.xlabel('Returns')
# plt.ylabel('Frequency')
# plt.grid(True, linestyle='--', alpha=0.7)
# plt.savefig(f'distributions_histogram_{tag}_stopped.png')

# # histogram 2
# plt.figure(figsize=(10, 6))
# plt.hist(df['B&H'], bins=50, edgecolor='black')
# plt.title('B&H - Kandle return')
# plt.xlabel('Returns')
# plt.ylabel('Frequency')
# plt.grid(True, linestyle='--', alpha=0.7)
# plt.savefig(f'distributions_histogram_{tag}_BH_stopped.png')
# # plt.show()

# scatter plots
plt.figure(figsize=(12, 8))
# Plot points above y=0 in transparent green
plt.scatter(df[df['return'] > 0]['final price'], 
            df[df['return'] > 0]['return'], 
            color='green', alpha=0.3, label='Positive Return')
# Plot points below y=0 in transparent red
plt.scatter(df[df['return'] <= 0]['final price'], 
            df[df['return'] <= 0]['return'], 
            color='red', alpha=0.3, label='Negative Return')
plt.axhline(y=0, color='black', linestyle='--', linewidth=0.5)

# sns.scatterplot(data=df, x='final price', y='return', alpha=0.6)

plt.title('Joint Distribution of Return vs Final Price', fontsize=16)
plt.xlabel('Final Price', fontsize=12)
plt.ylabel('Return', fontsize=12)

# Add a trend line
# sns.regplot(data=df, x='final price', y='return', scatter=False, color='red')

plt.tight_layout()
plt.savefig(f'return_vs_final_price_scatter_{tag}_stopped.png', dpi=300)

# summary statistics
print(df[['return', 'final price']].describe())
print("\nCorrelation coefficient between return and final price:")
print(df['return'].corr(df['final price']))