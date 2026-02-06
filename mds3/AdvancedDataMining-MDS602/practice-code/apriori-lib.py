#!../venv/bin/python

import pandas as pd
import mlxtend.preprocessing
import mlxtend.frequent_patterns

# 1. Prepare the dataset (list of lists)
dataset = [
    {'Butter', 'Bread', 'Milk'},
    {'Bread', 'Milk'},
    {'Butter', 'Milk'},
    {'Butter', 'Eggs', 'Bread'},
    {'Butter', 'Eggs', 'Bread', 'Milk'},
]

# 2. Transform data into one-hot encoded format
encoder = mlxtend.preprocessing.TransactionEncoder()
matrix = encoder.fit(dataset).transform(dataset)
df = pd.DataFrame(matrix, columns=encoder.columns_)
print(df)

# 3. Apply Apriori to find frequent itemsets (min_support = 0.6)
frequent_itemsets = mlxtend.frequent_patterns.apriori(df, min_support=0.6, use_colnames=True)

# 4. Generate association rules (min_confidence = 0.7)
rules = mlxtend.frequent_patterns.association_rules(frequent_itemsets, metric='confidence', min_threshold=0.7)

print('Frequent Itemsets:')
print(frequent_itemsets)
print()
print('Association Rules:')
print(rules[['antecedents', 'consequents', 'support', 'confidence', 'lift']])
