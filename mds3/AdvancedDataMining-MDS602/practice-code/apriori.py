#!/usr/bin/python

import argparse
import itertools
import collections

def get_frequent_itemsets(transactions, min_support):
    # Count individual items
    freq = collections.defaultdict(int)
    for n, row in enumerate(transactions):
        for item in row:
            freq[frozenset([item])] += 1

    frequent_itemsets = {}

    # frequent 1-itemsets (L1)
    for itemset, count in freq.items():
        if count / n >= min_support:
            frequent_itemsets[itemset] = count

    frequent_n_itemsets = frequent_itemsets.copy()
    for k in range(2, len(freq) + 1):
        all_k_itemset = set()
        for c in itertools.combinations(frequent_n_itemsets.keys(), 2):
            k_itemset = frozenset.union(*c)
            if len(k_itemset) == k:
                all_k_itemset.add(k_itemset)

        new_freq = collections.defaultdict(int)
        for row in transactions:
            row_set = set(row)
            for k_itemset in all_k_itemset:
                new_freq[k_itemset] += k_itemset.issubset(row_set)

        # frequent n-itemsets (L1, L2... Lk)
        frequent_n_itemsets = {
            itemset : count for itemset, count in new_freq.items()
            if count / n >= min_support
        }

        if len(frequent_n_itemsets) == 0: break
        frequent_itemsets.update(frequent_n_itemsets)

    return frequent_itemsets


def gen_rules(frequent_itemsets, min_confidence, n):
    raise NotImplemented


def main(sysArgs):
    dataset = [
        ['Butter', 'Bread', 'Milk'],
        ['Bread', 'Milk'],
        ['Butter', 'Milk'],
        ['Butter', 'Eggs', 'Bread'],
        ['Butter', 'Eggs', 'Bread', 'Milk'],
    ]

    print('Frequent Itemsets:')
    frequent_itemsets = get_frequent_itemsets(dataset, sysArgs.min_support)
    print('sno', 'Support', 'Itemset')
    for i, (itemset, count) in enumerate(frequent_itemsets.items(), 0):
        print('{:3} {:7.4f} {}'.format(i, count/len(dataset), itemset))

    if sysArgs.min_confidence is None: return
    print()

    print('Rule Generation')
    rules = gen_rules(frequent_itemsets, sysArgs.min_confidence, n=len(dataset))
    print('sno', 'Support', 'Confidence', 'Lift', '  Rule')
    for i, rule in enumerate(rules, 1):
        print('{:3} {:7.4f} {:8.4f} {:8.4f} {}'.format(i, rule['support'], rule['confidence'], rule['lift'], rule['rule']))

    print("Plot the rules here")
    itemsets = frequent_itemsets(dataset, minsupport=0.6)
    rules = gen_rules(itemsets, minconfidence=0.7, n=len(dataset))
    print(rules)
    

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument(
        '-s',
        '--min_support',
        default = 0.6,
        type    = float,
        help    = 'minimum support value',
    )

    ap.add_argument(
        '-c',
        '--min_confidence',
        type    = float,
        help    = 'minimum confidence value',
    )

    main(ap.parse_args())