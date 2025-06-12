import pandas as pd
import numpy as np
import random
import itertools

import rpy2.robjects as robjects

# Load the RDS file

np.random.seed(37)
random.seed(37)

height = [
    np.random.normal(5.5, 1.0, 100),
    np.random.normal(5.3, 1.0, 200),
    np.random.normal(5.9, 1.0, 300),
    np.random.normal(5.7, 1.0, 200),
    np.random.normal(5.3, 1.0, 400),
    np.random.normal(5.2, 1.0, 500),
    np.random.normal(5.8, 1.0, 300),
    np.random.normal(5.5, 1.0, 200)
]

demographic = [
    ['white', 'minor', 'male'],
    ['white', 'minor', 'female'],
    ['white', 'adult', 'male'],
    ['white', 'adult', 'female'],
    ['other', 'minor', 'male'],
    ['other', 'minor', 'female'],
    ['other', 'adult', 'male'],
    ['other', 'adult', 'female']
]
a=zip(demographic, height)

for d, s in zip(demographic, height):
  print(d)
  print(s)

data = [[{'race': d[0], 'age': d[1], 'gender': d[2], 'height': h} for h in s]
        for d, s in zip(demographic, height)]
data = list(itertools.chain(*data))

df = pd.DataFrame(data)
df.head()

def get_target_marginals(d):
    factors = list(d.keys())
    aa=d.items()
    #targets = [sorted([(k2, v2) for k2, v2 in v.items()]) for k, v in d.items()]
    targets = [([(k2, v2) for k2, v2 in v.items()]) for k, v in d.items()]
    for item in targets:
        a=item
        for _, v in item:
            aaa=v
    aaa=[[v for _, v in item] for item in targets]
    #targets = np.array([[v for _, v in item] for item in targets])
    targets_name = ([[v for v, _ in item] for item in targets])
    targets_value = ([[v for _, v in item] for item in targets])
    aaa=1
    return factors, targets_name, targets_value

def get_table(df, targets):
    factors,target_name, target_marginals = get_target_marginals(targets)
  #  aaa=df[factors[0]]
  #  cross_tab = pd.crosstab(df[factors[0]], [df[c] for c in factors[1:]])
  #  shape = tuple([df[c].unique().shape[0] for c in factors])
   # aaa=cross_tab.values
    table=np.ones((4,3,5))

   # ini_data = robjects.r['readRDS']('Charlotte_income_ini.rds')
   # tensor_data = np.array(ini_data)
   # table = tensor_data
    table = np.random.rand(4, 3, 5)
    #table = cross_tab.values.reshape(shape)

    return factors, target_name, target_marginals, table


f, u_name, u, X = get_table(df, {
    'House Size': {'0-1499':0.432751091703057, '1500-2499':0.312336244541485, '2500-3499':0.129585152838428, '3500+':0.125327510917031},
    #'House Bedroom': {'1': 0.09726210989, '2': 0.21740379092,'3':0.42188397472,'4 or more': 0.26345012445},
    'AC type': {'Central':0.923565705866102, 'Room':0.0764342941338981, 'None':0.0001},
    'Income':{'<15k':0.105757472566671, '15k-34999':0.187347199522719, '35k-74999':0.316535650973835, '75k-99999':0.128590951606107, '>100k':0.261768725330669}})


def get_coordinates(M):
    for n in M.shape:
        a=n
        aa=list(range(n))
        aab=[list(range(n)) for n in M.shape]
        c=list(itertools.product(*[[1,3],[1,2],[22]]))
        aaaa=list(itertools.product(*[list(range(n)) for n in M.shape]))
        aaa=1
    return list(itertools.product(*[list(range(n)) for n in M.shape]))

def get_marginals(M, i):
    coordinates = get_coordinates(M)

    key = lambda tup: tup[0]
    counts = [(c[i], M[c]) for c in coordinates]
    cc=counts.copy()
    counts = sorted(counts, key=key)
    counts = itertools.groupby(counts, key=key)
    counts = {k: sum([v[1] for v in g]) for k, g in counts}

    return counts

def get_all_marginals(M):
    a=[[v for _, v in get_marginals(M, i).items()]
                     for i in range(len(M.shape))]
    return ([[v for _, v in get_marginals(M, i).items()]
                     for i in range(len(M.shape))])

def get_counts(M, i):
    coordinates = get_coordinates(M)

    key = lambda tup: tup[0]
    counts = [(c[i], M[c], c) for c in coordinates]
    counts = sorted(counts, key=key)
    counts = itertools.groupby(counts, key=key)
    counts = {k: [(tup[1], tup[2]) for tup in g] for k, g in counts}

    return counts

def update_values(M, i, u):
    marg = get_marginals(M, i)
    vals = get_counts(M, i)

    d = [[(c, n * u[k] / marg[k]) for n, c in v] for k, v in vals.items()]
    d = itertools.chain(*d)
    d = list(d)

    return d

def ipf_update(M, u):
    aaa=range(len(M.shape))
    for i in range(len(M.shape)):
        aaa=u[i]
        values = update_values(M, i, u[i])
        for idx, v in values:
            M[idx] = v

    o = get_all_marginals(M)
    d = get_deltas(o, u)

    return M, d

def get_deltas(o, t):
    aaa=len(o)
    #aa=np.linalg.norm(np.array(o[1]-t[1]), 2)
    return np.array([np.linalg.norm(np.array(o[r]) - np.array(t[r]), 2) for r in range(len(o))])

def get_weights(X, max_iters=50, zero_threshold=0.0001, convergence_threshold=3, debug=True):
    M = X.copy()

    d_prev = np.zeros(len(M.shape))
    count_zero = 0

    for _ in range(max_iters):
        M, d_next = ipf_update(M, u)
        d = np.linalg.norm(d_prev - d_next, 2)

        if d < zero_threshold:
            count_zero += 1

        if debug:
            print(','.join([f'{v:.5f}' for v in d_next]), d)
        d_prev = d_next

        if count_zero >= convergence_threshold:
            break

    w = M / M.sum()
    return w, M
w, M= get_weights(X)
list_final = []

for index1,element1 in enumerate(M):
    u_name1=u_name[0][index1]
    for index2,element2 in enumerate(element1):
        u_name2=u_name[1][index2]
        aaa={ 'House Size': u_name1, 'AC Type':u_name2}
                #{'vintage': u_name1, 'hearting fuel': u_name2, 'gender': d[2], 'height': h}
        total = sum(element2)
        new_element = element2.copy()/total
    #new_element = element4.copy()
        new_element[new_element < 10 ** (-4)] = 0
        for d, s in zip(u_name[2], new_element):
            aaa[d]=s
            # a.extend(element2)
        total2=sum(new_element)
        aaa['counts']=total2
        list_final.append(aaa)

df = pd.DataFrame(list_final)
column_sum=df.sum()
#column_sum2=df[u_name5].sum()

df.to_csv('./StLouis_random.csv')