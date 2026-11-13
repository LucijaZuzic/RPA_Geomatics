import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import matplotlib.pyplot as plt
import seaborn as sns

plt.rcParams['font.family'] = 'Arial'

if not os.path.isdir("Definitions"):
    os.makedirs("Definitions")

def string_for_number(num_val, num_dec, num_comma, use_scientific):
    expnum = 0
    if use_scientific:
        while num_val < 1 and num_val:
            num_val *= 10
            expnum += 1
    if str(num_val) == "nan":
        return ""
    str_val = str(np.round(float(num_val), num_dec))
    str_val_whole, str_val_decimal = str_val.split(".")
    chars_processed = 0
    new_str = ""
    for cnum in range(len(str_val_whole) - 1, -1, -1):
        new_str = str_val[cnum] + new_str
        if chars_processed % 3 == 2:
            new_str = "," + new_str
        chars_processed += 1
    if len(str_val_whole) < num_comma:
        new_str = str_val_whole
    if new_str[0] == ",":
        new_str = new_str[1:]
    scipart = ""
    if expnum:
        scipart = "$\n$\\times {10}^{-" + str(expnum) + "}"
    if str_val_decimal != "0":
        return "$" * use_scientific + new_str + "." + str_val_decimal + scipart + "$" * use_scientific
    else:
        return "$" * use_scientific + new_str + scipart + "$" * use_scientific

nuum, num_ticks, num_decs, num_commas, num_decs2, stepv, spacev = 120, 5, 2, 4, 4, 1, 0.25
data_elem = pd.read_csv(f"examples_indicators_{nuum}.csv", index_col = False)
vars_names = ["RR", "DET", "NRLINE", "L", "ENTR", "rENTR", "LAM", "TT"]
names_translate = {"sin": "Sine", "normal": "Normal", "ar": "AR", "brownian": "Brownian", "logistic": "Logistic", "logistic_linear": "Logistic\n(Linear)"}
cols_translate = {"sin": "#4477AA", "normal": "#EE6677", "ar": "#228833", "brownian": "#CCBB44", "logistic": "#66CCEE", "logistic_linear": "#AA3377"}
plt.figure(figsize = (10.5, 10), dpi = 300)
for i in range(len(vars_names)):
    plt.subplot(2, len(vars_names) // 2, 1 + i)
    cols_names = [cols_translate[n] for n in list(data_elem["name"])]
    tag_names = [names_translate[n] for n in list(data_elem["name"])]
    tag_values = [data_elem[vars_names[i]][j] for j in range(len(tag_names))]
    mini, maxi = min(tag_values), max(tag_values)
    rval = maxi - mini
    buff = rval / num_ticks
    tick_values = [j * (stepv + spacev) for j in range(len(tag_names))]
    for j in range(len(tag_names) - 1, -1, -1):
        plt.text(tag_values[j] + buff / 10, tick_values[j], string_for_number(tag_values[j], num_decs2, num_commas, False), rotation = 90, verticalalignment = "center")
        plt.barh(tick_values[j], tag_values[j], height = stepv, color = [cols_names[j]], label = tag_names[j])
    plt.ylabel("$" + vars_names[i] + "$")
    plt.yticks([])
    if vars_names[i] == "NRLINE":
        plt.legend()
        plt.title("RPA results for characteristic classes of recurrence plots")
    label_values = np.linspace(mini, maxi, num_ticks)
    label_strings = [string_for_number(v, num_decs, num_commas, False) for v in label_values]
    plt.xlim(max(0, mini - buff), maxi + buff)
    plt.xticks(label_values, label_strings, rotation = 90)
#plt.suptitle("RPA results for characteristic classes of recurrence plots")
plt.savefig(f"Definitions/examples_indicators_{nuum}.pdf", bbox_inches = "tight")
plt.close()

posthoc_results = pd.read_csv(f"posthoc_results_{nuum}.csv", index_col = False)
statis = sorted(list(set(posthoc_results["group1"]).union(set(posthoc_results["group2"]))))
plt.figure(figsize = (10, 6.5), dpi = 300)
for i in range(len(vars_names)):
    plt.subplot(2, len(vars_names) // 2, 1 + i)
    partpost = posthoc_results[posthoc_results["Metric"] == vars_names[i]]
    dictip = {s1: {s2: 1 for s2 in statis} for s1 in statis}
    g1 = list(partpost["group1"])
    g2 = list(partpost["group2"])
    pl = list(partpost["p.value"])
    for ixp in range(len(g1)):
        n1, n2, pv = g1[ixp], g2[ixp], pl[ixp]
        dictip[n1][n2] = pv
        dictip[n2][n1] = pv
    if vars_names[i] != "NRLINE":
        plt.title("$" + vars_names[i] + "$")
    else:
        plt.title("Pairwise post-hoc comparisons between stations for each RQA metric ($p$-values)\n$" + vars_names[i] + "$")
    matrp = np.array([[dictip[s1][s2] for s2 in statis] for s1 in statis])
    strp = [[string_for_number(dictip[s1][s2], num_decs2, num_commas, dictip[s1][s2] < 0.05) for s2 in statis] for s1 in statis]
    sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, xticklabels = statis, yticklabels = statis)
plt.savefig(f"Definitions/posthoc_results_{nuum}.pdf", bbox_inches = "tight")
plt.close()

stations_vs_models = pd.read_csv(f"station_vs_models_{nuum}.csv", index_col = False)
trans_stat = {"Wilcoxon_p": "Wilcoxon $p$-value", "T_test_p": "$t$-test $p$-value", "Wilcoxon_p_adj": "Wilcoxon adjusted $p$-value"}
for cstat in trans_stat:
    plt.figure(figsize = (10, 14), dpi = 300)
    for i in range(len(vars_names)):
        plt.subplot(len(vars_names) // 2, 2, 1 + i)
        partpstat = stations_vs_models[stations_vs_models["Metric"] == vars_names[i]]
        dictip = {s1: {m1: 1 for m1 in names_translate} for s1 in statis}
        g1 = list(partpstat["Station"])
        g2 = list(partpstat["Model"])
        pl = list(partpstat[cstat])
        for ixp in range(len(g1)):
            n1, n2, pv = g1[ixp], g2[ixp], pl[ixp]
            dictip[n1][n2] = pv
        if vars_names[i] != "DET":
            plt.title("$" + vars_names[i] + "$")
        else:
            plt.title(f"Comparing stations and models ({trans_stat[cstat]})\n${vars_names[i]}$")
        matrp = np.array([[dictip[s1][m1] for m1 in names_translate] for s1 in statis])
        strp = [[string_for_number(dictip[s1][m1], num_decs2, num_commas, dictip[s1][m1] < 0.05) for m1 in names_translate] for s1 in statis]
        sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, yticklabels = statis, xticklabels = names_translate.values())
    plt.savefig(f"Definitions/station_vs_models_120_{cstat}.pdf", bbox_inches = "tight")
    plt.close()

distance_to_models = pd.read_csv(f"distance_to_models_{nuum}.csv", index_col = False)
trans_stat2 = {"Mahalanobis_Dist": "Mahalanobis distance", "ChiSq_p_value": "$\\chi^2$ $p$-value"}
plt.figure(figsize = (6, 6.5), dpi = 300)
statnum = 0
for cstat2 in trans_stat2:
    plt.subplot(2, 1, 1 + statnum)
    statnum += 1
    dictip = {s1: {m1: 1 for m1 in names_translate} for s1 in statis}
    g1 = list(distance_to_models["Station"])
    g2 = list(distance_to_models["Model"])
    pl = list(distance_to_models[cstat2])
    for ixp in range(len(g1)):
        n1, n2, pv = g1[ixp], g2[ixp], pl[ixp]
        dictip[n1][n2] = pv
    plt.title(f"Comparing stations and models ({trans_stat2[cstat2]})")
    matrp = np.array([[dictip[s1][m1] for m1 in names_translate] for s1 in statis])
    strp = [[string_for_number(dictip[s1][m1], num_decs2, num_commas, dictip[s1][m1] < 0.05) for m1 in names_translate] for s1 in statis]
    sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, yticklabels = statis, xticklabels = names_translate.values())
plt.savefig(f"Definitions/distance_to_models_{nuum}.pdf", bbox_inches = "tight")
plt.close()

z_distance_to_models = pd.read_csv(f"z_distance_to_models_{nuum}.csv", index_col = False)
trans_stat3 = {"Z_Euclidean_Dist": "$z$-score Euclidean distance"}
plt.figure(figsize = (6, 3), dpi = 300)
for cstat3 in trans_stat3:
    dictip = {s1: {m1: 1 for m1 in names_translate} for s1 in statis}
    g1 = list(z_distance_to_models["Station"])
    g2 = list(z_distance_to_models["Model"])
    pl = list(z_distance_to_models[cstat3])
    for ixp in range(len(g1)):
        n1, n2, pv = g1[ixp], g2[ixp], pl[ixp]
        dictip[n1][n2] = pv
    plt.title(f"Comparing stations and models ({trans_stat3[cstat3]})")
    matrp = np.array([[dictip[s1][m1] for m1 in names_translate] for s1 in statis])
    strp = [[string_for_number(dictip[s1][m1], num_decs2, num_commas, dictip[s1][m1] < 0.05) for m1 in names_translate] for s1 in statis]
    sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, yticklabels = statis, xticklabels = names_translate.values())
plt.savefig(f"Definitions/z_distance_to_models_{nuum}.pdf", bbox_inches = "tight")
plt.close()