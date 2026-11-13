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
statis = sorted(["Darwin", "Iqaluit", "Matera"])
publication_summary = pd.read_csv(f"publication_summary_{nuum}.csv", index_col = False)
vars_names = ["RR", "DET", "NRLINE", "L", "ENTR", "rENTR", "LAM", "TT"]
names_translate = {"Darwin": "Darwin", "Iqaluit": "Iqaluit", "Matera": "Matera"}
cols_translate = {"Darwin": "#4477AA", "Iqaluit": "#EE6677", "Matera": "#228833"}
plt.figure(figsize = (10.5, 10), dpi = 300)
#Metric,Darwin,Iqaluit,Matera,Kruskal_stat,Kruskal_Epsilon2,Kruskal_p_value,Friedman_Chi,Friedman_Kendall_W
tmetr = {"median": "Median", "IQR": "Interquartile Range"}
iim = 0
for metri in tmetr:
    vls = list(publication_summary["Metric"])
    iqrs_medians = {s1: [float(vvv.split(" ")[iim].replace("(", "").replace(")", "")) for vvv in list(publication_summary[s1])] for s1 in statis}
    iim += 1
    plt.figure(figsize = (10.5, 10), dpi = 300)
    for i in range(len(vars_names)):
        plt.subplot(2, len(vars_names) // 2, 1 + i)
        cols_names = [cols_translate[s1] for s1 in statis]
        tag_names = [names_translate[s1] for s1 in statis]
        numu = vls.index(vars_names[i])
        tag_values = [iqrs_medians[s1][numu] for s1 in statis]
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
            plt.title(f"The {metri} for each RQA metric across the $120$ analysed days in 2014")
        label_values = np.linspace(mini, maxi, num_ticks)
        label_strings = [string_for_number(v, num_decs, num_commas, False) for v in label_values]
        plt.xlim(max(0, mini - buff), maxi + buff)
        plt.xticks(label_values, label_strings, rotation = 90)
    plt.savefig(f"Definitions/publication_summary_{metri}_{nuum}.pdf", bbox_inches = "tight")
    plt.close()
    
vls = list(publication_summary["Metric"])
iqrs_medians1 = {s1: [float(vvv.split(" ")[0].replace("(", "").replace(")", "")) for vvv in list(publication_summary[s1])] for s1 in statis}
iqrs_medians2 = {s1: [float(vvv.split(" ")[1].replace("(", "").replace(")", "")) for vvv in list(publication_summary[s1])] for s1 in statis}
plt.figure(figsize = (10.5, 10), dpi = 300)
for i in range(len(vars_names)):
    plt.subplot(2, len(vars_names) // 2, 1 + i)
    cols_names = [cols_translate[s1] for s1 in statis]
    tag_names = [names_translate[s1] for s1 in statis]
    numu = vls.index(vars_names[i])
    tag_values1 = [iqrs_medians1[s1][numu] for s1 in statis]
    tag_values2 = [iqrs_medians2[s1][numu] for s1 in statis]
    mini, maxi = min(tag_values), max(tag_values)
    rval = maxi - mini
    buff = rval / num_ticks
    tick_values = [j * (stepv + spacev) for j in range(len(tag_names))]
    for j in range(len(tag_names) - 1, -1, -1):
        plt.text(tag_values[j] + buff / 10, tick_values[j], string_for_number(tag_values1[j], num_decs2, num_commas, False) + " (" + string_for_number(tag_values2[j], num_decs2, num_commas, False) + ")", rotation = 90, verticalalignment = "center")
        plt.barh(tick_values[j], tag_values[j], height = stepv, color = [cols_names[j]], label = tag_names[j])
    plt.ylabel("$" + vars_names[i] + "$")
    plt.yticks([])
    if vars_names[i] == "NRLINE":
        plt.legend()
        plt.title(f"The median (IQR in parenthesis) for each RQA metric across the $120$ analysed days in 2014")
    label_values = np.linspace(mini, maxi, num_ticks)
    label_strings = [string_for_number(v, num_decs, num_commas, False) for v in label_values]
    plt.xlim(max(0, mini - buff), maxi + buff)
    plt.xticks(label_values, label_strings, rotation = 90)
plt.savefig(f"Definitions/publication_summary_median_IQR_{nuum}.pdf", bbox_inches = "tight")
plt.close()

station_comparison = pd.read_csv(f"station_comparison_{nuum}.csv", index_col = False)
trans_stat2 = {"ANOVA_p_value": "ANOVA $p$-value", "Kruskal_stat": "Kruskal statistic", "Kruskal_p_value": "Kruskal $p$-value"}
plt.figure(figsize = (6, 2), dpi = 300)
dictip = {cstat2: {v1: 1 for v1 in vars_names} for cstat2 in trans_stat2}
mls = list(station_comparison["Metric"])
for cstat2 in trans_stat2:
    g2 = list(station_comparison[cstat2])
    for ixp in range(len(mls)):
        n2, pv = mls[ixp], g2[ixp]
        dictip[cstat2][n2] = pv
plt.title(f"Omnibus unpaired comparison of RQA metrics across stations ($N = 120$)")
matrp = np.array([[dictip[s1][m1] for m1 in vars_names] for s1 in trans_stat2])
strp = [[string_for_number(dictip[s1][m1], num_decs2, num_commas, dictip[s1][m1] < 0.05) for m1 in vars_names] for s1 in trans_stat2]
sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, yticklabels = [trans_stat2[cstat2] for cstat2 in trans_stat2], xticklabels = ["$" + v + "$" for v in vars_names])
plt.savefig(f"Definitions/station_comparison_{nuum}.pdf", bbox_inches = "tight")
plt.close()

friedman_results = pd.read_csv(f"friedman_results_{nuum}.csv", index_col = False)
trans_stat2 = {"Friedman_Chi": "Friedman $\\chi^2$", "p_value": "$p$-value"}
plt.figure(figsize = (6, 2), dpi = 300)
dictip = {cstat2: {v1: 1 for v1 in vars_names} for cstat2 in trans_stat2}
mls = list(friedman_results["Metric"])
for cstat2 in trans_stat2:
    g2 = list(friedman_results[cstat2])
    for ixp in range(len(mls)):
        n2, pv = mls[ixp], g2[ixp]
        dictip[cstat2][n2] = pv
plt.title(f"Friedman rank-sum test comparing stations ($N = 120$, df $= 2$)")
matrp = np.array([[dictip[s1][m1] for m1 in vars_names] for s1 in trans_stat2])
strp = [[string_for_number(dictip[s1][m1], num_decs2, num_commas, dictip[s1][m1] < 0.05) for m1 in vars_names] for s1 in trans_stat2]
sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, yticklabels = [trans_stat2[cstat2] for cstat2 in trans_stat2], xticklabels = ["$" + v + "$" for v in vars_names])
plt.savefig(f"Definitions/friedman_results_{nuum}.pdf", bbox_inches = "tight")
plt.close()

trans_stat2 = {"Kruskal_stat": "Kruskal statistic", "Kruskal_Epsilon2": "Kruskal $\\varepsilon^2$", "Kruskal_p_value": "Kruskal $p$-value", "Friedman_Chi": "Friedman $\\chi^2$", "Friedman_Kendall_W": "Friedman Kendall $W$"}
plt.figure(figsize = (6, 2.5), dpi = 300)
dictip = {cstat2: {v1: 1 for v1 in vars_names} for cstat2 in trans_stat2}
mls = list(publication_summary["Metric"])
for cstat2 in trans_stat2:
    g2 = list(publication_summary[cstat2])
    for ixp in range(len(mls)):
        n2, pv = mls[ixp], g2[ixp]
        dictip[cstat2][n2] = pv
plt.title(f"Non-parametric omnibus test statistics and bounded effect sizes")
matrp = np.array([[dictip[s1][m1] for m1 in vars_names] for s1 in trans_stat2])
strp = [[string_for_number(dictip[s1][m1], num_decs2, num_commas, dictip[s1][m1] < 0.05) for m1 in vars_names] for s1 in trans_stat2]
sns.heatmap(matrp, annot = strp, fmt = "", cbar = False, yticklabels = [trans_stat2[cstat2] for cstat2 in trans_stat2], xticklabels = ["$" + v + "$" for v in vars_names])
plt.savefig(f"Definitions/publication_summary_other_{nuum}.pdf", bbox_inches = "tight")
plt.close()