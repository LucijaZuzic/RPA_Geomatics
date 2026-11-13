import matplotlib.pyplot as plt

lns, ncols = dict(), dict()
lns[1] = ["Parameter", "Value", "Notes",
"Processing mode", "Kinematic\n(post-processing)", "Standard point positioning",
"Sampling interval", "$30$ s", "IGS standard",
"Frequency", "Single-frequency (L1,\n1575.42 MHz)", "Commercial-grade setup",
"Elevation mask angle", "$15\\degree$", "Excludes low-elevation satellites",
"Ephemeris source", "Broadcast ephemeris", "Standard GNSS model",
"Ionosphere model", "Off / Klobuchar\nbaseline", "Raw L1 pseudoranges retained to preserve ionospheric signature",
"Troposphere model", "Saastamoinen", "Standard tropospheric delay model",
"Multipath mitigation", "Choke-ring antenna at\nIGS site", "Isolates ionospheric error dynamics",
"Output", "Easting, Northing\nerrors (horizontal)", "Geodesic distance in metres",
"Reference position", "Known IGS station\ncoordinates", "Ground-truth baseline"]
ncols[1] = 3
lns[2] = ["Predictor", "Description",
"$RR$ (recurrence rate)", "Measures the percentage of recurrent points falling within a tolerance radius $\\varepsilon$ in the\n$N \\times N$ recurrence matrix ($0\\%$ to $100\\%$)",
"$DET$ (determinism)", "The percentage of recurrent points that form diagonal line structures of at least minimum\nlength $l_{\\min} = 2$, reflecting system predictability",
"$NRLINE$", "The total number of diagonal lines in the recurrence plot",
"$L$", "The average length of diagonal lines, describing the mean duration of similar trajectory\nevolution",
"$ENTR$ (Shannon entropy)", "The Shannon information entropy of the diagonal line length distribution, quantifying\ndynamical complexity",
"$rENTR$", "Normalised Shannon entropy (entropy normalised by the number of diagonal line\nlengths)",
"$LAM$ (laminarity)", "The percentage of recurrent points that form vertical line structures of at least minimum\nlength $v_{\\min} = 2$, capturing state persistence",
"$TT$ (trapping time)", "The average length of vertical line structures, measuring the mean time the system\nremains trapped in a laminar state"]
ncols[2] = 2
lns[3] = ["Research question", "Statistical approach", "R function",
"Do the $3$ stations differ in\nRQA metrics? (Unpaired\ndistribution comparison)", "One-Way ANOVA\n(parametric) and\nKruskal-Wallis +\nBH-adjusted Wilcoxon\n(non-parametric)", "aov(), kruskal.test(),\npairwise.wilcox.test()",
"Do the $3$ stations differ\nwhen matched by DOY?\n(Paired by day of year)", "Non-parametric Friedman\nRank Sum Test + Kendall's\n$W$", "friedman.test()",
"Do the $3$ stations differ across\nall $8$ metrics jointly?\n(Multivariate profile)", "Multivariate Analysis of\nVariance (MANOVA,\nPillai's trace)", "manova()",
"Does a station match a\nsynthetic model on\nindividual metrics?\n(Sample vs reference value)", "One-Sample $t$-test and\nOne-Sample Wilcoxon\nSigned-Rank Test against\n$\\mu = \\text{model value}$", "t.test(x, mu = val),\nwilcox.test(x, mu =\nval)",
"Which dynamical regime\nbest describes each station?\n(Multi-metric classification)", "Mahalanobis Distance and\nStandardised Euclidean\n($z$-score) Distance + PCA\nBiplot", "mahalanobis(), prcomp()"]
ncols[3] = 3
for i in lns:
    plt.figure(dpi = 300)
    plt.axis("off")
    plt.ylim(0, len(lns[i]) // ncols[i] - 1)
    ixi = 0
    isfirst = True
    for j in range(0, len(lns[i]), ncols[i]):
        cs = lns[i][j:j + ncols[i]]
        ixi += 1
        for jxi in range(ncols[i]):
            ismono = i == 3 and jxi == 2 and ixi != 1
            plt.text(jxi / 2,
                     (len(lns[i]) - ixi * ncols[i]) // ncols[i],
                     cs[jxi],
                     horizontalalignment = "center" * (i == 3) + "left" * (i != 3),
                     verticalalignment = "center",
                     fontfamily = "sans-serif" * (not ismono) + "monospace" * (ismono),
                     fontweight = "normal" * (not isfirst) + "bold" * (isfirst))
        isfirst = False
    plt.savefig(f"Definitions/tab_{i}.pdf", bbox_inches = "tight")
    plt.close()