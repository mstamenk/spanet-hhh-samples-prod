
################################################################################
#
# This gnuplot file was generated by MadGraph5_aMC@NLO project, a program which 
# automatically generates Feynman diagrams and matrix elements for arbitrary
# high-energy processes in the Standard Model and beyond. It also perform the
# integration and/or generate events for these processes, at LO and NLO accuracy.
#
# For more information, visit madgraph.phys.ucl.ac.be and amcatnlo.web.cern.ch
#
################################################################################
# Automatic plotting from MG5aMC
reset

set lmargin 10
set rmargin 0
set terminal postscript portrait enhanced color "Helvetica" 9 
# The pdf terminal offers transparency support, but you will have to adapt things a bit
#set terminal pdf enhanced font "Helvetica 12" lw 1.0 dashed size 29.7cm, 21cm
set key font ",9"
set key samplen "2"
set output "central_qCut_djr_plots.ps"

# This is the "PODO" color palette of gnuplot v.5, but with the order
# changed: palette of colors selected to be easily distinguishable by
# color-blind individuals with either protanopia or deuteranopia. Bang
# Wong [2011] Nature Methods 8, 441.

set style line   1 lt 1 lc rgb "#009e73" lw 1.3
set style line 101 lt 1 lc rgb "#009e73" lw 1.3 dt (6,3)
set style line  11 lt 2 lc rgb "#009e73" lw 1.3 dt (6,3)
set style line  21 lt 4 lc rgb "#009e73" lw 1.3 dt (3,2)
set style line  31 lt 6 lc rgb "#009e73" lw 1.3 dt (2,1)
set style line  41 lt 8 lc rgb "#009e73" lw 1.3 dt (4,3)

set style line   2 lt 1 lc rgb "#0072b2" lw 1.3
set style line 102 lt 1 lc rgb "#0072b2" lw 1.3 dt (6,3)
set style line  12 lt 2 lc rgb "#0072b2" lw 1.3 dt (6,3)
set style line  22 lt 4 lc rgb "#0072b2" lw 1.3 dt (3,2)
set style line  32 lt 6 lc rgb "#0072b2" lw 1.3 dt (2,1)
set style line  42 lt 8 lc rgb "#0072b2" lw 1.3 dt (4,3)

set style line   3 lt 1 lc rgb "#d55e00" lw 1.3
set style line 103 lt 1 lc rgb "#d55e00" lw 1.3 dt (6,3)
set style line  13 lt 2 lc rgb "#d55e00" lw 1.3 dt (6,3)
set style line  23 lt 4 lc rgb "#d55e00" lw 1.3 dt (3,2)
set style line  33 lt 6 lc rgb "#d55e00" lw 1.3 dt (2,1)
set style line  43 lt 8 lc rgb "#d55e00" lw 1.3 dt (4,3)

set style line   4 lt 1 lc rgb "#f0e442" lw 1.3
set style line 104 lt 1 lc rgb "#f0e442" lw 1.3 dt (6,3)
set style line  14 lt 2 lc rgb "#f0e442" lw 1.3 dt (6,3)
set style line  24 lt 4 lc rgb "#f0e442" lw 1.3 dt (3,2)
set style line  34 lt 6 lc rgb "#f0e442" lw 1.3 dt (2,1)
set style line  44 lt 8 lc rgb "#f0e442" lw 1.3 dt (4,3)

set style line   5 lt 1 lc rgb "#56b4e9" lw 1.3
set style line 105 lt 1 lc rgb "#56b4e9" lw 1.3 dt (6,3)
set style line  15 lt 2 lc rgb "#56b4e9" lw 1.3 dt (6,3)
set style line  25 lt 4 lc rgb "#56b4e9" lw 1.3 dt (3,2)
set style line  35 lt 6 lc rgb "#56b4e9" lw 1.3 dt (2,1)
set style line  45 lt 8 lc rgb "#56b4e9" lw 1.3 dt (4,3)

set style line   6 lt 1 lc rgb "#cc79a7" lw 1.3
set style line 106 lt 1 lc rgb "#cc79a7" lw 1.3 dt (6,3)
set style line  16 lt 2 lc rgb "#cc79a7" lw 1.3 dt (6,3)
set style line  26 lt 4 lc rgb "#cc79a7" lw 1.3 dt (3,2)
set style line  36 lt 6 lc rgb "#cc79a7" lw 1.3 dt (2,1)
set style line  46 lt 8 lc rgb "#cc79a7" lw 1.3 dt (4,3)

set style line   7 lt 1 lc rgb "#e69f00" lw 1.3
set style line 107 lt 1 lc rgb "#e69f00" lw 1.3 dt (6,3)
set style line  17 lt 2 lc rgb "#e69f00" lw 1.3 dt (6,3)
set style line  27 lt 4 lc rgb "#e69f00" lw 1.3 dt (3,2)
set style line  37 lt 6 lc rgb "#e69f00" lw 1.3 dt (2,1)
set style line  47 lt 8 lc rgb "#e69f00" lw 1.3 dt (4,3)

set style line   8 lt 1 lc rgb "black" lw 1.3
set style line 108 lt 1 lc rgb "black" lw 1.3 dt (6,3)
set style line  18 lt 2 lc rgb "black" lw 1.3 dt (6,3)
set style line  28 lt 4 lc rgb "black" lw 1.3 dt (3,2)
set style line  38 lt 6 lc rgb "black" lw 1.3 dt (2,1)
set style line  48 lt 8 lc rgb "black" lw 1.3 dt (4,3)


set style line 999 lt 1 lc rgb "gray" lw 1.3

safe(x,y,a) = (y == 0.0 ? a : x/y)

set style data histeps
set key invert



################################################################################
### Rendering of the plot titled 'log10d01 []'
################################################################################

set multiplot
set label "log10d01 []" font ",13" at graph 0.04, graph 1.05
set xrange [1.4100e+00:2.8500e+00]
set bmargin 0 
set tmargin 0
set xtics nomirror
set ytics nomirror
set mytics 10
set xtics auto
set key horizontal noreverse maxcols 1 width -4 
set label front 'MadGraph5\_aMC\@NLO' font "Courier,11" rotate by 90 at graph 1.02, graph 0.04

#-- rendering subhistograms 'None and None results'

set format y '10^{%T}'
set yrange [1.7345e-07:1.3355e-03]
set origin 0.0000e+00, 5.0000e-01
set size 1.0000e+00, 4.0000e-01
set mytics 10
set ytics auto
set format x ''
set logscale y
set ylabel "{/Symbol s} per bin [pb]"

plot \
'central_qCut_djr_plots.HwU' index 5 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 106 title '',\
'central_qCut_djr_plots.HwU' index 5 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 106 title '',\
'central_qCut_djr_plots.HwU' index 5 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 6 title '',\
'central_qCut_djr_plots.HwU' index 5 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 6 title 'jet sample 4',\
'central_qCut_djr_plots.HwU' index 4 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 105 title '',\
'central_qCut_djr_plots.HwU' index 4 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 105 title '',\
'central_qCut_djr_plots.HwU' index 4 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 5 title '',\
'central_qCut_djr_plots.HwU' index 4 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 5 title 'jet sample 3',\
'central_qCut_djr_plots.HwU' index 3 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 104 title '',\
'central_qCut_djr_plots.HwU' index 3 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 104 title '',\
'central_qCut_djr_plots.HwU' index 3 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 4 title '',\
'central_qCut_djr_plots.HwU' index 3 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 4 title 'jet sample 2',\
'central_qCut_djr_plots.HwU' index 2 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 103 title '',\
'central_qCut_djr_plots.HwU' index 2 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 103 title '',\
'central_qCut_djr_plots.HwU' index 2 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 3 title '',\
'central_qCut_djr_plots.HwU' index 2 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 3 title 'jet sample 1',\
'central_qCut_djr_plots.HwU' index 1 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 102 title '',\
'central_qCut_djr_plots.HwU' index 1 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 102 title '',\
'central_qCut_djr_plots.HwU' index 1 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 2 title '',\
'central_qCut_djr_plots.HwU' index 1 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 2 title 'jet sample 0',\
'central_qCut_djr_plots.HwU' index 0 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 101 title '',\
'central_qCut_djr_plots.HwU' index 0 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 101 title '',\
'central_qCut_djr_plots.HwU' index 0 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 1 title '',\
'central_qCut_djr_plots.HwU' index 0 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 1 title 'all jet samples'
#-- rendering subhistograms 'Relative scale and PDF uncertainty'
unset label
unset format
set yrange [-1.4800e+00:1.4000e+00]
set origin 0.0000e+00, 3.5000e-01
set size 1.0000e+00, 1.5000e-01
set mytics 2
set ytics auto
set format x
unset logscale y
set ylabel "(1) rel.unc."
set label "Relative uncertainties w.r.t. central values" font ",9" front at graph 0.03, graph 0.13
plot \
0.0 ls 999 title '',\
'central_qCut_djr_plots.HwU' index 0 using (($1+$2)/2):(0.0):(safe($4,$3,0.0)) w yerrorbar ls 1 title ''

unset label

################################################################################

################################################################################
### Rendering of the plot titled 'log10d12 []'
################################################################################

set multiplot
set label "log10d12 []" font ",13" at graph 0.04, graph 1.05
set xrange [1.2600e+00:2.7000e+00]
set bmargin 0 
set tmargin 0
set xtics nomirror
set ytics nomirror
set mytics 10
set xtics auto
set key horizontal noreverse maxcols 1 width -4 
set label front 'MadGraph5\_aMC\@NLO' font "Courier,11" rotate by 90 at graph 1.02, graph 0.04

#-- rendering subhistograms 'None and None results'

set format y '10^{%T}'
set yrange [1.7345e-07:1.4754e-03]
set origin 0.0000e+00, 5.0000e-01
set size 1.0000e+00, 4.0000e-01
set mytics 10
set ytics auto
set format x ''
set logscale y
set ylabel "{/Symbol s} per bin [pb]"

plot \
'central_qCut_djr_plots.HwU' index 11 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 106 title '',\
'central_qCut_djr_plots.HwU' index 11 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 106 title '',\
'central_qCut_djr_plots.HwU' index 11 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 6 title '',\
'central_qCut_djr_plots.HwU' index 11 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 6 title 'jet sample 4',\
'central_qCut_djr_plots.HwU' index 10 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 105 title '',\
'central_qCut_djr_plots.HwU' index 10 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 105 title '',\
'central_qCut_djr_plots.HwU' index 10 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 5 title '',\
'central_qCut_djr_plots.HwU' index 10 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 5 title 'jet sample 3',\
'central_qCut_djr_plots.HwU' index 9 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 104 title '',\
'central_qCut_djr_plots.HwU' index 9 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 104 title '',\
'central_qCut_djr_plots.HwU' index 9 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 4 title '',\
'central_qCut_djr_plots.HwU' index 9 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 4 title 'jet sample 2',\
'central_qCut_djr_plots.HwU' index 8 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 103 title '',\
'central_qCut_djr_plots.HwU' index 8 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 103 title '',\
'central_qCut_djr_plots.HwU' index 8 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 3 title '',\
'central_qCut_djr_plots.HwU' index 8 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 3 title 'jet sample 1',\
'central_qCut_djr_plots.HwU' index 7 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 102 title '',\
'central_qCut_djr_plots.HwU' index 7 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 102 title '',\
'central_qCut_djr_plots.HwU' index 7 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 2 title '',\
'central_qCut_djr_plots.HwU' index 7 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 2 title 'jet sample 0',\
'central_qCut_djr_plots.HwU' index 6 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 101 title '',\
'central_qCut_djr_plots.HwU' index 6 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 101 title '',\
'central_qCut_djr_plots.HwU' index 6 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 1 title '',\
'central_qCut_djr_plots.HwU' index 6 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 1 title 'all jet samples'
#-- rendering subhistograms 'Relative scale and PDF uncertainty'
unset label
unset format
set yrange [-1.4800e+00:1.4000e+00]
set origin 0.0000e+00, 3.5000e-01
set size 1.0000e+00, 1.5000e-01
set mytics 2
set ytics auto
set format x
unset logscale y
set ylabel "(1) rel.unc."
set label "Relative uncertainties w.r.t. central values" font ",9" front at graph 0.03, graph 0.13
plot \
0.0 ls 999 title '',\
'central_qCut_djr_plots.HwU' index 6 using (($1+$2)/2):(0.0):(safe($4,$3,0.0)) w yerrorbar ls 1 title ''

unset label

################################################################################

################################################################################
### Rendering of the plot titled 'log10d23 []'
################################################################################

set multiplot
set label "log10d23 []" font ",13" at graph 0.04, graph 1.05
set xrange [8.7000e-01:2.6100e+00]
set bmargin 0 
set tmargin 0
set xtics nomirror
set ytics nomirror
set mytics 10
set xtics auto
set key horizontal noreverse maxcols 1 width -4 
set label front 'MadGraph5\_aMC\@NLO' font "Courier,11" rotate by 90 at graph 1.02, graph 0.04

#-- rendering subhistograms 'None and None results'

set format y '10^{%T}'
set yrange [1.7345e-07:1.7150e-03]
set origin 0.0000e+00, 5.0000e-01
set size 1.0000e+00, 4.0000e-01
set mytics 10
set ytics auto
set format x ''
set logscale y
set ylabel "{/Symbol s} per bin [pb]"

plot \
'central_qCut_djr_plots.HwU' index 17 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 106 title '',\
'central_qCut_djr_plots.HwU' index 17 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 106 title '',\
'central_qCut_djr_plots.HwU' index 17 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 6 title '',\
'central_qCut_djr_plots.HwU' index 17 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 6 title 'jet sample 4',\
'central_qCut_djr_plots.HwU' index 16 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 105 title '',\
'central_qCut_djr_plots.HwU' index 16 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 105 title '',\
'central_qCut_djr_plots.HwU' index 16 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 5 title '',\
'central_qCut_djr_plots.HwU' index 16 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 5 title 'jet sample 3',\
'central_qCut_djr_plots.HwU' index 15 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 104 title '',\
'central_qCut_djr_plots.HwU' index 15 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 104 title '',\
'central_qCut_djr_plots.HwU' index 15 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 4 title '',\
'central_qCut_djr_plots.HwU' index 15 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 4 title 'jet sample 2',\
'central_qCut_djr_plots.HwU' index 14 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 103 title '',\
'central_qCut_djr_plots.HwU' index 14 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 103 title '',\
'central_qCut_djr_plots.HwU' index 14 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 3 title '',\
'central_qCut_djr_plots.HwU' index 14 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 3 title 'jet sample 1',\
'central_qCut_djr_plots.HwU' index 13 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 102 title '',\
'central_qCut_djr_plots.HwU' index 13 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 102 title '',\
'central_qCut_djr_plots.HwU' index 13 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 2 title '',\
'central_qCut_djr_plots.HwU' index 13 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 2 title 'jet sample 0',\
'central_qCut_djr_plots.HwU' index 12 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 101 title '',\
'central_qCut_djr_plots.HwU' index 12 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 101 title '',\
'central_qCut_djr_plots.HwU' index 12 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 1 title '',\
'central_qCut_djr_plots.HwU' index 12 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 1 title 'all jet samples'
#-- rendering subhistograms 'Relative scale and PDF uncertainty'
unset label
unset format
set yrange [-1.4800e+00:1.4000e+00]
set origin 0.0000e+00, 3.5000e-01
set size 1.0000e+00, 1.5000e-01
set mytics 2
set ytics auto
set format x
unset logscale y
set ylabel "(1) rel.unc."
set label "Relative uncertainties w.r.t. central values" font ",9" front at graph 0.03, graph 0.13
plot \
0.0 ls 999 title '',\
'central_qCut_djr_plots.HwU' index 12 using (($1+$2)/2):(0.0):(safe($4,$3,0.0)) w yerrorbar ls 1 title ''

unset label

################################################################################

################################################################################
### Rendering of the plot titled 'log10d34 []'
################################################################################

set multiplot
set label "log10d34 []" font ",13" at graph 0.04, graph 1.05
set xrange [7.2000e-01:2.1900e+00]
set bmargin 0 
set tmargin 0
set xtics nomirror
set ytics nomirror
set mytics 10
set xtics auto
set key horizontal noreverse maxcols 1 width -4 
set label front 'MadGraph5\_aMC\@NLO' font "Courier,11" rotate by 90 at graph 1.02, graph 0.04

#-- rendering subhistograms 'None and None results'

set format y '10^{%T}'
set yrange [1.7345e-07:1.5178e-03]
set origin 0.0000e+00, 5.0000e-01
set size 1.0000e+00, 4.0000e-01
set mytics 10
set ytics auto
set format x ''
set logscale y
set ylabel "{/Symbol s} per bin [pb]"

plot \
'central_qCut_djr_plots.HwU' index 23 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 106 title '',\
'central_qCut_djr_plots.HwU' index 23 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 106 title '',\
'central_qCut_djr_plots.HwU' index 23 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 6 title '',\
'central_qCut_djr_plots.HwU' index 23 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 6 title 'jet sample 4',\
'central_qCut_djr_plots.HwU' index 22 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 105 title '',\
'central_qCut_djr_plots.HwU' index 22 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 105 title '',\
'central_qCut_djr_plots.HwU' index 22 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 5 title '',\
'central_qCut_djr_plots.HwU' index 22 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 5 title 'jet sample 3',\
'central_qCut_djr_plots.HwU' index 21 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 104 title '',\
'central_qCut_djr_plots.HwU' index 21 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 104 title '',\
'central_qCut_djr_plots.HwU' index 21 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 4 title '',\
'central_qCut_djr_plots.HwU' index 21 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 4 title 'jet sample 2',\
'central_qCut_djr_plots.HwU' index 20 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 103 title '',\
'central_qCut_djr_plots.HwU' index 20 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 103 title '',\
'central_qCut_djr_plots.HwU' index 20 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 3 title '',\
'central_qCut_djr_plots.HwU' index 20 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 3 title 'jet sample 1',\
'central_qCut_djr_plots.HwU' index 19 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 102 title '',\
'central_qCut_djr_plots.HwU' index 19 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 102 title '',\
'central_qCut_djr_plots.HwU' index 19 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 2 title '',\
'central_qCut_djr_plots.HwU' index 19 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 2 title 'jet sample 0',\
'central_qCut_djr_plots.HwU' index 18 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)):4 w yerrorbar ls 101 title '',\
'central_qCut_djr_plots.HwU' index 18 using (($1+$2)/2):($3 >= 0 ? sqrt(-1) : abs($3)) ls 101 title '',\
'central_qCut_djr_plots.HwU' index 18 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3):4 w yerrorbar ls 1 title '',\
'central_qCut_djr_plots.HwU' index 18 using (($1+$2)/2):($3 < 0 ? sqrt(-1) : $3) ls 1 title 'all jet samples'
#-- rendering subhistograms 'Relative scale and PDF uncertainty'
unset label
unset format
set yrange [-1.4800e+00:1.4000e+00]
set origin 0.0000e+00, 3.5000e-01
set size 1.0000e+00, 1.5000e-01
set mytics 2
set ytics auto
set format x
unset logscale y
set ylabel "(1) rel.unc."
set label "Relative uncertainties w.r.t. central values" font ",9" front at graph 0.03, graph 0.13
plot \
0.0 ls 999 title '',\
'central_qCut_djr_plots.HwU' index 18 using (($1+$2)/2):(0.0):(safe($4,$3,0.0)) w yerrorbar ls 1 title ''

unset label

################################################################################
unset multiplot
!ps2pdf "central_qCut_djr_plots.ps" &> /dev/null