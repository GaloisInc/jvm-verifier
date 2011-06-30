f(x,y) = y**2 - x**3 + x - 1;
set xrange [-3:3];
set yrange [-3:3];
set view 0,0;
set isosample 1000,1000;
set table;
set output "temp.dat"
set size square;
set cont base;
set cntrparam levels incre 0,0.1,0;
unset surface;
splot f(x,y)
unset table;

set terminal svg size 800, 800
set output "eccplot.svg"
set xlabel
unset clabel
unset key
plot "temp.dat" w l 
