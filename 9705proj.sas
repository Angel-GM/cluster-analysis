data countries;
infile 'data-scored-avg-1000.csv' delimiter=',';
input COUNTRY $ EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
proc standard data=countries out=countries mean=0 std=1;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run; 
proc cluster data=countries outtree=unitree method=average nonorm;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
id COUNTRY;
run;
proc tree data=unitree nclusters=6 out=newdata noprint;
id COUNTRY;
copy EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
proc sort data=newdata;
by cluster;
run;
proc print data=newdata;
var COUNTRY cluster;
run;
proc means data=newdata;
by cluster;
output out=Seeds mean=EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
proc fastclus data=countries maxc=6 maxiter=50 seed=Seeds out=Clus_out;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
id COUNTRY;
run;
proc sort data=Clus_out;
by cluster distance;
run;
proc print data=Clus_out;
var COUNTRY cluster distance;
run;
proc candisc data=Clus_out out=ProCan(keep=COUNTRY cluster Can1 Can2) noprint;
class cluster;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
goptions reset=all;
symbol pointlabel=("#COUNTRY") value=dot;
proc gplot data=ProCan;
plot Can2*Can1=cluster / vaxis=axis2 haxis=axis1 nolegend;
axis1 label=("z1" justify=center);
axis2 label=("z2" justify=center r=0 a=90);
run;





/** KMEANS***/
title 'KMEAN method';
data countries;
infile 'data-scored-avg-1000.csv' delimiter=',';
input COUNTRY $ EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
proc standard data=countries out=countries mean=0 std=1;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
proc fastclus data=countries maxc=6 replace=none maxiter=20 radius=3 out=Clus_out;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
id COUNTRY;
run;
proc sort data=Clus_out;
by cluster distance;
run;
proc print data=Clus_out;
var COUNTRY cluster distance;
run;
proc candisc data=Clus_out noprint out=ProCan(keep=COUNTRY cluster Can1 Can2);
class cluster;
var EXTROVERSION	AGREEABLENESS	OPENNES	CONCIENTIOUSNESS	NEUROTICISM;
run;
goptions reset=all;
symbol pointlabel=("#COUNTRY") value=dot;
proc gplot data=ProCan;
plot Can2*Can1=cluster / vaxis=axis2 haxis=axis1 nolegend;
axis1 label=("z1" justify=center);
axis2 label=("z2" justify=center a=90);
run;
