/* Objective function */
max: +420 Large1 +360 Medium1 +300 Small1 +420 Large2 +360 Medium2 +300 Small2 +420 Large3 +360 Medium3 +300 Small3;

/* Constraints */
Con1: +Large1 +Medium1 +Small1 <= 750;
Con2: +Large2 +Medium2 +Small2 <= 900;
Con3: +Large3 +Medium3 +Small3 <= 450;
storage1: +20 Large1 +15 Medium1 +12 Small1 <= 13000;
Storage2: +20 Large2 +15 Medium2 +12 Small2 <= 12000;
Storage33: +20 Large3 +15 Medium3 +12 Small3 <= 5000;
Sale1: +Large1 +Medium1 +Small1 <= 900;
Sale2: +Large2 +Medium2 +Small2 <= 1200;
Sale3: +Large3 +Medium3 +Small3 <= 750;
%C1: +6 Large1 +6 Medium1 +6 Small1 -5 Large2 -5 Medium2 -5 Small2 = 0;
%C2: +3 Large1 +3 Medium1 +3 Small1 -5 Large3 -5 Medium3 -5 Small3 = 0;
