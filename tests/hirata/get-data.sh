url="https://hirata-lab.chemistry.illinois.edu/"
files=(
ccsd_e.out
ccsd_t1.out
ccsd_t2.out
ccsdt_e.out
ccsdt_t1.out
ccsdt_t2.out
ccsdt_t3.out
ccsdtq_e.out
ccsdtq_t1.out
ccsdtq_t2.out
ccsdtq_t3.out
ccsdtq_t4.out
ccsd_lambda1.out
ccsd_lambda2.out
ccsdt_lambda1.out
ccsdt_lambda2.out
ccsdt_lambda3.out
ccsdtq_lambda1.out
ccsdtq_lambda2.out
ccsdtq_lambda3.out
ccsdtq_lambda4.out
ccsd_density1.out
ccsdt_density1.out
ccsdtq_density1.out
eomccsd_x1.out
eomccsd_x2.out
eomccsd_y1.out
eomccsd_y2.out
eomccsdt_x1.out
eomccsdt_x2.out
eomccsdt_x3.out
eomccsdt_y1.out
eomccsdt_y2.out
eomccsdt_y3.out
eomccsdtq_x1.out
eomccsdtq_x2.out
eomccsdtq_x3.out
eomccsdtq_x4.out
eomccsdtq_y1.out
eomccsdtq_y2.out
eomccsdtq_y3.out
eomccsdtq_y4.out
ipeomccsd_x1.out
ipeomccsd_x2.out
ipeomccsdt_x1.out
ipeomccsdt_x2.out
ipeomccsdt_x3.out
ipeomccsdtq_x1.out
ipeomccsdtq_x2.out
ipeomccsdtq_x3.out
ipeomccsdtq_x4.out
eaeomccsd_x1.out
eaeomccsd_x2.out
eaeomccsdt_x1.out
eaeomccsdt_x2.out
eaeomccsdt_x3.out
eaeomccsdtq_x1.out
eaeomccsdtq_x2.out
eaeomccsdtq_x3.out
eaeomccsdtq_x4.out
eaeomccsd3p2h_x1.out
eaeomccsd3p2h_x2.out
eaeomccsd3p2h_x3.out
eaeomccsdt4p3h_x1.out
eaeomccsdt4p3h_x2.out
eaeomccsdt4p3h_x3.out
eaeomccsdt4p3h_x4.out
eomccsd_density1.out
eomccsdt_density1.out
eomccsdtq_density1.out
eomccsd_denominator.out
eomccsdt_denominator.out
eomccsdtq_denominator.out
ccsd_t_singles.out
ccsd_t_doubles.out
ccsd2_t_left.out
ccsd2_q_left.out
ccsd2_q_right.out
ccsdt2_q_left.out
ccsdt2_q_right.out
cr_ccsd_t_D.out
cr_ccsd_t_E.out
cr_ccsd_t_N.out
cisd_e.out
cisd_c1.out
cisd_c2.out
cisdt_e.out
cisdt_c1.out
cisdt_c2.out
cisdt_c3.out
cisdtq_e.out
cisdtq_c1.out
cisdtq_c2.out
cisdtq_c3.out
cisdtq_c4.out
mbpt1_t1.out
mbpt1_t2.out
mbpt2_e.out
mbpt2_t1.out
mbpt2_t2.out
mbpt2_t2_b.out
mbpt2_t3.out
mbpt2_t4.out
mbpt3_e.out
mbpt3_t1.out
mbpt3_t2.out
mbpt4_e.out
ccd_e.out
ccd_t2.out
lccd_e.out
lccd_t2.out
lccsd_e.out
lccsd_t1.out
lccsd_t2.out
qcisd_e.out
qcisd_t1.out
qcisd_t2.out
cis_x1.out
cis_density1.out
cis_denominator.out
cis2_d.out
cis2_tf.out
cis3_dd.out
cis3_ttf.out
cis3_dtf.out
cis4_d.out
cis4_tf.out
)

for f in ${files[@]}; do
  method=${f%_*}
  mkdir -p $method
  echo $method
  wget $url/$f -O $method/$f &
done

wait
