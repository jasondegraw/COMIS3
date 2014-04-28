project(comis3)

cmake_minimum_required(VERSION 2.8)

enable_language(Fortran)

set(SRCS_WITH_WARNINGS comv-feq.for
                       comv-rex.for
                       comv-flo.for)

set(SRCS comv-mai.for
         comv-ch2.for
         comv-che.for
         comv-cra.for
         comv-dat.for
         comv-deb.for
         comv-do2.for
         comv-eda.for
         comv-fe8.for
         comv-flb.for
         comv-gr2.for
         comv-gr3.for
         comv-gr4.for
         comv-gro.for
         comv-hva.for
         comv-in1.for
         comv-in2.for
         comv-in3.for
         comv-in4.for
         comv-in5.for
         comv-in6.for
         comv-in7.for
         comv-in8.for
         comv-inh.for
         comv-inp.for
         comv-lar.for
         comv-lsl.for
         comv-mat.for
         comv-mul.for
         comv-new.for
         comv-occ.for
         comv-ou2.for
         comv-ou3.for
         comv-ou4.for
         comv-out.for
         comv-phy.for
         comv-pol.for
         comv-pos.for
         comv-pr2.for
         comv-pre.for
         comv-prj.for
         comv-slv.for
         comv-tim.for
         comv-tm1.for
         comv-tm2.for
         comv-tm3.for
         comv-tm4.for
         comv-tm5.for
         comv-tm6.for
         comv-tm7.for
         comv-tm8.for
         comv-uni.for
         comv-unx.for
         comv-usr.for
         comv-ut0.for
         comv-ut1.for
         comv-ut2.for
         comv-ut4.for
         comv-ut5.for
         comv-uti.for)

add_executable(comis3 ${SRCS} ${SRCS_WITH_WARNINGS})
