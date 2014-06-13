COMIS 
=====

Multizone Air Flow and Pollutant Simulation

Summary
-------

COMIS (Conjunction of Multizone Infiltration Specialists) is a multizone
airflow simulation program.          

Detailed description
--------------------

Many modules are embedded into COMIS, from air flow components such
as cracks, test data components, windows, doors, vertical apertures
(2-way flow), ducts and duct fittings, fans, flow controllers to
pollutant sources and occupants, plus the possibility of defining 
schedules attached to most of the components (e.g. closing and opening 
of windows in relation to inhabitant behavior). Thermostatic flow 
devices (flow resistance depending on the air flow temperature) can 
also be modeled. All these modules allow for various applications such 
as sizing of mechanical ventilation systems, effects of retrofitting 
measures on ventilation efficiency of buildings, transport of 
contaminants (between zones, but also from outside), ventilation 
effectiveness, pollutant removal efficiency, age of air, smoke 
propagation, assessment of ventilation heat losses, passive cooling, 
assessment of heat transport between zones.

COMIS can be used with Simulation Studio, the powerful and convenient 
simulation environment of TRNSYS. The two programs can be coupled via 
this environment, thus allowing for coupled thermal and air flow 
studies.  

History
-------

COMIS (Conjunction Of Multizone Infiltration Specialists) was 
developed in 1988-89 by ten scientists from nine countries, during a 
twelve-month workshop hosted by the Lawrence Berkeley National 
Laboratory (LBNL). It is based on the result of the International 
Energy Agencies (IEA) Annex 23. Annex 23 was supported between 1990 
and 1996 by nine participating nations: Belgium, Canada, France, 
Greece, Italy, Japan, Switzerland, The Netherlands, and USA. Its 
objectives were to study the physical phenomena causing air flow and 
pollutant transport in multizone buildings, develop numerical modules 
to be integrated in the COMIS multi zone air flow modeling system, and 
evaluate the COMIS code. Annex 23 was dissolved at the end of 1997. 
The official COMIS code was handed over to the Swiss agency EMPA in 
1998, and recent versions (up to COMIS 3.2) have been developed in 
collaboration between EMPA and CSTB. It was released as an open 
source project in 2011.

License
-------

The program is licensed under the LGPL version 3 (or greater).

About This Version
------------------

This version has been slightly modified from the original version to compile
with the free gfortran compiler. The files have been renamed to be all lower
case. There are a number of compiler-specific files:

* comv-cca.for: write console progress lines with CVF
* comv-cqw.for: write progress lines in a window with CVF QuickWin
* comv-win.for: command line parser for use with DVF

and there is one TRNSYS-related file:

* comv-trn.for: TYPE157 subroutine

These files are kept but are not compiled. The command line parser in
comv-unx.for seems to be compatible with gfortran, so that one is used. A 
(hopefully) fairly standard Makefile is included as well as the start of a
true CMake-based build.

