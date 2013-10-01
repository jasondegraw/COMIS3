C+********************************************************* comv-phy.inc
C This contains physical constants
C
C I can imagine that more physical constants can be moved to here
C
C COMMON for physical constants
C GasConst =Universal GasConstant         287.055  kJ/kg/K
C                                           8.31441 J/mol/K
C MolVol   =Volume of one Mol             22.414   m3/kmol
C@NBI PGS 2000Oct31 - MolVol is strictly not a constant, but depends
C@NBI                 upon temperature & pressure, and is governed by
C@NBI                 the ideal gas law (PV=nRT).  At STP (273.15 K and
C@NBI                 101325 Pa) MolVol = 22.414, but in COMIS, it is
C@NBI                 more sensible to operate with MolVol at room
C@NBI                 temperature of, say 20°C (24.055 m3/kmol) as we
C@NBI                 aren't interested in pollutant concentration at
C@NBI                 other temperatures. This is important, because the
C@NBI                 error in pollutant concentration can exceed 7%.
C Tzero    =Temperature of 0 degC in K    273.15   K
C MMair    =Molar Mass of dry air         28.9645  kg/kmol
C MMH2O    =Molar Mass of water vapour    18.01534 kg/kmol
C CpAir    =Specific heat of air          1004     kJ/kg/K
C LamAir   =heat conductivity coef of air 0.025    W/m/K

C***********************************************************************

       REAL GasConst,MolVol,Tzero,MMair,MMH2O,CpAir,LamAir
       COMMON / PHYSIC/ GasConst,MolVol,Tzero,MMair,MMH2O,
     *          CpAir,LamAir
