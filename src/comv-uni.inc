C+*********************************************************** comv-uni.inc


C@lbl bvs 1996Nov1 CCP never used
C@empa aw 1995jan09 TIF = unit number for temporary input file (F:) added
CC        INTEGER        Kbd,Crt,CER,SET,CIF,DAF,COF,HEF,CCP,
CC     &  TMS,TMM,TSV,IFS,COU,MSF
        INTEGER        Kbd,Crt,CER,SET,CIF,DAF,COF,HEF,
     &		TMS,TMM,TSV,IFS,COU,MSF,TIF
C change name /UNITS/ to /COMVUNITS/ (coupling with TRNSYS)
CC	COMMON /COMVUNITS/ Kbd,Crt,CER,SET,CIF,DAF,COF,HEF,CCP
CC     &          TMS,TMM,TSV,IFS,COU,MSF
	COMMON /COMVUNITS/ Kbd,Crt,CER,SET,CIF,DAF,COF,HEF,
     &		TMS,TMM,TSV,IFS,COU,MSF,TIF

        INTEGER         UseCRT,Test,IEcho,PEcho,SEcho,OEcho
	COMMON /REPORT/ UseCRT,Test,IEcho,PEcho,SEcho,OEcho
