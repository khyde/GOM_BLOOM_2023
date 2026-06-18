; $ID:	GOM_TRIPOS_CPR.PRO,	2025-01-22-11,	USER-KJWH	$
  PRO GOM_TRIPOS_CPR

;+
; NAME:
;   GOM_TRIPOS_CPR
;
; PURPOSE:
;   $PURPOSE$
;
; PROJECT:
;   GOM_BLOOM_2023
;
; CALLING SEQUENCE:
;   GOM_TRIPOS_CPR,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
;
; REQUIRED INPUTS:
;   Parm1.......... Describe the positional input parameters here. 
;
; OPTIONAL INPUTS:
;   Parm2.......... Describe optional inputs here. If none, delete this section.
;
; KEYWORD PARAMETERS:
;   KEY1........... Document keyword parameters like this. Note that the keyword is shown in ALL CAPS!
;
; OUTPUTS:
;   OUTPUT.......... Describe the output of this program or function
;
; OPTIONAL OUTPUTS:
;   None
;
; COMMON BLOCKS: 
;   None
;
; SIDE EFFECTS:  
;   None
;
; RESTRICTIONS:  
;   None
;
; EXAMPLE:
; 
;
; NOTES:
;   From Chris Melrose regarding the CPR data:
;   1. They use the old Ceratium taxonomy, but Ceratium tripos should be the one of interest. I highlighted that column.
;   2. I also included the other species of the genus for comparison, and the dinoflagellate cysts. However, cysts could be from any dino since they aren't identified further.
;   3. You'll see the "sample ID" column has a bunch of codes. Basically in the format of a cruise number and a then a sample number within the cruise. Any sample with "MC" is from the Gulf of Maine route, MB is the Mid-Atlantic route. 
;   4. Samples correspond to roughly 10 nautical miles along the transect, with the position given being the estimated center location.
;   5. There is a "chlorophyll index" column. This has four possible values: 0, 1, 2 or 6.5. It is based on the color of the silk when compared to standard color cards through visual assessment. 0 being no pigment detectable, and 6.5 representing very green/lots of pigment.
;   
; COPYRIGHT: 
; Copyright (C) 2025, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on January 22, 2025 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Jan 22, 2025 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'GOM_TRIPOS_CPR'
  COMPILE_OPT IDL3
  SL = PATH_SEP()
  
  MP = 'NES'
  MS = MAPS_SIZE(MP, PX=PX, PY=PY)
  LAND = READ_LANDMASK(MP)
  
  DIR_PROJ = !S.GOM_BLOOM_2023
  DIR_PNG  = DIR_PROJ + 'PNGS' + SL & DIR_TEST, DIR_PNG
  CPR_DATA = DIR_PROJ + 'DATA' + SL + 'CPR_data.csv'

  CPR = CSV_READ(CPR_DATA)
  DATES = NUM2STR(CPR.YEAR)+ADD_STR_ZERO(NUM2STR(CPR.MONTH))+ADD_STR_ZERO(NUM2STR(CPR.DAY))

  OK = WHERE_STRING(CPR.SAMPLE_ID,'MC',COMPLEMENT=COMP)
  GOM = CPR[OK]
  MAB = CPR[COMP]
  LAT = CPR.LATITUDE
  LON = CPR.LONGITUDE
  TRIPOS = CPR.CERATIUM_TRIPOS
  SATFILE = GET_FILES('OCCCI',PRODS='CHLOR_A-CCI',PERIODS='M',DATERANGE='2023')
  
  SPROD = 'CHLOR_A_0.1_30'
  CB_TITLE = UNITS('CHLOROPHYLL')
  TICKVALUES = []
  
  RANGES = [0,15000,35000,65000,95000,130000,170000,225000,300000]
  SIZES   = [0.15,0.66,1,1.33,1.66,2,2.33,2.66,3,3.33,3.66]
  
  BARR = BYTARR(1024,1024)
  FSZ = [14]
  
  MAPS_SET,MP
  PXPY=MAP_DEG2IMAGE(MAPS_BLANK(MP), LON, LAT, X=X, Y=Y,  AROUND=0, SUBS=SUBS)
  ZWIN
  
  
  PIONEER = DIR_PROJ + 'DATA' + SL + 'PIONEER_ARRAY_COORDINATES.csv'
  PDATA = CSV_READ(PIONEER)
  MAPS_SET, MP
  LL = MAP_DEG2IMAGE(MAPS_BLANK(MP),PDATA.LON,PDATA.LAT,X=PNX,Y=PNY)
  ZWIN

  LATS = [36,38,40,42,44]              & LATNAMES = '$' + ['36','38','40','42','44'] + '^{o}N$'
  LONS = [-76,-74,-72,-70,-68,-66,-64] & LONNAMES = '$' + ['76','74','72','70','68','66','64'] + '^{o}W$'
  LL = MAPS_2LONLAT(MP)
  LATL = [] & LATR = [] & LONT = [] & LONB = []
  FOR L=0, N_ELEMENTS(LATS)-1 DO BEGIN
    OKL = WHERE_NEAREST(LL.LATS[0,*],FLOAT(LATS[L]),NEAR=0.01,COUNT)
    IF OKL NE [] THEN LATL = [LATL,OKL] ELSE STOP
    OKR = WHERE_NEAREST(LL.LATS[-1,*],FLOAT(LATS[L]),NEAR=0.01,COUNT)
    IF OKR NE [] THEN LATR = [LATR,OKR] ELSE STOP
  ENDFOR

  FOR L=0, N_ELEMENTS(LONS)-1 DO BEGIN
    OKT = WHERE_NEAREST(LL.LONS[*,0],LONS[L],NEAR=0.01,COUNT)
    IF OKT NE [] THEN LONT = [LONT,OKT] ELSE STOP
    OKB = WHERE_NEAREST(LL.LONS[*,-1],LONS[L],NEAR=0.01,COUNT)
    IF OKB NE [] THEN LONB = [LONT,OKB] ELSE STOP
  ENDFOR
  
 ; FS = FSZ[FTH]
  OUTFILE = DIR_PNG +'TRIPOS_2023.png'
 ; IF KEY(ADD_BATHY) THEN OUTFILE = REPLACE(OUTFILE,'LONLAT','LONLAT_BATHY_'+NUM2STR(STRJOIN(ABS(DEPTH),'_')))
 ; IF FILE_MAKE([SATFILE,DFILE],OUTFILE,OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE

  DAT = STACKED_READ(SATFILE)
  MONDAT = MAPS_REMAP(DAT.CHLOR_A_MEAN[*,*,6],MAP_IN=DAT.INFO.MAP,BINS=DAT.BINS,MAP_OUT=MP)

  EXT = 1
 ; W = WINDOW(DIMENSIONS=[PX,PY],BUFFER=1,BACKGROUND_COLOR='BLACK')
  PRODS_2PNG, DATA_IMAGE=MONDAT, MAPP=MP,PROD=SPROD,/ADD_CB,CB_TITLE=CB_TITLE,/ADD_BATHY,/NO_SAVE, OBJ=OBJ, BUFFER=1,IMG_DIMS=[PX,PY],$
    CB_SIZE=16,CB_STYLE='BOLD'
;  STACKED_2PNGS, SATFILE,MAPP=MP,PNGPROD=SPROD,DATERANGE=['20230601','20230630'],DEPTH=DEPTH,BATHY_THICK=3,BATHY_COLOR=0,/ADD_CB,CB_TITLE=TITLE,CB_TYPE=3,CB_POS=[.03,.91,0.45,.94],$
;    CB_TICKVALUES=TICKVALUES,CB_SIZE=16,CB_STYLE='BOLD',/DEVICE,/CURRENT

  BOX = POLYLINE([0.0,1.0,1.0,0.0,0.0],[0.0,0.0,1.0,1.0,0.0],COLOR='BLACK',THICK=15)

;  TXT = TEXT(.25,.945,LABEL,FONT_SIZE=18,FONT_STYLE='BOLD',ALIGNMENT=0.5)
  TXT = []
  
 ; FOR L=0, N_ELEMENTS(LATS)-1 DO TL = TEXT(5,LATL[L]+EXT,LATNAMES[L],ALIGNMENT=0.0,/DEVICE,FONT_SIZE=FS,FONT_STYLE='BOLD',VERTICAL_ALIGNMENT=0.5)
 ; FOR L=0, N_ELEMENTS(LONS)-1 DO TL = TEXT(LONB[L]+EXT,5,LONNAMES[L],ALIGNMENT=0.5,/DEVICE,FONT_SIZE=FS,FONT_STYLE='BOLD',VERTICAL_ALIGNMENT=0.0)

  FOR R=0, N_ELEMENTS(RANGES)-1 DO BEGIN
   OK = WHERE(TRIPOS GE RANGES[R] AND DATES GE 20230101,COUNT)
   ;  TXT = [TXT,NUM2STR(ARANGE[R]) + ' - ' + NUM2STR(ARANGE[R+1])]
   IF COUNT GE 1 THEN S = SYMBOL(X[OK],Y[OK],'CIRCLE',/DEVICE,SYM_COLOR='RED',SYM_FILLED=1,SYM_THICK=SYM_THICK,SYM_SIZE=SIZES[R],SYM_TRANSPARENCY=50)
 ENDFOR
  
  FOR R=0, N_ELEMENTS(RANGES)-1 DO BEGIN
    OK = WHERE(TRIPOS GE RANGES[R] AND DATES LT 20230101,COUNT)
  ;  TXT = [TXT,NUM2STR(ARANGE[R]) + ' - ' + NUM2STR(ARANGE[R+1])]
    IF COUNT GE 1 THEN S = SYMBOL(X[OK],Y[OK],'CIRCLE',/DEVICE,SYM_COLOR='WHITE',SYM_FILLED=SYM_FILLED,SYM_THICK=SYM_THICK,SYM_SIZE=SIZES[R],SYM_TRANSPARENCY=SYM_TRANSPARENCY)
  ENDFOR
  
  
  
;  OK = WHERE(FLOAT(SET.(POS)) GE ARANGE[R],COUNT)
;  IF COUNT GE 1 THEN S = SYMBOL(X[OK],Y[OK],'CIRCLE',/DEVICE,SYM_FILLED=SYM_FILLED,SYM_COLOR='BLACK',SYM_THICK=SYM_THICK,SYM_SIZE=SIZES[R],SYM_TRANSPARENCY=SYM_TRANSPARENCY)
;  TXT = [TXT,'> '+NUM2STR(ARANGE[R])]
;
;  BOT = 45
;  TOP = BOT + MAX(YPOS) + YPOS[0] ; IF N_ELEMENTS(TXT) EQ 5 THEN TOP = 245 ELSE TOP = 345
;  BOX = POLYLINE([760,990,990,760,760],[BOT,BOT,TOP,TOP,BOT],/DEVICE,COLOR='BLACK',THICK=2)
;
;  FOR N=0,N_ELEMENTS(TXT)-1 DO BEGIN
;    S = SYMBOL(795,TOP-YPOS[N],'CIRCLE',/DEVICE,SYM_COLOR='BLACK',SYM_FILLED=SYM_FILLED,SYM_THICK=SYM_THICK,SYM_SIZE=SIZES[N]) ; ,LABEL_STRING='  '+TXT(N),LABEL_POSITION='R',LABEL_FONT_SIZE=16
;    SPOS = S.POSITION
;    T = TEXT(835,SPOS[1]*1024,/DEVICE,TXT[N],COLOR='BLACK', FONT_SIZE=FS,FONT_STYLE='BOLD',VERTICAL_ALIGNMENT=0.5)
;  ENDFOR
;
;  T2 = TEXT(875,TOP+35,/DEVICE,PROD_TITLE,FONT_SIZE=FS,ALIGNMENT=0.5,FONT_STYLE='BOLD')
;  T3 = TEXT(875,TOP+5,/DEVICE,PROD_UNITS,FONT_SIZE=FS,ALIGNMENT=0.5,FONT_STYLE='BOLD')
  PFILE, OUTFILE
  OBJ.SAVE,OUTFILE,RESOLUTION=300
  OBJ.CLOSE
STOP

END ; ***************** End of GOM_TRIPOS_CPR *****************
