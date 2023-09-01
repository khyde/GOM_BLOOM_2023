; $ID:	GOM_CHL_2023_VERSION.PRO,	2023-07-28-17,	USER-KJWH	$
  FUNCTION GOM_CHL_2023_VERSION, VERSION

;+
; NAME:
;   GOM_CHL_2023_VERSION
;
; PURPOSE:
;   Set up the defaults for the GOM_CHL_2023 outputs
;
; PROJECT:
;   GOM_CHL_2023
;
; CALLING SEQUENCE:
;   Result = GOM_CHL_2023_VERSION($Parameter1$, $Parameter2$, $Keyword=Keyword$, ...)
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
;   OUTPUT.......... Decribe the output of this program or function
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
;   $Citations or any other useful notes$
;   
; COPYRIGHT: 
; Copyright (C) 2023, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on July 28, 2023 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Jul 28, 2023 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'GOM_CHL_2023_VERSION'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF ~N_ELEMENTS(VERSION) THEN VERSION = ['V2023']                       ; Each year, add the next version

  VSTR = []                                                       ; Create a null variable for the version structure
  ; ===> Loop throug the version
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]

    ; ===> Make the project directories
    DIR_PRO = !S.GOM_CHL_2023
    DIR_FILES = DIR_PRO + 'FILES' + SL
    DIR_VER = DIR_PRO + 'OUTPUTS' + SL ;+ VER + SL
    DIRNAMES = ['EXTRACTS','PNGS','COMP_ANIMATIONS','ANIMATIONS','COMPOSITES','LOGS','NETCDF']
    DNAME = 'DIR_'  + DIRNAMES                                                                      ; The tag name for the directory in the structure
    DIRS  = DIR_VER + DIRNAMES + SL                                                                 ; The actual directory name
    DIR_TEST, DIRS                                                                                  ; Make the output directories if they don't already exist
    DSTR = CREATE_STRUCT('DIR_PROJECT',DIR_PRO,'DIR_FILES',DIR_FILES,'DIR_VERSION',DIR_VER)                               ; Create the directory structure
    FOR D=0, N_ELEMENTS(DIRS)-1 DO DSTR=CREATE_STRUCT(DSTR,DNAME[D],DIRS[D])                        ; Add each directory to the structure

    ; ===> Default product information
    DOWNLOAD_PRODS = ['CHLOR_A','SST']
    PROCESS_PRODS = ['PPD','PSC','CHLOR_A','SST'] 

    CHL_DATASET = 'GLOBCOLOUR' & CHL_ALG = 'GSM' & CHL_TEMP = 'GLOBCOLOUR'
    SST_DATASET  = 'MUR' & SST_TEMP = 'MUR'
    MAP_OUT  = 'GOM'                                                                            ; The map to be used for any plots
    PRODS = ['CHLOR_A','SST','GRAD_CHL','GRAD_SST','PSC_'+['MICRO','NANO','PICO'],'PSC_F'+['MICRO','NANO','PICO']]
    STAT_PERIODS = ['W','WEEK','M','MONTH']
    ANOM_PERIODS=['W','M']
    PNG_PRODS = ['CHLOR_A','SST','PSC_MICRO','PSC_FMICRO','PSC_NANO','PSC_FNANO','PPD']
    PNG_PERIODS = ['W']
    NETCDF_PRODS = PNG_PRODS
    NETCDF_PERIODS = PNG_PERIODS
    ANIMATION_PRODS = ['CHLOR_A']
    COMP_ANIMATION_PRODS = ['CHLOR_A']
    FRONT_PRODS = ['GRAD_CHL']
    EXTRACT_PRODS = ['CHLOR_A','SST','MICRO','NANO','PICO',['MICRO','NANO','PICO']+'_PERCENTAGE']
    EXTRACT_PERIODS = ['W','WEEK','M','MONTH']
    CHL_DATASET = 'GLOBCOLOUR' & CHL_ALG = 'GSM' & CHL_TEMP = 'GLOBCOLOUR'
    SLA_DATASET = 'CMES' & SLA_TEMP = 'CMES'
    OCN_DATASET = 'CMES' & OCN_TEMP = 'CMES'
    GRADCHL_DATASET = 'GLOBCOLOUR' & GC_TEMP='GLOBCOLOUR' & GCHL_ALG = 'BOA'
    GRADSST_DATASET = 'MUR' & GS_TEMP='MUR' & GSST_ALG = 'BOA'
    TEMP_PRODS = ['CHLOR_A','MICRO','NANO','PICO',['MICRO','NANO','PICO']+'_PERCENTAGE']
    PSC_DATASET = 'OCCCI' & PSC_TEMP = 'GLOBCOLOUR' & PSC_ALG = 'TURNER'
    GRID_PERIOD = 'W'

    ; ===> Change the specific product information based on the version
    CASE VER OF
      'V2023': BEGIN
        GOM_YR = '2023'
        SST_DATASET  = 'ACSPONRT'
        SST_TEMP = 'ACSPONRT'
      END
    ENDCASE ; VER

    FULL_DATERANGE = GET_DATERANGE(['1998',GOM_YR])
    DATERANGE = GET_DATERANGE(GOM_YR)                                                     ; The full date range of the current year
    PREVIOUS_DATERANGE = GET_DATERANGE(GOM_YR-1)                                                  ; The date range of the previous year

    HSTR = []
    FOR S=0, N_ELEMENTS(SHPFILE)-1 DO BEGIN
      SHPS = READ_SHPFILE(SHPFILE[S], MAPP=MAP_OUT)
      SUBAREAS = TAG_NAMES(SHPS) & SUBAREAS = SUBAREAS[WHERE(SUBAREAS NE 'OUTLINE' AND SUBAREAS NE 'MAPPED_IMAGE')]
      OUTLINE = []
      SUBAREA_TITLES = []
      FOR F=0, N_ELEMENTS(SUBAREAS)-1 DO BEGIN
        POS = WHERE(TAG_NAMES(SHPS) EQ STRUPCASE(SUBAREAS[F]),/NULL)
        OUTLINE = [OUTLINE,SHPS.(POS).OUTLINE]
        TITLE = SHPS.(POS).SUBAREA_TITLE
        SUBAREA_TITLES = [SUBAREA_TITLES,TITLE]
      ENDFOR
      STR = CREATE_STRUCT('MAP_OUT',MAP_OUT,'SHAPEFILE',SHPFILE, 'SUBAREA_NAMES',SUBAREAS,'SUBAREA_TITLES',SUBAREA_TITLES,'SUBAREA_OUTLINE',OUTLINE)
      HSTR = CREATE_STRUCT(HSTR,SHPFILE[S],STR)
    ENDFOR

    RESOLUTION=300

    ISTR = CREATE_STRUCT('YEAR',GOM_YR,'DATERANGE',DATERANGE,'PREVIOUS_DATERANGE',PREVIOUS_DATERANGE,'FULL_DATERANGE',FULL_DATERANGE,'MAP_OUT',MAP_OUT,'RESOLUTION',RESOLUTION, $  ; 'DATAFILE',DATFILE,
      'STAT_PERIODS',STAT_PERIODS,'ANOM_PERIODS',ANOM_PERIODS,'PNG_PRODS',PNG_PRODS,'PNG_PERIODS',PNG_PERIODS,'ANIMATION_PRODS',ANIMATION_PRODS,'COMP_ANIMATION_PRODS',COMP_ANIMATION_PRODS,'NETCDF_PRODS',NETCDF_PRODS,'NETCDF_PERIODS',NETCDF_PERIODS,'DOWNLOAD_PRODS',DOWNLOAD_PRODS,'PROCESS_PRODS',PROCESS_PRODS,'TEMP_PRODS',TEMP_PRODS,'FRONT_PRODS',FRONT_PRODS,'EXTRACT_PRODS',EXTRACT_PRODS,'EXTRACT_PERIODS',EXTRACT_PERIODS,'GRID_PERIOD',GRID_PERIOD)

    PSTR = []
    FOR P=0, N_ELEMENTS(PRODS)-1 DO BEGIN
      SPROD = PRODS[P]
      ATITLE = ''
      MONTH_SCALE = []
      CASE SPROD OF
        'CHLOR_A':  BEGIN & DTSET=CHL_DATASET & TPSET=CHL_TEMP & SPROD=SPROD+'-'+CHL_ALG & DWLPROD='CHL1' & PTAG='MED'   & PSCALE='CHLOR_A_0.1_10' & PAL='PAL_NAVY_GOLD'  & ASCALE='RATIO_0.1_10'    & APAL='PAL_BLUEGREEN_ORANGE' & ATITLE='Chlorophyll Anomaly (Ratio)' & END
        'SST':      BEGIN & DTSET=SST_DATASET & TPSET=SST_TEMP & SPROD=SPROD             & DWLPROD='' & PTAG='AMEAN' & PSCALE='SST_0_30'       & PAL='PAL_BLUERED'   & ASCALE='DIF_-5_5' & APAL='PAL_SUNSHINE_DIF' & ATITLE='SST Anomaly ' + UNITS('SST',/NO_NAME)
        MONTH_SCALE = CREATE_STRUCT('M01', 'SST_0_30',$
          'M02', 'SST_0_30',$
          'M03', 'SST_0_30',$
          'M04', 'SST_5_30',$
          'M05', 'SST_5_30',$
          'M06', 'SST_5_30',$
          'M07', 'SST_10_30',$
          'M08', 'SST_15_30',$
          'M09', 'SST_15_30',$
          'M10', 'SST_10_30',$
          'M11', 'SST_5_30',$
          'M12', 'SST_5_30')
      END
      'SEALEVEL': BEGIN & DTSET=SLA_DATASET & TPSET=SLA_TEMP & SPROD=SPROD & DWLPROD='SEALEVEL_NRT' & PTAG='' & PSCALE='' & PAL='' & ASCALE='' & APAL='' & END
      'OCEAN':    BEGIN & DTSET=OCN_DATASET & TPSET=OCN_TEMP & SPROD=SPROD & DWLPROD='OCEAN_NRT'    & PTAG='' & PSCALE='' & PAL='' & ASCALE='' & APAL='' & END
      'GRAD_CHL': BEGIN & DTSET=GRADCHL_DATASET & TPSET=GS_TEMP & SPROD=SPROD+'-'+GCHL_ALG & DWLPROD='' & PTAG='' & PSCALE='' & PAL='' & ASCALE='' & APAL='' & END
      'GRAD_SST': BEGIN & DTSET=GRADSST_DATASET & TPSET=GC_TEMP & SPROD=SPROD+'-'+GSST_ALG & DWLPROD='' & PTAG='' & PSCALE='' & PAL='' & ASCALE='' & APAL='' & END
      'PSC_FMICRO': BEGIN & DTSET=PSC_DATASET & TPSET=PSC_TEMP & SPROD=SPROD+'-'+PSC_ALG & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='NUM_0.0_1.0'    & PAL='PAL_DEFAULT'    & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='NUM_0_0.8' & APAL='PAL_BLUEGREEN_ORANGE' & END
      'PSC_FNANO':  BEGIN & DTSET=PSC_DATASET & TPSET=PSC_TEMP & SPROD=SPROD+'-'+PSC_ALG & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='NUM_0.0_1.0'    & PAL='PAL_DEFAULT'    & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='NUM_0_0.8' & APAL='PAL_BLUEGREEN_ORANGE' & END
      'PSC_FPICO':  BEGIN & DTSET=PSC_DATASET & TPSET=PSC_TEMP & SPROD=SPROD+'-'+PSC_ALG & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='NUM_0.0_1.0'    & PAL='PAL_DEFAULT'    & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='NUM_0_0.8' & APAL='PAL_BLUEGREEN_ORANGE' & END
      'PSC_MICRO':            BEGIN & DTSET=PSC_DATASET & TPSET=PSC_TEMP & SPROD=SPROD+'-'+PSC_ALG & TPROD=SPROD                  & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.03_3'  & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='CHLOR_A_0.01_10' & APAL='PAL_BLUEGREEN_ORANGE' & END
      'PSC_NANO':             BEGIN & DTSET=PSC_DATASET & TPSET=PSC_TEMP & SPROD=SPROD+'-'+PSC_ALG & TPROD=SPROD                  & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.03_3'  & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='CHLOR_A_0.01_10' & APAL='PAL_BLUEGREEN_ORANGE' & END
      'PSC_PICO':             BEGIN & DTSET=PSC_DATASET & TPSET=PSC_TEMP & SPROD=SPROD+'-'+PSC_ALG & TPROD=SPROD                  & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.03_3'  & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='CHLOR_A_0.01_10' & APAL='PAL_BLUEGREEN_ORANGE' & END

    ENDCASE ; SPROD
    STR = CREATE_STRUCT('DATASET',DTSET,'TEMP_DATASET',TPSET,'PROD',SPROD,'DOWNLOAD_PROD',DWLPROD,'PLOT_TAG',PTAG,'PROD_SCALE',PSCALE,'PAL',PAL,'ANOM_SCALE',ASCALE,'ANOM_PAL',APAL,'ANOM_TITLE',ATITLE)
    IF MONTH_SCALE NE [] THEN STR = CREATE_STRUCT(STR,'MONTH_SCALE',MONTH_SCALE)
    PSTR = CREATE_STRUCT(PSTR,PRODS[P],STR)
  ENDFOR ; PRODS
  STR = CREATE_STRUCT('VERSION',VER,'INFO',ISTR,'DIRS',DSTR,'PROD_INFO',PSTR);,'SHAPEFILES',HSTR)
  IF N_ELEMENTS(VERSION) EQ 1 THEN RETURN, STR
  VSTR = CREATE_STRUCT(VSTR,VER,STR)
ENDFOR ; VERSION


END ; ***************** End of GOM_CHL_2023_VERSION *****************
