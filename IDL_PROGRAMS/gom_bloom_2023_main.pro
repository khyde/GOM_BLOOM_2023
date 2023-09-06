; $ID:	GOM_CHL_2023_MAIN.PRO,	2023-07-28-17,	USER-KJWH	$
  PRO GOM_BLOOM_2023_MAIN, VERSION, LOGFILE=LOGFILE, BUFFER=BUFFER, OVEWRITE=OVERWRITE, _EXTRA=EXTRA

;+
; NAME:
;   GOM_BLOOM_2023_MAIN
;
; PURPOSE:
;   The MAIN program for processing files for the GOM 2023 bloom project
;
; PROJECT:
;   GOM_BLOOM_2023_MAIN
;
; CALLING SEQUENCE:
;   GOM_BLOOM_2023_MAIN
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   VERSION
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
  ROUTINE_NAME = 'GOM_BLOOM_2023_MAIN'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  DIR_PROJECT = !S.GOM_BLOOM_2023

  IF ~N_ELEMENTS(VERSION)    THEN VERSION = 'V2023'
  IF ~N_ELEMENTS(BUFFER)     THEN BUFFER  = 1
  IF ~N_ELEMENTS(VERBOSE)    THEN VERBOSE = 0
  IF ~N_ELEMENTS(RESOLUTION) THEN RESOLUTION = 300
  IF ~N_ELEMENTS(LOGLUN)    THEN LUN   = [] ELSE LUN = LOGLUN

  ; ===> Manually adjust the ILLEX program steps as needed
  IF HAS(EXTRA,'DOWNLOAD_FILES' )      THEN DOWNLOAD_FILES = 1 ELSE DOWNLOAD_FILES = 0
  IF HAS(EXTRA,'ANIMATIONS' )      THEN ANIMATIONS = 1 ELSE ANIMATIONS = 0
  IF HAS(EXTRA,'MAKE_COMPOSITES' )      THEN MAKE_COMPOSITES = 1 ELSE MAKE_COMPOSITES = 0
  IF HAS(EXTRA,'PRESS_RELEASE_FIGS' )      THEN PRESS_RELEASE_FIGS = 1 ELSE PRESS_RELEASE_FIGS = 0
  IF HAS(EXTRA,'GIT_PUSH' )      THEN GIT_PUSH = 1 ELSE GIT_PUSH = 0
  

  ; ===> Loop through versions
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    VERSTR = PROJECT_VERSION_DEFAULT('GOM_BLOOM_2023')
    DR = VERSTR.INFO.DATERANGE

    IF KEYWORD_SET(LOGFILE) AND LUN EQ [] THEN BEGIN
      LOGDIR = VERSTR.DIRS.DIR_LOGS ;DIR_PROJECT + 'OUTPUT' + SL + 'LOGS' + SL & DIR_TEST, LOGDIR
      IF IDLTYPE(LOGFILE) NE 'STRING' THEN LOGFILE =  LOGDIR + ROUTINE_NAME + '-' + DATE_NOW() + '.log'
      OPENW, LUN, LOGFILE, /APPEND, /GET_LUN, WIDTH=180 ;  ===> Open log file
    ENDIF ELSE BEGIN
      LUN = []
      LOGFILE = ''
    ENDELSE
    PLUN, LUN, '******************************************************************************************************************'
    PLUN, LUN, 'Starting ' + ROUTINE_NAME + ' log file: ' + LOGFILE + ' on: ' + systime() + ' on ' + !S.COMPUTER, 0

    IF KEYWORD_SET(DOWNLOAD_FILES) THEN GOM_CHL_2023_DOWNLOADS, VERSTR

    IF KEYWORD_SET(PROCESS_FILES) THEN BEGIN 
      PRODS=VERSTR.INFO.PROCESS_PRODS

      DSETS = []
      FOR N=0, N_ELEMENTS(PRODS)-1 DO BEGIN
        PROD = PRODS[N]
        CASE PROD OF
          'PSC':  PSTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ 'PSC_MICRO'))
          'PPD': PSTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ 'CHLOR_A'))
          ELSE: PSTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ PRODS[N]))
        ENDCASE
        DSET = PSTR.TEMP_DATASET        
        
        CASE PROD OF
          'CHLOR_A': FILES_2STACKED_WRAPPER, DSET, PRODS=PSTR.PROD,DATERANGE=DR
          'SST': FILES_2STACKED_WRAPPER, DSET, PRODS=PSTR.PROD,DATERANGE=DR
          'PSC': STACKED_MAKE_PRODS_WRAPPER, DSET, DO_PSC=1, DO_STATS=1, DO_ANOMS=1
          'PPD':  STACKED_MAKE_PRODS_WRAPPER, DSET, DO_PPD=1, DO_INTERP=1, DO_STATS=1, DO_ANOMS=1
        ENDCASE
        IF PROD EQ VALIDS('PRODS',PSTR.PROD) THEN STACKED_STATS_WRAPPER, DSET, PRODS=PSTR.PROD, PERIODS=VERSTR.INFO.STAT_PERIODS
        IF PROD EQ VALIDS('PRODS',PSTR.PROD) THEN STACKED_ANOMS_WRAPPER, DSET, PRODS=PSTR.PROD, PERIODS=VERSTR.INFO.ANOM_PERIODS
        ;     STACKED_2NETCDF_WRAPPER, DSET,PRODS=PSTR.PROD,PERIODS=VERSTR.INFO.NETCDF_PERIODS, DATERANGE=DR
      ENDFOR ; PRODS
    ENDIF
    
    
    IF KEYWORD_SET(MAKE_COMPOSITES) THEN $
      PROJECT_MAKE_COMPOSITE, VERSTR, PRODS='CHLOR_A',/WEEKS,/ADD_LONLAT,LONS=[-73,-70,-67],LATS=[40,42,44],/ADD_COLORBAR,/ADD_BATHY,/ADD_BOARDER,OUT_COLOR=0,BUFFER=1

    IF KEYWORD_SET(ANIMATIONS) THEN $
      PROJECT_COMPOSITE_ANIMATION, VERSTR, /WEEKS, EXTENSIONS=['webm'],BUFFER=BUFFER, FRAMES=2, /ADD_COLORBAR, /ADD_BATHY, /ADD_LONLAT, LONS=[-73,-70,-67],LATS=[40,42,44],/ADD_BOARDER;, OUTCOLOR=0


    IF KEYWORD_SET(PRESS_RELEASE_FIGS) THEN BEGIN

      ; ===> SET UP PLOT SPACING
      DSET = 'GLOBCOLOUR'
      YEARS = ['2022','2023'] & DTR = GET_DATERANGE(['2022','2023'])
      PRODS = ['CHLOR_A-GSM'] & PR = PRODS_READ('CHLOR_A')
      TYPES = ['STACKED_ANOMS']  ; ,'STACKED_STATS'
      PERIOD = 'M'
      MTHS = MONTH_RANGE(/STRING)
      NFILES = N_ELEMENTS(YEARS)*N_ELEMENTS(TYPES)
  
      IF ~N_ELEMENTS(MAPP) THEN MP = VERSTR.INFO.MAP_OUT ELSE MP = MAPP
      MR = MAPS_READ(MP)
      MR_DIMS  = FLOAT(STRSPLIT(MR.IMG_DIMS,';',/EXTRACT))
      XX = MR_DIMS[0]/MR.PX & YY = MR_DIMS[1]/MR.PY
      IF ~N_ELEMENTS(RESIZE) THEN RESZ = 0.85 ELSE RESZ = RESIZE
      
      NPLOTS = N_ELEMENTS(YEARS)*N_ELEMENTS(TYPES)
  
      IF ~N_ELEMENTS(BUFFER) THEN BUFFER = 1 ; Do plotting in background
      IF ~N_ELEMENTS(SPACE)  THEN SPACE  = 1
      IF ~N_ELEMENTS(LEFT)   THEN LEFT   = 1
      IF ~N_ELEMENTS(RIGHT)  THEN RIGHT  = 1
      IF ~N_ELEMENTS(TOP)    THEN TOP    = 10
      IF ~N_ELEMENTS(BOTTOM) THEN IF ~KEYWORD_SET(NO_COLORBAR) THEN BOTTOM = 8 ELSE BOTTOM = SPACE * 3
      IF ~N_ELEMENTS(NCOLS)  THEN IF NPLOTS EQ 2 THEN NCOL=2 ELSE NCOL  = N_ELEMENTS(YEARS) ELSE NCOL = NCOLS
      IF ~N_ELEMENTS(NROWS)  THEN IF NPLOTS EQ 1 THEN NROW=1 ELSE NROW  = N_ELEMENTS(TYPES) ELSE NROW = NROWS
      IF ~N_ELEMENTS(XDIM)   THEN XDIM   = MR.PX/NFILES & IF XDIM LT 300 THEN XDIM = XDIM*2
      IF ~N_ELEMENTS(YDIM)   THEN YDIM   = MR.PY/NFILES & IF YDIM LT 300 THEN YDIM = YDIM*2
  
      XNSPACE = NCOL-1 & YNSPACE = NROW-1
      WIDTH   = LEFT   + NCOL*XDIM + XNSPACE*SPACE + RIGHT
      HEIGHT  = BOTTOM + NROW*YDIM + YNSPACE*SPACE + TOP
      
      FILES = []
      FOR T=0, N_ELEMENTS(TYPES)-1 DO BEGIN
        FILE = GET_FILES(DSET,PRODS=PRODS,PERIOD=PERIOD,FILE_TYPE=TYPES[T], DATERANGE=DTR)
        IF FILE EQ [] THEN FILE = ''
        FILES = [FILES,FILE]
      ENDFOR ; PRODS
     
      FOR M=0, N_ELEMENTS(MTHS)-1 DO BEGIN
        COUNTER = 0
        
     
        PNGFILE = VERSTR.DIRS.DIR_COMPOSITES + PERIOD + '_' + STRJOIN(YEARS+MTHS[M],'_') + '-' + MP + '-' + STRJOIN(PRODS,'_') + '-' + STRJOIN(TYPES,'_') + '-COMPOSITE' +'.PNG'
        PNGFILE = REPLACE(PNGFILE,'STACKED_','')
        IF ~FILE_MAKE(FILES,PNGFILE,OVERWRITE=OVERWRITE,VERBOSE=VERBOSE) THEN CONTINUE
        OK = WHERE(FILES NE '', COUNT_FILES)
        IF COUNT_FILES EQ 0 THEN CONTINUE
            
        FOR Y=0, N_ELEMENTS(YEARS)-1 DO BEGIN
          PER = PERIOD_2STRUCT(PERIOD + '_' + YEARS[Y] + MTHS[M])
          IF DATE_2JD(PER.DATE_START) GT DATE_NOW(/JD) THEN CONTINUE
          DR = [PER.DATE_START,PER.DATE_END]
      
          FOR T=0, N_ELEMENTS(TYPES)-1 DO BEGIN
            ATYPE = TYPES[T]
          
            TXT = MONTH_NAMES(PER.MONTH_START) + '  ' + PER.YEAR_START
            PSTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ PR.PROD))
            DSET = PSTR.DATASET
         ;   IF PR.IN_PROD NE PR.PROD THEN PROD_SCALE = PR.IN_PROD ELSE PROD_SCALE = [];
            PROD_SCALE = PSTR.PROD_SCALE
            IF ATYPE EQ 'ANOMS' THEN PROD_SCALE = PSTR.ANOM_SCALE
    
            IF NROW EQ 1 THEN BEGIN ; For a 2x1 composite
              C = COUNTER MOD NCOL           ; Number of columns is associated with the number of months so C represents the column number
              XPOS = LEFT + C*XDIM + C*SPACE  ; Determine the left side of the image
              IF T EQ 0 THEN YPOS = HEIGHT - TOP
              IPOS = [XPOS,YPOS-YDIM,XPOS+XDIM,YPOS]
              COUNTER = COUNTER + 1
            ENDIF ELSE BEGIN
              C = COUNTER MOD NCOL           ; Number of columns is associated with the number of months so C represents the column number
              XPOS = LEFT + C*XDIM + C*SPACE  ; Determine the left side of the image
              IF C EQ 0 THEN R = COUNTER/NCOL ELSE R = T ; When C = 0, start a new row
              IF T EQ 0 THEN YPOS = HEIGHT - TOP - R*YDIM - R*SPACE ELSE YPOS = YPOS ; Determine the top position of the image
              IPOS = [XPOS,YPOS-YDIM,XPOS+XDIM,YPOS]
              COUNTER = COUNTER + 1
            ENDELSE
    
            FILE = GET_FILES(DSET,PRODS=PSTR.PROD,PERIOD=PER.PERIOD_CODE,FILE_TYPE=ATYPE, DATERANGE=YEARS[Y])
            IF FILE EQ [] THEN CONTINUE
            IF N_ELEMENTS(FILE) GT 1 THEN MESSAGE,'ERROR: More that one file found for ' + PSTR.PROD + ' - ' + ATYPE
            FP = PARSE_IT(FILE,/ALL)
            DIR_PNG = VERSTR.DIRS.DIR_PNGS + PR.PROD + SL + ATYPE + SL & DIR_TEST, DIR_PNG
            IPNG = DIR_PNG + REPLACE(FP.NAME +'.PNG',[FP.PERIOD,FP.MAP],[PER.PERIOD,MP])
  
            IMG = PROJECT_MAKE_PNG(VERSTR, FILE=FILE, DATERANGE=DR,BUFFER=BUFFER, RESIZE=RESZ, MAPP=MP, PROD_SCALE=PROD_SCALE,$
              /ADD_LONLAT,LONS=[-73,-70,-67],LATS=[40,42,44],/ADD_COLORBAR,/ADD_BATHY,/ADD_BOARDER,OUT_COLOR=0, _EXTRA=EXTRA)
    
            IF IDLTYPE(IMG) NE 'OBJREF' THEN CONTINUE
            CPIM = IMG.COPYWINDOW()
            TX = TEXT(0.5,0.95,TXT,ALIGNMENT=0.50,FONT_SIZE=14)
            IF FILE_MAKE(FILE,IPNG,OVERWRITE=OVERWRITE) THEN IMG.SAVE, IPNG, RESOLUTION=RESOLUTION
            IMG.CLOSE
            IF W EQ [] THEN W = WINDOW(DIMENSIONS=[WIDTH,HEIGHT],BUFFER=BUFFER,TITLE='')
  
            W.WINDOW.SETCURRENT
            IMG = IMAGE(CPIM, /CURRENT, POSITION=IPOS, /DEVICE)
            IF T EQ NROW-1 THEN BEGIN
              CASE NROW OF
                1: BEGIN & XPOS=IPOS[2]-50 & YPOS=IPOS[1]+50 & FONT_SIZE=14 & END
                2: BEGIN & XPOS=IPOS[2]-25 & YPOS=IPOS[1]+25 & FONT_SIZE=10 & END
              ENDCASE
              YTXT = TEXT(XPOS,YPOS,ROUNDS(YEARS[Y]),/DEVICE,FONT_STYLE='BOLD',FONT_SIZE=FONT_SIZE,ALIGNMENT=1.0)
            ENDIF
          ENDFOR ; PRODS
        ENDFOR ; TYPES
        
        IF IDLTYPE(W) EQ 'OBJREF' THEN BEGIN
          CASE NROW OF
            1: BEGIN & TXTHT = HEIGHT-TOP*2 & FTSIZE=14 & END
            2: BEGIN & TXTHT = HEIGHT-TOP/2 & FTSIZE=10 & END
          ENDCASE
          TX = TEXT(WIDTH/2,TXTHT, MONTH_NAMES(MTHS[M]), /DEVICE, FONT_STYLE='BOLD', FONT_SIZE=FTSIZE, ALIGNMENT=0.5, VERTICAL_ALIGNMENT=1.0)
          W.SAVE, PNGFILE, RESOLUTION=RESOLUTION
          W.CLOSE
        ENDIF  
      ENDFOR ; PERIODS
    ENDIF ; PRESS_RELEASE_FIGS


    
    ENDFOR ; VERSION
    
    
    FLUSH, LUN & CLOSE, LUN & FREE_LUN, LUN
    IF KEYWORD_SET(GIT_PUSH) THEN BEGIN
      IF LOGFILE NE '' THEN OPENW,LUN,LOGFILE,/GET_LUN ELSE LUN=[]
      COUNTER = 0
      WHILE COUNTER LT 2 DO BEGIN
        COUNTER = COUNTER + 1

        ; ===> Change directory
        cd, DIR_PROJECT

        ; ===> Check the version control status
        CMD = "git status"
        SPAWN, CMD, LOG, EXIT_STATUS=ES
        PLUN, LUN, LOG
        IF ES EQ 1 THEN GOTO, GIT_ERROR
        IF ~HAS(LOG, 'nothing to commit') THEN BEGIN

          ; ===> Add the files to git
          CMD = "git add *"
          SPAWN, CMD, LOG, EXIT_STATUS=ES
          PLUN, LUN, LOG
          IF ES EQ 1 THEN GOTO, GIT_ERROR

          ; ===> Commit the files to git
          COMMIT_MSG = ' GOM Bloom 2023 update - ' + NUM2STR(DATE_NOW(/DATE_ONLY))
          CMD = "git commit -m '" + COMMIT_MSG + "'"
          SPAWN, CMD, LOG, EXIT_STATUS=ES
          PLUN, LUN, LOG
          IF ES EQ 1 THEN GOTO, GIT_ERROR
        ENDIF

        ; ===> Push the files to GitHub
        CMD = "git push"
        SPAWN, CMD, LOG, EXIT_STATUS=ES
        PLUN, LUN, LOG
        IF ES EQ 1 THEN GOTO, GIT_ERROR

        ; ===> Double check the Git Status
        CMD = "git status"
        SPAWN, CMD, LOG, EXIT_STATUS=ES
        IF LOG[1] EQ "Your branch is up to date with 'origin/main'." AND LOG[3] EQ "nothing to commit, working tree clean" THEN BREAK

      ENDWHILE

      GIT_ERROR:
      IF ES EQ 1 THEN BEGIN
        MESSAGE, 'ERROR: Unable to complete git steps and upload files'
      ENDIF ELSE BEGIN
        MAILTO = 'kimberly.hyde@noaa.gov'
        CMD = 'echo "GOM Bloom 2023 composites and animations uploaded to GitHub ' + SYSTIME() + '" | mailx -s "Illex composites ' + SYSTIME() + '" ' + MAILTO
        IF KEYWORD_SET(EMAIL) THEN SPAWN, CMD
      ENDELSE

    ENDIF

    PLUN,  LUN, 'Finished ' + ROUTINE_NAME + ': ' + ' on: ' + systime() + ' on ' + !S.COMPUTER, 2
    IF ANY(LUN) THEN BEGIN
      FLUSH, LUN & CLOSE, LUN & FREE_LUN, LUN
    ENDIF



END ; ***************** End of GOM_BLOOM_2023_MAIN *****************
