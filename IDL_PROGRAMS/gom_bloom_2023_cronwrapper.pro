; $ID:	GOM_BLOOM_2023_CRONWRAPPER.PRO,	2022-08-08-11,	USER-KJWH	$
  PRO GOM_BLOOM_2023_CRONWRAPPER

;+
; NAME:
;   GOM_BLOOM_2023_CRONWRAPPER
;
; PURPOSE:
;   Program to be called by the cronjob
;
; PROJECT:
;   GOM_BLOOM_2023
;
; CALLING SEQUENCE:
;   GOM_BLOOM_2023_CRONWRAPPER
;
; REQUIRED INPUTS:
;   None 
;
; OPTIONAL INPUTS:
;   TBD
;
; KEYWORD PARAMETERS:
;   TBD
;
; OUTPUTS:
;   Runs GOM_BLOOM_2023_CRONWRAPPER
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
; Copyright (C) 2022, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on Seotember 01, 2022 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Sep 01, 2023 - KJWH: Initial code written, adapted from ILLEX_VIEWER_CRONWRAPPER
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'GOM_BLOOM_2023_CRONWRAPPER'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  DT = DATE_PARSE(DATE_NOW())
   
  LOGDIR = !S.GOM_BLOOM_2023 + SL + 'CRONLOGS' + SL & DIR_TEST, LOGDIR
  LOGFILE = LOGDIR + ROUTINE_NAME + '-' + DATE_NOW() + '.log'
  OPENW, LUN, LOGFILE, /APPEND, /GET_LUN, WIDTH=180 ;  ===> Open log file
  PLUN, LUN, '******************************************************************************************************************'
  PLUN, LUN, 'Starting ' + ROUTINE_NAME + ' log file: ' + LOGFILE + ' on: ' + systime() + ' on ' + !S.COMPUTER, 0
  
  PLUN, LUN, 'Running GOM_BLOOM_2023_MAIN...'
  GOM_BLOOM_2023_MAIN, /LOGFILE, /ANIMATIONS, /MAKE_COMPOSITES, /GIT_PUSH, LOGLUN=LUN, _EXTRA=EXTRA

  FLUSH, LUN & CLOSE, LUN & FREE_LUN, LUN

END ; ***************** End of GOM_BLOOM_2023_CRONWRAPPER *****************
