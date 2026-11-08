C$Procedure      F_ZZUTC ( Family of tests for ZZUTCPM )
 
      SUBROUTINE F_ZZUTC ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine is a family of tests for the routine ZZUTCPM.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
 
      CHARACTER*(LNSIZE)    STRING
 
      DOUBLE PRECISION      EXPHOF
      DOUBLE PRECISION      EXPMOF
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
 
      INTEGER               ELAST
      INTEGER               LAST
      INTEGER               START
      LOGICAL               ESUCC
      LOGICAL               SUCCES
 
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZUTC.' )
 
      CALL TCASE ( 'Positive Hours only.' )
 
         STRING = 'Some ::UTC+10 hours'
         START  =  6
         EXPHOF =  10.0D0
         EXPMOF =   0.0D0
         ELAST  =   13
         ESUCC  = .TRUE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 6,      0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
 
 
      CALL TCASE ( 'Negative Hours only.' )
 
         STRING = 'Some ::UTC-8 hours'
         START  =  6
         EXPHOF =  -8.0D0
         EXPMOF =   0.0D0
         ELAST  =   12
         ESUCC  = .TRUE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 6,      0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
      CALL TCASE ( 'Postive Hours and Minutes.' )
 
         STRING = 'An offset of ::UTC+11:17 hours and minutes'
 
         START  =  14
         EXPHOF =  11.0D0
         EXPMOF =  17.0D0
         ELAST  =  24
         ESUCC  = .TRUE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 14,     0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
      CALL TCASE ( 'Negative Hours and Minutes.' )
 
         STRING = 'An offset of ::UTC-05:33 hours and minutes'
 
         START  =  14
         EXPHOF =  -5.0D0
         EXPMOF = -33.0D0
         ELAST  =  24
         ESUCC  = .TRUE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 14,     0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
      CALL TCASE ( 'Out of range Hours.' )
 
         STRING = 'An offset of ::UTC-15:33 hours and minutes'
 
         START  =  14
         EXPHOF =  0.0D0
         EXPMOF =  0.0D0
         ELAST  =  13
         ESUCC  = .FALSE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 14,     0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
      CALL TCASE ( 'Out of range Minutes.' )
 
         STRING = 'An offset of ::UTC+05:63 hours and minutes'
 
         START  =  14
         EXPHOF =  5.0D0
         EXPMOF =  0.0D0
         ELAST  =  21
         ESUCC  = .TRUE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 14,     0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
      CALL TCASE ( 'Unparsable Hours.' )
 
         STRING = 'An offset of ::UTC+ONE:33 hours and minutes'
 
         START  =  14
         EXPHOF =  0.0D0
         EXPMOF =  0.0D0
         ELAST  =  13
         ESUCC  = .FALSE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 14,     0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
      CALL TCASE ( 'Unparsable Minutes.' )
 
         STRING = 'An offset of ::UTC+01:TWELVE hours and minutes'
 
         START  =  14
         EXPHOF =  1.0D0
         EXPMOF =  0.0D0
         ELAST  =  21
         ESUCC  = .TRUE.
 
         CALL ZZUTCPM ( STRING, START, HOFF, MOFF, LAST, SUCCES )
 
         CALL CHCKSI  ( 'LAST',   LAST,   '=', ELAST,  0,     OK )
         CALL CHCKSI  ( 'START',  START,  '=', 14,     0,     OK )
         CALL CHCKSD  ( 'MOFF',   MOFF,   '=', EXPMOF, 0.0D0, OK )
         CALL CHCKSD  ( 'HOFF',   HOFF,   '=', EXPHOF, 0.0D0, OK )
         CALL CHCKSL  ( 'SUCCES', SUCCES,      ESUCC,         OK )
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
