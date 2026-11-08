!$Procedure      F_ZZUTC ( Family of tests for ZZUTCPM )

      SUBROUTINE F_ZZUTC ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine is a family of tests for the routine ZZUTCPM.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Variables
!
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



!
!     Begin every test family with an open call.
!
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
