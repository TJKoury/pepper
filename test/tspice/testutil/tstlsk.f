C$Procedure      TSTLSK ( Test Leapseconds Kernel )
 
      SUBROUTINE TSTLSK
 
C$ Abstract
C
C    This test utility routine creates a leapsecond kernel
C    (valid as of July 1, 1997) loads the file into the
C    kernel pool, and then deletes the resulting file.
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
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST UTILITIES
C
C$ Declarations
 
      IMPLICIT NONE
 
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     A leapseconds kernel is created, loaded into the kernel pool
C     and then deleted.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine creates a temporary file based on the file
C     naming pattern (see NEWFIL_1 in support for details)
C
C         lsk{0-9}{0-9}{0-9}{0-9}.tmp
C
C     The resulting file is loaded into the kernel pool and
C     then deleted.
C
C     The fact that this file is created is logged in the
C     test log file.
C
C$ Examples
C
C     Suppose that you are testing some portion of the toolkit
C     that requires the use of a leapseconds kernel.  This routine
C     allows you to load a leapseconds kernel without having to
C     know where a current leapsecond kernel is located on the
C     file system.
C
C        CALL TSTLSK
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 1-JUL-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Generate and load a leapseconds kernel for testing.
C
C-&
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               NLINES
      PARAMETER           ( NLINES = 29 )
 
 
      CHARACTER*(LNSIZE)    FILE
      CHARACTER*(LNSIZE)    TEXT ( NLINES )
      CHARACTER*(WDSIZE)    BEGDAT
 
      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' '
      TEXT(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      TEXT(  4 ) = 'DELTET/K               =    1.657D-3'
      TEXT(  5 ) = 'DELTET/EB              =    1.671D-2'
      TEXT(  6 ) = 'DELTET/M               = (  6.239996D0   '
     .//           '1.99096871D-7 )'
      TEXT(  7 ) = ' '
      TEXT(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-JA'
     .//           'N-1'
      TEXT(  9 ) = '                           11,   @1972-JU'
     .//           'L-1'
      TEXT( 10 ) = '                           12,   @1973-JA'
     .//           'N-1'
      TEXT( 11 ) = '                           13,   @1974-JA'
     .//           'N-1'
      TEXT( 12 ) = '                           14,   @1975-JA'
     .//           'N-1'
      TEXT( 13 ) = '                           15,   @1976-JA'
     .//           'N-1'
      TEXT( 14 ) = '                           16,   @1977-JA'
     .//           'N-1'
      TEXT( 15 ) = '                           17,   @1978-JA'
     .//           'N-1'
      TEXT( 16 ) = '                           18,   @1979-JA'
     .//           'N-1'
      TEXT( 17 ) = '                           19,   @1980-JA'
     .//           'N-1'
      TEXT( 18 ) = '                           20,   @1981-JU'
     .//           'L-1'
      TEXT( 19 ) = '                           21,   @1982-JU'
     .//           'L-1'
      TEXT( 20 ) = '                           22,   @1983-JU'
     .//           'L-1'
      TEXT( 21 ) = '                           23,   @1985-JU'
     .//           'L-1'
      TEXT( 22 ) = '                           24,   @1988-JA'
     .//           'N-1'
      TEXT( 23 ) = '                           25,   @1990-JA'
     .//           'N-1'
      TEXT( 24 ) = '                           26,   @1991-JA'
     .//           'N-1'
      TEXT( 25 ) = '                           27,   @1992-JU'
     .//           'L-1'
      TEXT( 26 ) = '                           28,   @1993-JU'
     .//           'L-1'
      TEXT( 27 ) = '                           29,   @1994-JU'
     .//           'L-1'
      TEXT( 28 ) = '                           30,   @1996-JA'
     .//           'N-1'
      TEXT( 29 ) = '                           31,   @1997-JU'
     .//           'L-1 )'
 
 
      CALL NEWFIL_1 ( 'lsk{0-9}{0-9}{0-9}{0-9}.tmp', FILE )
      CALL TSTTXT   ( FILE, TEXT, NLINES, .TRUE., .FALSE. )
 
      RETURN
      END
