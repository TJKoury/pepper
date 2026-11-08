C$Procedure TSTLSK (Create a test LSK)
 
      SUBROUTINE ZZTSTLSK( NAMELS )
 
C$ Abstract
C
C    This test utility routine creates a leapsecond kernel
C    (valid as of July 1, 1997).
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
C       TESTING
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         NAMELS

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
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A leapseconds kernel is created.
C
C$ Examples
C
C     Suppose that you are testing some portion of the toolkit
C     that requires the use of a leapseconds kernel.  This routine
C     allows you to create a leapseconds kernel without having to
C     know where a current leapsecond kernel is located on the
C     file system.
C
C        CALL ZZTSTLSK( NAMELS )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    Test Utilities 1.0.0, 18-AUG-2008 (EDW)
C
C        Replaced WRITE with WRITLN. 
C
C-    Test Utilities 1.0.0, 05-JAN-2006 (EDW)
C
C        Reimplementation of TSTLSK removing all code 
C        except that needed to write the test LSK.
C
C-&
 
C$ Index_Entries
C
C     Generate a leapseconds kernel for testing.
C
C-&
 
C
C     Spicelib Functions
C
      INTEGER               RTRIM

C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGDAT
  
C
C     Local Variables.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               NLINES
      PARAMETER           ( NLINES = 29 )
 
 
      CHARACTER*(LNSIZE)    LSK ( NLINES )
      CHARACTER*(LNSIZE)    LSKLN

      INTEGER               I
      INTEGER               R
      INTEGER               UNIT
 
      CALL KILFIL ( NAMELS ) 
 
      LSK(  1 ) = BEGDAT()
      LSK(  2 ) = ' '
      LSK(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      LSK(  4 ) = 'DELTET/K               =    1.657D-3'
      LSK(  5 ) = 'DELTET/EB              =    1.671D-2'
      LSK(  6 ) = 'DELTET/M               = (  6.239996D0   '
     .//           '1.99096871D-7 )'
      LSK(  7 ) = ' '
      LSK(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-JA'
     .//           'N-1'
      LSK(  9 ) = '                           11,   @1972-JU'
     .//           'L-1'
      LSK( 10 ) = '                           12,   @1973-JA'
     .//           'N-1'
      LSK( 11 ) = '                           13,   @1974-JA'
     .//           'N-1'
      LSK( 12 ) = '                           14,   @1975-JA'
     .//           'N-1'
      LSK( 13 ) = '                           15,   @1976-JA'
     .//           'N-1'
      LSK( 14 ) = '                           16,   @1977-JA'
     .//           'N-1'
      LSK( 15 ) = '                           17,   @1978-JA'
     .//           'N-1'
      LSK( 16 ) = '                           18,   @1979-JA'
     .//           'N-1'
      LSK( 17 ) = '                           19,   @1980-JA'
     .//           'N-1'
      LSK( 18 ) = '                           20,   @1981-JU'
     .//           'L-1'
      LSK( 19 ) = '                           21,   @1982-JU'
     .//           'L-1'
      LSK( 20 ) = '                           22,   @1983-JU'
     .//           'L-1'
      LSK( 21 ) = '                           23,   @1985-JU'
     .//           'L-1'
      LSK( 22 ) = '                           24,   @1988-JA'
     .//           'N-1'
      LSK( 23 ) = '                           25,   @1990-JA'
     .//           'N-1'
      LSK( 24 ) = '                           26,   @1991-JA'
     .//           'N-1'
      LSK( 25 ) = '                           27,   @1992-JU'
     .//           'L-1'
      LSK( 26 ) = '                           28,   @1993-JU'
     .//           'L-1'
      LSK( 27 ) = '                           29,   @1994-JU'
     .//           'L-1'
      LSK( 28 ) = '                           30,   @1996-JA'
     .//           'N-1'
      LSK( 29 ) = '                           31,   @1997-JU'
     .//           'L-1 )'
 
 
C
C     Create the LSK kernel.
C
      CALL TXTOPN ( NAMELS, UNIT )
 
      DO I = 1, NLINES
         LSKLN = LSK(I)
         R = RTRIM( LSKLN )
         CALL WRITLN ( LSKLN(1:R), UNIT )
      END DO

      CLOSE ( UNIT )
 
      RETURN
      END
