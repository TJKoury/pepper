C$Procedure  F_MOVED ( Family of tests for MOVED )
 
      SUBROUTINE F_MOVED ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine tests MOVED.
C
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
C     TBD.
C
C$ Keywords
C
C     TBD.
C
C$ Declarations
C
C     TBD.
C
C$ Brief_I/O
C
C     TBD.
C
C$ Detailed_Input
C
C     TBD.
C
C$ Detailed_Output
C
C     TBD.
C
C$ Parameters
C
C     TBD.
C
C$ Exceptions
C
C     TBD.
C
C$ Files
C
C     TBD.
C
C$ Particulars
C
C     This routine has already been used to detect a bug (NDIM = -1
C     resulted in a segmentation fault for the PC-LINUX and NeXT
C     platforms, on which memmove is used to effect the copy).
C
C     So don't laugh.
C
C$ Examples
C
C     TBD.
C
C$ Restrictions
C
C     TBD.
C
C$ Literature_References
C
C     TBD.
C
C$ Author_and_Institution
C
C     TBD.
C
C$ Version
C
C-     Version 1.26.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-     Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-     Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-     Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-     Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-     Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-     Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-     Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-     Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-     Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-     Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-     Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-     Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-     Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-     Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-     Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-     Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-     Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-     Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-     Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-     Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-     Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-     Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-     Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-     Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-     Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    Version 1.0.1 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    Version 1.0.0 12-NOV-2001 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
 
      INTEGER               INTMAX
 
C
C     Local parameters
C
      INTEGER               NITEMS
      PARAMETER           ( NITEMS = 17 )
 
      INTEGER               E1
      PARAMETER           ( E1     =  10 )
 
      INTEGER               E2
      PARAMETER           ( E2     = 100 )
 
      INTEGER               E3
      PARAMETER           ( E3     = 300 )
 
C
C     Local variables
C
      DOUBLE PRECISION      BUFF   ( NITEMS )
      DOUBLE PRECISION      BUFF2  ( NITEMS )
      DOUBLE PRECISION      XBUFF  ( NITEMS )
 
      INTEGER               H
      INTEGER               I
      INTEGER               IC     ( 2 )
      INTEGER               IC2    ( 2 )
      INTEGER               J
      INTEGER               K
      INTEGER               MAXP
      INTEGER               XIC    ( 2 )
 
C
C     EQUIVALENCE statements
C
      EQUIVALENCE           ( BUFF (1), IC  )
      EQUIVALENCE           ( BUFF2(1), IC2 )
 
C
C     Initial values
C
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_MOVED' )
 
C
C     Case 1.
C
      CALL TCASE ( 'Test MOVED for a selection of standard d.p. '//
     .             'numbers.' )
 
 
      BUFF(1)   =    1.D0
      BUFF(2)   =   -1.D0
      BUFF(3)   =    0.D0
      BUFF(4)   =    10.D0**( E1)
      BUFF(5)   =    10.D0**(-E1)
      BUFF(6)   = -( 10.D0**( E1) )
      BUFF(7)   = -( 10.D0**(-E1) )
      BUFF(8)   =    10.D0**( E2)
      BUFF(9)   =    10.D0**(-E2)
      BUFF(10)  = -( 10.D0**( E2) )
      BUFF(11)  = -( 10.D0**( E2) )
      BUFF(12)  =    10.D0**( E3)
      BUFF(13)  =    10.D0**(-E3)
      BUFF(14)  = -( 10.D0**( E3) )
      BUFF(15)  = -( 10.D0**(-E3) )
      BUFF(16)  =    DPMAX()
      BUFF(17)  =    DPMIN()
 
      XBUFF(1)  =    1.D0
      XBUFF(2)  =   -1.D0
      XBUFF(3)  =    0.D0
      XBUFF(4)  =    10.D0**( E1)
      XBUFF(5)  =    10.D0**(-E1)
      XBUFF(6)  = -( 10.D0**( E1) )
      XBUFF(7)  = -( 10.D0**(-E1) )
      XBUFF(8)  =    10.D0**( E2)
      XBUFF(9)  =    10.D0**(-E2)
      XBUFF(10) = -( 10.D0**( E2) )
      XBUFF(11) = -( 10.D0**( E2) )
      XBUFF(12) =    10.D0**( E3)
      XBUFF(13) =    10.D0**(-E3)
      XBUFF(14) = -( 10.D0**( E3) )
      XBUFF(15) = -( 10.D0**(-E3) )
      XBUFF(16) =    DPMAX()
      XBUFF(17) =    DPMIN()
 
 
      CALL MOVED  ( BUFF, NITEMS, BUFF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'BUFF2', BUFF2, '=', XBUFF, NITEMS, 0.D0, OK )
 
C
C     Case 2.
C
      CALL TCASE ( 'Test MOVED for a case known to be problematic.' )
 
      IC(1)  = -771000
      IC(2)  = -771900
 
      XIC(1) = -771000
      XIC(2) = -771900
 
      CALL MOVED  ( BUFF, 1, BUFF2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAI ( 'IC2', IC2, '=', XIC, 2, OK )
 
C
C     Case 3.
C
      CALL TCASE ( 'Test MOVED for a selection of bit patterns ' //
     .             'generated by equivalencing 2-integer arrays '//
     .             'with dp. numbers.'                            )
 
      MAXP = INT (  LOG10( DBLE(INTMAX()) )  )
 
      DO I = -1, MAXP + 1
C
C        Assign the first element of the IC array.
C
         IF ( I .EQ. -1 ) THEN
 
            IC(1) = 0
 
         ELSE IF ( I .EQ. MAXP+1 ) THEN
 
            IC(1) = INTMAX()
 
         ELSE
 
            IC(1) = 10**I
 
         END IF
 
 
         DO J = -1, MAXP + 1
C
C           Assign the second element of the IC array.
C
            IF ( J .EQ. -1 ) THEN
 
               IC(2) = 0
 
            ELSE IF ( J .EQ. MAXP+1 ) THEN
 
               IC(2) = INTMAX()
 
            ELSE
 
               IC(2) = 10**J
 
            END IF
 
C
C           Try every sign combination.
C
            DO K = 1, 2
 
               IC(1) = IC(1) * (-1)**K
 
               DO H = 1, 2
 
                  IC(2) = IC(2) * (-1)**H
C
C                 Test MOVED on the current artificially constructed
C                 d.p. variable.
C
                  CALL MOVEI  ( IC,   2, XIC   )
                  CALL MOVED  ( BUFF, 1, BUFF2 )
 
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
                  CALL CHCKAI ( 'IC2', IC2, '=', XIC, 2, OK )
 
               END DO
 
            END DO
C
C           We're done testing the current pair of integers for each
C           sign combination.
C
         END DO
 
      END DO
 
C
C     Case 4.
C
      CALL TCASE ( 'Test MOVED for NDIM = 0 and -1.'  )
 
      CALL FILLD  ( 1.0D0,    NITEMS, BUFF )
 
      CALL CLEARD ( NITEMS, BUFF2 )
      CALL CLEARD ( NITEMS, XBUFF )
 
      CALL MOVED  ( BUFF,   0,      BUFF2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'BUFF2', BUFF2, '=', XBUFF, NITEMS, 0.D0, OK )
 
      CALL MOVED  ( BUFF,  -1,      BUFF2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKAD ( 'BUFF2', BUFF2, '=', XBUFF, NITEMS, 0.D0, OK )
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
