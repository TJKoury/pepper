C$Procedure F_XLATEI ( ZZXLATEI Test Family )
 
      SUBROUTINE F_XLATEI ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZXLATEI
C     routine.
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
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      LOGICAL               OK
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exercises ZZXLATEI's logic.
C
C$ Examples
C
C     None.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.26.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    TSPICE Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    TSPICE Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    TSPICE Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    TSPICE Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    TSPICE Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    TSPICE Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    TSPICE Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    TSPICE Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    TSPICE Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    TSPICE Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    TSPICE Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    TSPICE Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    TSPICE Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    TSPICE Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    TSPICE Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    TSPICE Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    TSPICE Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    TSPICE Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    TSPICE Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    TSPICE Version 1.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               INTMIN
      INTEGER               INTMAX
 
C
C     Local Parameters
C
      INTEGER               NUMOUT
      PARAMETER           ( NUMOUT = 256 )
 
C
C     Local Variables
C
      CHARACTER*(1024)      INPUT
 
      INTEGER               COMPAR ( NUMOUT )
      INTEGER               I
      INTEGER               J
      INTEGER               INBFF
      INTEGER               OUTPUT ( NUMOUT )
      INTEGER               SPACE
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_XLATEI' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INBFF out of range error' )
 
C
C     Setup the inputs and outputs for checking the lower bound.
C
      INBFF = 0
      SPACE = 10
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
C
C     Invoke the module.
C
      CALL ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )
 
C
C     Check to see that OUTPUT was unmodified.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C     Setup the inputs and outputs for checking the upper bound.
C
      INBFF = NUMBFF + 1
      SPACE = 10
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
C
C     Invoke the module.
C
      CALL ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )
 
C
C     Check to see that OUTPUT was unmodified.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LTL-IEEE -- Bad Byte Count BIG-IEEE INPUT' )
 
C
C     Setup the inputs and outputs.
C
      INBFF = BIGI3E
 
      SPACE = 10
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
C
C     Invoke the module. Restrict INPUT to 1:13 range, since
C     BIG-IEEE integers come in 4-byte packages.
C
      CALL ZZXLATEI ( INBFF, INPUT(1:13), SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )
 
C
C     Check to see that OUTPUT was unmodified.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LTL-IEEE -- Not enough SPACE to store OUTPUT' )
 
C
C     Setup the inputs and outputs.
C
      INBFF = BIGI3E
 
      SPACE = 10
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
C
C     Invoke the module. Restrict INPUT to 1:13 range, since
C     BIG-IEEE integers come in 4-byte packages.
C
      CALL ZZXLATEI ( INBFF, INPUT(1:80), SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )
 
C
C     Check to see that OUTPUT was unmodified.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LTL-IEEE -- Unsupported INBFF' )
 
C
C     Setup the inputs and outputs.
C
      INBFF = VAXDFL
      SPACE = 10
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
C
C     Invoke the module. Restrict INPUT to 1:13 range, since
C     BIG-IEEE integers come in 4-byte packages.
C
      CALL ZZXLATEI ( INBFF, INPUT(1:40), SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(BUG)', OK )
 
C
C     Check to see that OUTPUT was unmodified.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LTL-IEEE -- BIG-IEEE Extreme Integer Values' )
 
C
C     Setup the inputs and outputs.
C
      INBFF = BIGI3E
 
      SPACE = 2
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
      COMPAR(1) = INTMAX()
      COMPAR(2) = INTMIN()
 
C
C     Get the smallest possible integer bit pattern in memory,
C     even if INTMIN does not cooperate.
C
      IF ( COMPAR(2) .EQ. -2147483647 ) THEN
         COMPAR(2) = COMPAR(2) - 1
      END IF
 
C
C     Prepare the INPUT buffer.
C
      CALL T_XLTFWI ( COMPAR, 2, INBFF, INPUT(1:8) )
 
C
C     Invoke the module.
C
      CALL ZZXLATEI ( INBFF, INPUT(1:8), SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check to see that OUTPUT is reasonable.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LTL-IEEE -- BIG-IEEE Integral Sequence' )
 
C
C     Setup the inputs and outputs.
C
      INBFF = BIGI3E
 
      SPACE = NUMOUT
 
      DO I = 1, NUMOUT
         OUTPUT(I) = 0
         COMPAR(I) = 0
      END DO
 
      DO I = 1, 31
         COMPAR(I) = 2**(I-1)
      END DO
 
      DO I = 32, 62
         COMPAR(I) = -2**(I-32)
      END DO
 
      DO I = 62, NUMOUT
         COMPAR(I) = 1000*I
      END DO
 
C
C     Prepare the INPUT buffer.
C
      CALL T_XLTFWI ( COMPAR, NUMOUT, INBFF, INPUT )
 
C
C     Invoke the module.
C
      CALL ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )
 
C
C     Check for an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check to see that OUTPUT is as expected.
C
      CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, NUMOUT, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LTL-IEEE -- BIG-IEEE Byte Value Cycle' )
 
C
C     Setup the inputs and outputs for the LSB byte case.
C
      INBFF = BIGI3E
 
      SPACE = 256
 
      DO J = 1, 4
 
         DO I = 1, SPACE
            OUTPUT(I) = 0
            COMPAR(I) = 0
         END DO
 
         DO I = 1, SPACE
            COMPAR(I) = ISHFT ( (I-1), 8*(J-1) )
         END DO
 
C
C        Prepare the INPUT buffer.
C
         CALL T_XLTFWI ( COMPAR, SPACE, INBFF, INPUT(1:4*SPACE) )
 
C
C        Invoke the module.
C
         CALL ZZXLATEI ( INBFF, INPUT(1:4*SPACE), SPACE, OUTPUT )
 
C
C        Check for an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Check to see that OUTPUT was unmodified.
C
         CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', COMPAR, SPACE, OK )
 
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
