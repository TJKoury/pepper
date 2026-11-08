C$Procedure F_ZZPLAT ( Test Fortran Intrinsics )
 
      SUBROUTINE F_ZZPLAT ( OK )
 
C$ Abstract
C
C     Test family to verify that ZZPLATFM returns proper values.
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
C     This routine checks to make certain that values returned from
C     ZZPLATFM are correct.  Not all values are checked.
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
C-    TSPICE Version 1.1.0, 29-NOV-2021 (BVS)
C
C        Added TOUCH* calls for *EQUIV variables to make sure that
C        they don't get out-optimized by compilers like IFORT on
C        Linux.
C
C-    TSPICE Version 1.0.0, 18-DEC-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      TOUCHD

      INTEGER               TOUCHI

      LOGICAL               EQSTR
 
C
C     Local Variables
C
      CHARACTER*(8)         BFFID
 
      DOUBLE PRECISION      DEQUIV
 
      INTEGER               IEQUIV ( 2 )
 
C
C     EQUIVALENCE statements
C
      EQUIVALENCE ( DEQUIV, IEQUIV )
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_ZZPLAT' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Binary File Format Consistency Check' )
 
C
C     Fetch the binary file format ID string.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', BFFID )

C
C     "Touch" IEQUIV to make sure it does not get optimized out,
C     as is the case with IFORT on Linux.
C
      IEQUIV(1) = TOUCHI ( IEQUIV(1) ) 
      IEQUIV(2) = TOUCHI ( IEQUIV(2) )
 
C
C     Set the test value. "Touch" it as well.
C
      DEQUIV = 1.0D0

      DEQUIV = TOUCHD ( DEQUIV )
 
C
C     Branch based on the value returned; check equivalenced
C     integers for the appropriate values.
C
      IF ( EQSTR ( BFFID, 'BIG-IEEE' ) ) THEN
 
         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 1072693248, 0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 0,          0, OK )
 
      ELSE IF ( EQSTR ( BFFID, 'LTL-IEEE' ) ) THEN
 
         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 0,          0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 1072693248, 0, OK )
 
      ELSE IF ( EQSTR ( BFFID, 'VAX-GFLT' ) ) THEN
 
         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 16400, 0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 0,     0, OK )
 
      ELSE IF ( EQSTR ( BFFID, 'VAX-DFLT' ) ) THEN
 
         CALL CHCKSI ( 'IEQUIV(1)', IEQUIV(1), '=', 16512, 0, OK )
         CALL CHCKSI ( 'IEQUIV(2)', IEQUIV(2), '=', 0,     0, OK )
      ELSE
         CALL TSTMSG ( '#', 'This test does not support this format.' )
         CALL CHCKSI ( 'ERROR', 0, '=', 1, 0, OK )
      END IF
 
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
