C$Procedure F_DSKTOL ( DSKTOL tests )
 
      SUBROUTINE F_DSKTOL ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine DSKTOL.
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
      INCLUDE 'dsktol.inc'

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
C     This routine tests the SPICELIB DSK support routine DSKTOL.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 01-AUG-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C


C
C     Local Parameters
C
 
C 
C     Local Variables
C
      DOUBLE PRECISION      DPVAL
      DOUBLE PRECISION      NEWVAL
 
C
C     Saved values
C      
C     Initial values
C
 

C
C     Open the test family.
C
      CALL TOPEN ( 'F_DSKTOL' )
 

C***********************************************************************
C
C     DSKGTL tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: get expansion fraction' )
 
      CALL DSKGTL ( KEYXFR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'XFRACT', DPVAL, '=', XFRACT, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: get greedy segment parameter' )
 
      CALL DSKGTL ( KEYSGR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'SGREED', DPVAL, '=', SGREED, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: get segment pad margin' )
 
      CALL DSKGTL ( KEYSPM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'SGPADM', DPVAL, '=', SGPADM, 0.D0, OK )

  
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: get surface-point membership margin' )
 
      CALL DSKGTL ( KEYPTM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'PTMEMM', DPVAL, '=', PTMEMM, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: get angular rounding margin' )
 
      CALL DSKGTL ( KEYAMG, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'ANGMRG', DPVAL, '=', ANGMRG, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: get longitude alias margin' )
 
      CALL DSKGTL ( KEYLAL, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'LONALI', DPVAL, '=', LONALI, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKGTL test: keyword out of range' )

      CALL DSKGTL ( 0, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKGTL ( 7, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )
      



C***********************************************************************
C
C     DSKSTL tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: set expansion fraction' )
 
      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYXFR, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYXFR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'XFRACT', DPVAL, '=', NEWVAL, 0.D0, OK )

C
C     Restore default.
C
      CALL DSKSTL ( KEYXFR, XFRACT ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: set greedy parameter' )
 
      NEWVAL = 1.D-2

      CALL DSKSTL ( KEYSGR, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYSGR, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'SGREED', DPVAL, '=', NEWVAL, 0.D0, OK )

C
C     Restore default.
C
      CALL DSKSTL ( KEYSGR, SGREED ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: set pad margin' )
 
      NEWVAL = 1.D-1

      CALL DSKSTL ( KEYSPM, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYSPM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'SGPADM', DPVAL, '=', NEWVAL, 0.D0, OK )

C
C     Restore default.
C
      CALL DSKSTL ( KEYSPM, SGPADM ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: set plate membership margin' )
 
      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYPTM, NEWVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGTL ( KEYPTM, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'PTMEMM', DPVAL, '=', NEWVAL, 0.D0, OK )

C
C     Restore default.
C
      CALL DSKSTL ( KEYPTM, PTMEMM ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: try to set angular rounding margin' )

C
C     This parameter is not adjustable.
C
      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYAMG, NEWVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(IMMUTABLEVALUE)', OK )

C
C     Make sure no change occurred.
C
      CALL DSKGTL ( KEYAMG, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'ANGMRG', DPVAL, '=', ANGMRG, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: try to set longitude alias margin' )

C
C     This parameter is not adjustable.
C
      NEWVAL = 1.D-3

      CALL DSKSTL ( KEYLAL, NEWVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(IMMUTABLEVALUE)', OK )

C
C     Make sure no change occurred.
C
      CALL DSKGTL ( KEYLAL, DPVAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact equality, since we're comparing against 
C     a value declared in an include file common to this 
C     test family and the DSKTOL package.
C
      CALL CHCKSD ( 'LONALI', DPVAL, '=', LONALI, 0.D0, OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DSKSTL test: keyword out of range' )

      CALL DSKSTL ( 0, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKSTL ( 7, DPVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


