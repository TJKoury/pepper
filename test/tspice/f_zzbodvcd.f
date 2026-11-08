C$Procedure F_ZZBODVCD ( ZZBODVCD tests )
 
      SUBROUTINE F_ZZBODVCD ( OK )
 
C$ Abstract
C
C     Exercise the routine ZZBODVCD.
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
      
      INCLUDE 'zzctr.inc'
      
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
C     This routine tests the SPICELIB routine ZZBODVCD.
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
C-    TSPICE Version 1.0.0, 09-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters 
C
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzbodvcd.tpc' )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 10000 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     Local Variables 
C  
      CHARACTER*(NAMLEN)    ITEM
      CHARACTER*(LNSIZE)    TITLE
 
      DOUBLE PRECISION      DPBUFF ( BUFSIZ )
      DOUBLE PRECISION      XBUFF  ( BUFSIZ )

      INTEGER               BODYID
      INTEGER               N
      INTEGER               XN
      
      INTEGER               VARCTR ( CTRSIZ )
      INTEGER               XCTR   ( CTRSIZ )
 
      LOGICAL               UPDATE

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZBODVCD' )



C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Set-up: create PCK.' )

      IF ( EXISTS(PCK0) ) THEN
         
         CALL DELFIL ( PCK0 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL T_PCK08 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the user counters.
C
      CALL ZZCTRUIN ( VARCTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZCTRUIN ( XCTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Fetch moon radii.'

      CALL TCASE ( TITLE )


      ITEM   = 'RADII'
      BODYID = 399

C
C     Get expected values.
C
      CALL BODVCD ( BODYID, ITEM, 3, XN, XBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check counter.
C
      CALL ZZPCTRCK ( XCTR, UPDATE ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

C
C     Check data size and values.
C
      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, XN, 0.D0, OK )

 
C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Fetch moon radii a second time. Counter should '
     .//       'remain unchanged.'

      CALL TCASE ( TITLE )


      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check counter.
C
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

C
C     Check data size and values.
C
      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, XN, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Touch the pool; fetch moon radii a third time. '
     .//       'Counter should be updated.'

      CALL DVPOOL ( 'BODY599_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check counter.
C
      CALL ZZPCTRCK ( XCTR, UPDATE ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

C
C     Check counter.
C
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

C
C     Check data size and values.
C
      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, XN, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Verify that input values are returned if the '
     .//       'pool wasn''t touched.'

      CALL TCASE ( TITLE )


      CALL CLEARD ( BUFSIZ, DPBUFF )
      CALL CLEARD ( BUFSIZ, XBUFF  )
      N = 0

      CALL ZZBODVCD ( BODYID, ITEM, 3, VARCTR, N, DPBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check counter.
C
      CALL CHCKAI ( 'VARCTR', VARCTR, '=', XCTR, CTRSIZ, OK )

C
C     Check data size and values.
C
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

      CALL CHCKAD ( ITEM, DPBUFF, '=', XBUFF, BUFSIZ, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Clean up.' )

      CALL DELFIL ( PCK0 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


