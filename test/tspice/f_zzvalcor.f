C$Procedure      F_ZZVALCOR ( Test ZZVALCOR )
  
      SUBROUTINE F_ZZVALCOR ( OK )

C$ Abstract
C
C     Test the aberration correction parsing routine ZZVALCOR.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'zzabcorr.inc'

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
C     This family tests the aberration correction parsing
C     utility routine ZZVALCOR.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 26-FEB-2009 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      LOGICAL               MATCHI

C     
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
      
      INTEGER               MAXCOR
      PARAMETER           ( MAXCOR = 9 )

C
C     Local Variables
C
      CHARACTER*(LNSIZE)    CORLST ( MAXCOR )
      CHARACTER*(LNSIZE)    QNAME

      INTEGER               I
      INTEGER               J

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               XATBLK ( ABATSZ )
   
C
C     Saved everything
C
      SAVE

C
C     Initial values
C
      DATA                 CORLST /  'None',
     .                               'lT',
     .                               'Xlt',
     .                               'lt + s',
     .                               'XLt + S',
     .                               'Cn',
     .                               'xCn',
     .                               'cN  +  s',
     .                               'xCN +  S'   /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZVALCOR' )




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Reject stellar aberration correction '
     .//           'without light time correction'         )       
 
      
      CALL ZZVALCOR ( 'S', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      
      CALL ZZVALCOR ( ' s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      
      CALL ZZVALCOR ( '+s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' + s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' + S', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


 
C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'Reject relativistic light time correction' )       


      CALL ZZVALCOR ( 'RLT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' rlT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( 'xRLT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' XrlT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( 'RLT + S', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( 'xRLT + s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



       
C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


      



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check attribute block for each normal ' 
     .//           'aberration correction flag.'            )
 
      
      DO I = 1, MAXCOR
C
C        Parse the correction.
C
         CALL ZZVALCOR ( CORLST(I), ATTBLK )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Set up the expected attribute block.
C
         DO J = 1, ABATSZ
            XATBLK(J) = .FALSE.
         END DO

C
C        Set the expected geometric element.
C         
         XATBLK( GEOIDX )  =  MATCHI( CORLST(I), '*NONE*', '*', '?' )

C
C        Set the expected transmission element.
C
         XATBLK( XMTIDX )  =  MATCHI( CORLST(I), '*X*', '*', '?' )

C
C        Set the expected stellar aberration element.
C
         XATBLK( STLIDX )  =  MATCHI( CORLST(I), '*+*S*', '*', '?' )

C
C        Set the expected relativistic correction element.
C
         XATBLK( RELIDX )  =  MATCHI( CORLST(I), '*RLT*', '*', '?' )

C
C        Set the expected light time and converted Newtonian
C        elements.
C
         IF (   MATCHI( CORLST(I), '*LT*', '*', '?' )   ) THEN
C
C           The light time element should be set; the converged
C           Newtonian element should not.
C
            XATBLK( LTIDX  ) = .TRUE.
            XATBLK( CNVIDX ) = .FALSE.

         ELSE IF (   MATCHI( CORLST(I), '*CN*', '*', '?' )   ) THEN

            XATBLK( LTIDX  ) = .TRUE.
            XATBLK( CNVIDX ) = .TRUE.
            
         ELSE

            XATBLK( LTIDX  ) = .FALSE.
            XATBLK( CNVIDX ) = .FALSE.
 
         END IF

 
C
C        Check each element of the attribute block.
C
         DO J = 1, ABATSZ

            QNAME = CORLST(I)
            CALL SUFFIX ( '/',             0, QNAME )
            CALL SUFFIX ( 'attblk elt. #', 1, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, ATTBLK(J), XATBLK(J), OK )
           

         END DO

      END DO

      RETURN
      END





